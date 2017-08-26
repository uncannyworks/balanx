{-# LANGUAGE
    LambdaCase
  , RecursiveDo
  , ScopedTypeVariables
#-}

module View.Nav where

import Control.Lens
import qualified Data.Map as Map
import Reflex 
import Reflex.Dom

import Uncanny.Prelude
import User.Json
import User.Types

import Types

navWidget :: MonadWidget t m => Dynamic t View -> Dynamic t (Maybe UserR) -> m (Event t View)
navWidget v u =
  elClass "header" "mdc-toolbar" $
    elClass "div" "mdc-toolbar__row" $ do
      elClass "section" "mdc-toolbar__section mdc-toolbar__section--align-start" $
        elClass "span" "mdc-toolbar__title" $ text "Balanx"
      elAttr "section" ( "class" =: "mdc-toolbar__section mdc-toolbar__section--align-end" <>
                         "role"  =: "toolbar" ) $
        elClass "nav" "mdc-tab-bar" $ do
          i <- navWidgetIn v u
          o <- navWidgetOut v u
          return . leftmost $ [ i, o ]

navWidgetActive :: View -> View -> Map.Map Text Text -> Map.Map Text Text
navWidgetActive t v a =
  if t == v
  then Map.adjust (const "mdc-tab mdc-tab--active") "class" a
  else a

navWidgetIn :: MonadWidget t m => Dynamic t View -> Dynamic t (Maybe UserR) -> m (Event t View)
navWidgetIn v u = do
  let
    attrs = ("style" =: "display: none;" <> "class" =: "mdc-tab")
    dd = ffor u $ \case
           Nothing -> Map.adjust (const "display: none;") "style" attrs
           Just _  -> Map.adjust (const "") "style" attrs
    dv t a = ffor v $ \v' -> navWidgetActive t v' a
  (b0, _) <- elDynAttr' "a" (dd >>= dv VLogout) $ text "Logout"

  let user = ffor u $ \case
               Nothing -> blank
               Just u' ->
                 elClass "span" "mdc-tab" $
                   text $ "Welcome, " <> (untag $ u' ^. urUsername)
  dyn user

  let req = XhrRequest "GET" "/auth/logout" def
  rsp <- performRequestAsync $ req <$ domEvent Click b0

  return $ leftmost [ VLogout <$ rsp
                    ]

navWidgetOut :: MonadWidget t m => Dynamic t View -> Dynamic t (Maybe UserR) -> m (Event t View)
navWidgetOut v u = do
  let
    attrs = ("style" =: "display: none;" <> "class" =: "mdc-tab")
    dd = ffor u $ \case
           Nothing -> Map.adjust (const "") "style" attrs
           Just _  -> Map.adjust (const "display: none;") "style" attrs
    dv t a = ffor v $ \v' -> navWidgetActive t v' a
  (b0, _) <- elDynAttr' "a" (dd >>= dv VHome)     $ text "Home"
  (b1, _) <- elDynAttr' "a" (dd >>= dv VRegister) $ text "Register"
  (b2, _) <- elDynAttr' "a" (dd >>= dv VLogin)    $ text "Login"
  return $ leftmost [ VHome     <$ domEvent Click b0
                    , VRegister <$ domEvent Click b1
                    , VLogin    <$ domEvent Click b2
                    ]

loginWidget :: MonadWidget t m => Dynamic t View -> m (Event t (Maybe UserR))
loginWidget v = do
  let dynAttr = ffor v $ \case
                  VLogin -> "style" =: ""
                  _      -> "style" =: "display: none;"
  elDynAttr "main" dynAttr $
    elClass "div" "container" $ do
      el "h1" $ text "Login"
      el "div" $ do
        u <- usernameWidget "username" Nothing
        p <- passwordWidget "password" Nothing
        b <- button "Login"
        let uv = fmap mkUsername $ u ^. textInput_value 
            pv = fmap mkPassword $ p ^. textInput_value 
            ue = tagPromptlyDyn (UserL <$> uv <*> pv) $
              leftmost [ b
                       , keypress Enter u
                       , keypress Enter p
                       ]
            req = postJson "/auth/login"
        rsp <- performRequestAsync $ req <$> ue
        return $ fmap decodeXhrResponse rsp

registerWidget :: MonadWidget t m => Dynamic t View -> m (Event t (Maybe UserR))
registerWidget v = do
  let dynAttr = ffor v $ \case
                  VRegister -> "style" =: ""
                  _         -> "style" =: "display: none;"
  elDynAttr "main" dynAttr $
    elClass "div" "container" $ do
      el "h1" $ text "Register"
      el "div" $ do
        u  <- usernameWidget "username"  (Just "Username: ")
        e  <- emailWidget    "email"     "Email: "
        p0 <- passwordWidget "password0" (Just "Password: ")
        p1 <- passwordWidget "password1" (Just "Verify: ")
        b  <- button "Register"
        let uv  = fmap mkUsername $ u ^. textInput_value 
            ev  = fmap mkEmail $ e ^. textInput_value 
            pv0 = fmap mkPassword $ p0 ^. textInput_value 
            pv1 = fmap mkPassword $ p1 ^. textInput_value 
            ue = tagPromptlyDyn (UserW <$> uv <*> pv0 <*> pv1 <*> ev) $
              leftmost [ b
                       , keypress Enter u
                       , keypress Enter e
                       , keypress Enter p0
                       , keypress Enter p1
                       ]
            req = postJson "/user"
        rsp <- performRequestAsync $ req <$> ue
        return $ fmap decodeXhrResponse rsp

usernameWidget :: MonadWidget t m => Text -> Maybe Text -> m (TextInput t)
usernameWidget f t = do
  case t of
    Nothing -> return ()
    Just t' -> elAttr "label" ("for" =: f) $ text t'
  elAttr "span" ("id" =: f) $ textInput def

passwordWidget :: MonadWidget t m => Text -> Maybe Text -> m (TextInput t)
passwordWidget f t = do
  case t of
    Nothing -> return ()
    Just t' -> elAttr "label" ("for" =: f) $ text t'
  elAttr "span" ("id" =: f) $
    textInput $ def & textInputConfig_inputType .~ "password"

emailWidget :: MonadWidget t m => Text -> Text -> m (TextInput t)
emailWidget f t = do
  elAttr "label" ("for" =: f) $ text t
  elAttr "span" ("id" =: f) $ textInput def
