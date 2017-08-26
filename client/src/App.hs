{-# LANGUAGE
    LambdaCase
  , RecursiveDo
  , ScopedTypeVariables
#-}

module App where

import Data.Maybe (isJust)
import Reflex 
import Reflex.Dom

import Uncanny.Prelude hiding (head)
import User.Json
import User.Types

import Extras
import Types
import View.Nav

runApp :: IO ()
runApp = mainWidgetWithHead head body

head :: DomBuilder t m => m ()
head = do
  style ":root { --mdc-theme-primary: #455a64; }"
  styleSheet "css/app.css"
  styleSheet "node_modules/material-components-web/dist/material-components-web.css"

body :: MonadWidget t m => m ()
body = do
  rec
    view <- foldDyn (\e _ -> e) VHome $
      leftmost [ fmap (\e -> if isJust e then VHome else VLogin) ue
               , fmap (\e -> if isJust e then VHome else VRegister) re
               , fmap (\e -> if e == VLogout then VHome else e) ne
               ]
    user <- foldDyn (\e _ -> e) Nothing $
      leftmost [ ue
               , re
               , attachWith (\u e -> if e == VLogout then Nothing else u) (current user) ne
               , ce ]
    ne <- navWidget view user
    homeWidget view
    ue <- loginWidget view
    re <- registerWidget view

    -- check auth on page load
    pb <- getPostBuild
    let req = XhrRequest "GET" "/auth/check" def
    rsp <- performRequestAsync $ req <$ pb
    let ce = fmapMaybe decodeXhrResponse rsp
  return ()

homeWidget :: MonadWidget t m => Dynamic t View -> m ()
homeWidget v = do
  let dynAttr = ffor v $ \case
                  VHome -> "style" =: ""
                  _     -> "style" =: "display: none;"
  elDynAttr "main" dynAttr $ blank
