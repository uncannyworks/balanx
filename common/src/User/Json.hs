{-# LANGUAGE TemplateHaskell #-}

module User.Json where

import Control.Lens hiding ((.=))
import Control.Monad (fail, mzero)
import Data.Aeson
import Data.Maybe
import Data.Text as T (length)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Text.Email.Validate

import Common.Types
import Shared
import Uncanny.Prelude
import User.Types

validateUsername :: Maybe Username -> Either ValidationError (Maybe Username)
validateUsername u@(Just v)
  | l < 3     = Left $ TooShort "username" l
  | l > 30    = Left $ TooLong "username" l
  | otherwise = Right u
  where
    l = T.length . untag $ v
validateUsername _ = Right Nothing

validatePassword :: Maybe Password -> Either ValidationError (Maybe Password)
validatePassword p@(Just v)
  | l < 8     = Left $ TooShort "password" l
  | l > 256   = Left $ TooLong "password" l
  | otherwise = Right p
  where
    l = T.length . untag $ v
validatePassword _ = Right Nothing

validateEmail :: Maybe Email -> Either ValidationError (Maybe Email)
validateEmail e@(Just v)
  | check v   = Right e
  | otherwise = Left $ Invalid "email"
  where
    check = isValid . encodeUtf8 . untag
validateEmail _ = Right Nothing

data UserR
  = UserR
  { _urUserId      :: UserId
  , _urUsername    :: Username
  , _urPassword    :: PasswordE
  , _urEmail       :: Email
  , _urPermissions :: Permissions
  , _urVerified    :: Bool
  , _urCreated     :: Created
  , _urUpdated     :: Updated
  } deriving (Show, Eq)

makeLenses ''UserR

instance ToJSON UserR where
  toJSON u =
    object [ "user_id"     .= (u ^. urUserId)
           , "username"    .= (u ^. urUsername)
           , "email"       .= (u ^. urEmail)
           , "permissions" .= (u ^. urPermissions)
           , "verified"    .= (u ^. urVerified)
           , "created"     .= (u ^. urCreated)
           , "updated"     .= (u ^. urUpdated)
           ]

instance FromJSON UserR where
  parseJSON (Object v) = UserR
    <$> v .: "user_id"
    <*> v .: "username"
    <*> (pure . mkPasswordE $ "")
    <*> v .: "email"
    <*> v .: "permissions"
    <*> v .: "verified"
    <*> v .: "created"
    <*> v .: "updated"
  parseJSON _ = mzero

data UserW
  = UserW
  { _uwUsername  :: Username
  , _uwPassword0 :: Password
  , _uwPassword1 :: Password
  , _uwEmail     :: Email
  } deriving (Show, Eq)

makeLenses ''UserW

instance ToJSON UserW where
  toJSON u =
    object [ "username"  .= (u ^. uwUsername)
           , "password0" .= (u ^. uwPassword0)
           , "password1" .= (u ^. uwPassword1)
           , "email"     .= (u ^. uwEmail)
           ]


instance FromJSON UserW where
  parseJSON (Object v) = UserW
    <$> (v .: "username"  >>= vUsn)
    <*> (v .: "password0" >>= vPwd)
    <*> (v .: "password1" >>= vPwd)
    <*> (v .: "email"     >>= vEml)
    where
      val  = either (fail . show) (pure . fromJust)
      vUsn = val . validateUsername
      vPwd = val . validatePassword
      vEml = val . validateEmail
  parseJSON _ = mzero

data UserU
  = UserU
  { _uuUserId    :: UserId
  , _uuUsername  :: Maybe Username
  , _uuPassword0 :: Password
  , _uuPassword1 :: Maybe Password
  , _uuPassword2 :: Maybe Password
  , _uuEmail     :: Maybe Email
  } deriving (Show, Eq)

makeLenses ''UserU

instance FromJSON UserU where
  parseJSON (Object v) = UserU
    <$>  v .:  "user_id"
    <*> (v .:? "username"  >>= vUsn)
    <*> (v .:  "password0" >>= vPwd0)
    <*> (v .:? "password1" >>= vPwd1)
    <*> (v .:? "password2" >>= vPwd1)
    <*> (v .:? "email"     >>= vEml)
    where
      val   = either (fail . show) pure
      vUsn  = val . validateUsername
      vPwd0 = either (fail . show) (pure . fromJust) . validatePassword
      vPwd1 = val . validatePassword
      vEml  = val . validateEmail
  parseJSON _ = mzero

data UserL
  = UserL
  { _ulUsername :: Username
  , _ulPassword :: Password
  } deriving (Show, Eq)

makeLenses ''UserL

instance ToJSON UserL where
  toJSON u =
    object [ "username" .= (u ^. ulUsername)
           , "password" .= (u ^. ulPassword)
           ]

instance FromJSON UserL where
  parseJSON (Object v) = UserL
    <$> v .: "username"
    <*> v .: "password"
  parseJSON _ = mzero
