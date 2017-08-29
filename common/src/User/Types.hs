{-# LANGUAGE
    CPP
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeFamilies
  , TypeSynonymInstances
#-}

module User.Types
  ( UserId
  , mkUserId
  , Email
  , mkEmail
  , Username
  , mkUsername
  , Permissions
  , mkPermissions
  , Token
  , mkToken
  , Password
  , mkPassword
  , PasswordE
  , mkPasswordE
  ) where

import Data.ByteString (ByteString)
#ifndef __GHCJS__
import Data.Text (pack)
import Data.Text.Read (decimal)
import Servant (FromHttpApiData, parseUrlPiece)
import Tisch
#endif

import Uncanny.Prelude

data UserIdTag
type UserId = Tagged UserIdTag Int32

mkUserId :: Int32 -> UserId
mkUserId = Tagged

#ifndef __GHCJS__
instance FromHttpApiData UserId where
  parseUrlPiece t =
    case decimal t of
      Right (v, _) -> Right . mkUserId . fromInteger $ v
      Left e       -> Left . pack $ e
instance PgTyped UserId where
  type PgType UserId = PGInt4
instance PgEq UserId
instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = qrcWrapped
#endif

data EmailTag
type Email = Tagged EmailTag Text

mkEmail :: Text -> Email
mkEmail = Tagged

#ifndef __GHCJS__
instance PgTyped Email where
  type PgType Email = PGText
instance PgEq Email
instance QueryRunnerColumnDefault PGText Email where
  queryRunnerColumnDefault = qrcWrapped
#endif

data UsernameTag
type Username = Tagged UsernameTag Text

mkUsername :: Text -> Username
mkUsername = Tagged

#ifndef __GHCJS__
instance FromHttpApiData Username where
  parseUrlPiece = Right . mkUsername
instance PgTyped Username where
  type PgType Username = PGText
instance PgEq Username
instance QueryRunnerColumnDefault PGText Username where
  queryRunnerColumnDefault = qrcWrapped
#endif

data PermissionsTag
type Permissions = Tagged PermissionsTag Int32

mkPermissions :: Int32 -> Permissions
mkPermissions = Tagged

#ifndef __GHCJS__
instance PgTyped Permissions where
  type PgType Permissions = PGInt4
instance PgEq Permissions
instance QueryRunnerColumnDefault PGInt4 Permissions where
  queryRunnerColumnDefault = qrcWrapped
#endif

data TokenTag
type Token = Tagged TokenTag Text

mkToken :: Text -> Token
mkToken = Tagged

#ifndef __GHCJS__
instance FromHttpApiData Token where
  parseUrlPiece = Right . mkToken
instance PgTyped Token where
  type PgType Token = PGText
instance PgEq Token
instance QueryRunnerColumnDefault PGText Token where
  queryRunnerColumnDefault = qrcWrapped
#endif

data PasswordTag
type Password = Tagged PasswordTag Text

mkPassword :: Text -> Password
mkPassword = Tagged

data PasswordETag
type PasswordE = Tagged PasswordETag ByteString

mkPasswordE :: ByteString -> PasswordE
mkPasswordE = Tagged

#ifndef __GHCJS__
instance PgTyped PasswordE where
  type PgType PasswordE = PGBytea
instance PgEq PasswordE
instance QueryRunnerColumnDefault PGBytea PasswordE where
  queryRunnerColumnDefault = qrcWrapped
#endif

data UserPermission
  = NotSet
  | Disabled
  | Member
  | Admin
  deriving (Eq, Ord)

instance Enum UserPermission where
  toEnum 1 = NotSet
  toEnum 2 = Disabled
  toEnum 4 = Member
  toEnum 8 = Admin
  toEnum _ = NotSet
  fromEnum NotSet   = 1
  fromEnum Disabled = 2
  fromEnum Member   = 4
  fromEnum Admin    = 8

instance Bounded UserPermission where
  minBound = NotSet
  maxBound = Admin
