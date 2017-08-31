{-# LANGUAGE
    Arrows
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , OverloadedLabels
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}

module Database.User where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Scrypt
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Tisch

import Common.Types
import Shared
import Uncanny.Prelude
import User.Json
import User.Types
import Types

data TUser
data instance Table TUser      = TUser
type instance Database TUser   = Db1
type instance SchemaName TUser = "public"
type instance TableName TUser  = "users"
type instance Columns TUser =
  [ 'Column "user_id"     'WD 'R  PGInt4        UserId
  , 'Column "username"    'W  'R  PGText        Username
  , 'Column "password"    'W  'R  PGBytea       PasswordE
  , 'Column "email"       'W  'R  PGText        Email
  , 'Column "permissions" 'WD 'R  PGInt4        Permissions
  , 'Column "verify_t"    'W  'R  PGText        Token
  , 'Column "verified"    'WD 'R  PGBool        Bool
  , 'Column "created"     'WD 'R  PGTimestamptz Created
  , 'Column "updated"     'WD 'R  PGTimestamptz Updated
  ]

hsRToUserR :: HsR TUser -> UserR
hsRToUserR u = UserR
  (view #user_id u)
  (view #username u)
  (view #password u)
  (view #email u)
  (view #permissions u)
  (view #verified u)
  (view #created u)
  (view #updated u)

userWToHsI :: PasswordE -> Token -> UserW -> HsI TUser
userWToHsI p t u =
  mkHsI TUser
    (hsi #user_id     WDef)
    (hsi #username    (u ^. uwUsername))
    (hsi #password    p)
    (hsi #email       (u ^. uwEmail))
    (hsi #permissions WDef)
    (hsi #verify_t    t)
    (hsi #verified    WDef)
    (hsi #created     WDef)
    (hsi #updated     WDef)

encryptPassword :: MonadIO m => Password -> m PasswordE
encryptPassword p = do
  e <- liftIO $ encryptPassIO defaultParams (Pass . encodeUtf8 . untag $ p)
  return . mkPasswordE . getEncryptedPass $ e

verifyPassword :: Password -> PasswordE -> Bool
verifyPassword p e =
  let p' = Pass . encodeUtf8 . untag $ p
      e' = EncryptedPass . untag $ e
  in fst $ verifyPass defaultParams p' e'

userUPassword :: MonadIO m => UserU -> m (Maybe PasswordE)
userUPassword u
  | chk1 && chk2 = do
      p <- encryptPassword (fromJust (u ^. uuPassword1))
      return . Just $ p
  | otherwise = return Nothing
  where
    chk1 = isJust (u ^. uuPassword1) && isJust (u ^. uuPassword2)
    chk2 = (u ^. uuPassword1) == (u ^. uuPassword2)

userWPassword :: MonadIO m => UserW -> m (Maybe PasswordE)
userWPassword u
  | (u ^. uwPassword0) == (u ^. uwPassword1) = do
      p <- encryptPassword (u ^. uwPassword0)
      return . Just $ p
  | otherwise = return Nothing

generateToken :: MonadIO m => m Token
generateToken = liftIO nextRandom >>= return . mkToken . toText

userQueryById :: UserId -> Query Db1 () (PgR TUser)
userQueryById uid = proc () -> do
  u <- query TUser -< ()
  restrict -< eq (#user_id u) (kol uid)
  returnA -< u

userQueryByUsername :: Username -> Query Db1 () (PgR TUser)
userQueryByUsername usn = proc () -> do
  u <- query TUser -< ()
  restrict -< eq (#username u) (kol usn)
  returnA -< u

userQueryByUsernameAndToken :: Username -> Token -> Query Db1 () (PgR TUser)
userQueryByUsernameAndToken usn tkn = proc () -> do
  u <- query TUser -< ()
  restrict -< eq (#username u) (kol usn)
  restrict -< eq (#verify_t u) (kol tkn)
  returnA -< u

userAuth :: (MonadIO m, MonadThrow m) =>
            UserL -> Conn' Db1 -> m (Either QueryError (HsR TUser))
userAuth u conn = do
  chk <- runQuery1 conn (userQueryByUsername (u ^. ulUsername))
  case chk of
    Nothing -> return . Left $ DoesNotExist
    Just usr ->
      if verifyPassword (u ^. ulPassword) (view #password usr)
      then return . Right $ usr
      else return . Left $ DoesNotExist

userFetch :: (MonadIO m, MonadThrow m) =>
             UserId -> Conn' Db1 -> m (Maybe (HsR TUser))
userFetch uid conn = runQuery1 conn (userQueryById uid)

userUpdate :: (MonadIO m, MonadThrow m) =>
              UserU -> Conn' Db1 -> m (Either QueryError ())
userUpdate u conn = do
  chk <- runQuery1 conn (userQueryById (u ^. uuUserId))
  case (chk :: Maybe (HsR TUser)) of
    Nothing -> return . Left $ DoesNotExist
    Just usr ->
      if verifyPassword (u ^. uuPassword0) (view #password usr)
      then do
        pw <- userUPassword u
        runUpdate conn TUser (upd pw) fid >> return (Right ())
      else return . Left $ PasswordMismatch
  where
    upd pw = updUsername (u ^. uuUsername) .
             updPassword pw .
             updEmail (u ^. uuEmail)

    fid :: PgR TUser -> Kol PGBool
    fid uu = eq (kol $ u ^. uuUserId) (#user_id uu)

    updUsername :: Maybe Username -> PgW TUser -> PgW TUser
    updUsername (Just usn) = set #username (kol usn)
    updUsername _ = id

    updPassword :: Maybe PasswordE -> PgW TUser -> PgW TUser
    updPassword (Just pw) = set #password (kol pw)
    updPassword _ = id

    updEmail :: Maybe Email -> PgW TUser -> PgW TUser
    updEmail (Just em) = set #email (kol em)
    updEmail _ = id

userInsert :: (MonadIO m, MonadThrow m) =>
              UserW -> Conn' Db1 -> m (Either QueryError (HsR TUser))
userInsert u conn = do
  chk <- runQuery1 conn (userQueryByUsername (u ^. uwUsername))
  case (chk :: Maybe (HsR TUser)) of
    Just _ -> return . Left . Exists "username" $ untag (u ^. uwUsername)
    Nothing ->
      userWPassword u >>= \p ->
        case p of
          Nothing -> return . Left $ PasswordMismatch
          Just pw -> do
            tn <- generateToken
            rt <- runInsertReturning1 conn TUser id (userWToHsI pw tn u)
            return . Right $ rt

userVerify :: (MonadIO m, MonadThrow m) =>
              Username -> Token -> Conn' Db1 -> m (Either QueryError ())
userVerify usn tkn conn = do
  chk <- runQuery1 conn (userQueryByUsernameAndToken usn tkn)
  case (chk :: Maybe (HsR TUser)) of
    Nothing -> return . Left $ DoesNotExist
    Just usr ->
      if view #verify_t usr == tkn
      then runUpdate conn TUser upd (fid $ view #user_id usr) >> return (Right ())
      else return . Left $ TokenMismatch
  where
    fid :: UserId -> PgR TUser -> Kol PGBool
    fid uid uu = eq (kol uid) (#user_id uu)

    upd :: PgW TUser -> PgW TUser
    upd = set #verified (WVal $ kol True)
