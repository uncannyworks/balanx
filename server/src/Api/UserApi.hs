{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , TypeOperators
  , TypeSynonymInstances
#-}

module Api.UserApi where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.ByteString.Lazy.Char8 (pack)
import Servant
import Servant.Server.Experimental.Auth.Cookie (addSession)

import Uncanny.Prelude
import User.Json
import User.Types

import Database.User
import Session
import Types

type CookieResp = Cookied (Maybe UserR)

type UserGet    = AppAuth :> Capture "id" UserId :> Get '[JSON] (Maybe UserR)
type UserInsert = ReqBody '[JSON] UserW :> Post '[JSON] CookieResp
type UserUpdate = AppAuth :> ReqBody '[JSON] UserU :> Put '[JSON] ()
type UserVerify = Capture "username" Username :> Capture "verify_t" Token :>
                  Put '[JSON] ()

type UserApi = UserGet
          :<|> UserInsert
          :<|> UserUpdate
          :<|> UserVerify

userApi :: Proxy UserApi
userApi = Proxy

userServer :: ServerT UserApi AppM
userServer = getUser :<|> insertUser :<|> updateUser :<|> verifyUser

getUser :: Maybe Session -> UserId -> AppM (Maybe UserR)
getUser ms uid =
  withSession ms $ \_ ->
    getConn >>= userFetch uid >>= mapM (return . hsRToUserR)

insertUser :: UserW -> AppM CookieResp
insertUser u = do
  r <- userInsert u =<< getConn
  case r of
    Left e -> throwError err400 { errBody = pack . show $ e }
    Right usr -> do
      let s = Session (hsRToUserR usr ^. urUserId)
      ctx <- ask
      addSession (appContextAuthSettings ctx) (appContextRandomSource ctx)
        (appContextServerKey ctx) s (Just . hsRToUserR $ usr) 

updateUser :: Maybe Session -> UserU -> AppM ()
updateUser ms u =
  withSession ms $ \_ -> do
    r <- userUpdate u =<< getConn
    case r of
      Left e  -> throwError err400 { errBody = pack . show $ e }
      Right _ -> return ()

verifyUser :: Username -> Token -> AppM ()
verifyUser usn tkn = do
  r <- userVerify usn tkn =<< getConn
  case r of
    Left e  -> throwError err400 { errBody = pack . show $ e }
    Right _ -> return ()
