{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , TypeOperators
  , TypeSynonymInstances
#-}

module Api.AuthApi where

import Control.Lens
import Control.Monad.Log
import Control.Monad.Reader (ask)
import Data.ByteString
import Servant
import Servant.Server.Experimental.Auth.Cookie (addSession)

import Uncanny.Prelude
import User.Json

import Database.User
import Logger
import Session
import Types

type CookieResp = Cookied (Maybe UserR)

type AuthLogin  = "login" :> ReqBody '[JSON] UserL :> Post '[JSON] CookieResp
type AuthLogout = "logout" :> AppAuth :> GetNoContent '[JSON] CookieResp
type AuthCheck  = "check" :> AppAuth :> Get '[JSON] (Maybe UserR)

type AuthApi = AuthLogin 
          :<|> AuthLogout
          :<|> AuthCheck

authApi :: Proxy AuthApi
authApi = Proxy

authServer :: ServerT AuthApi AppM
authServer = authLogin :<|> authLogout :<|> authCheck

authLogin :: UserL -> AppM CookieResp
authLogin u = do
  logMessage (WithSeverity Informational (toLog u))
  r <- userAuth u =<< getConn
  case r of
    Left _ -> throwError err401
    Right usr -> do
      let s = Session (hsRToUserR usr ^. urUserId)
      ctx <- ask
      addSession (appContextAuthSettings ctx) (appContextRandomSource ctx)
        (appContextServerKey ctx) s (Just . hsRToUserR $ usr)

authLogout :: Maybe Session -> AppM CookieResp
authLogout ms =
  withSession ms $ \_ -> do
    ctx <- ask
    addSession (appContextAuthSettings ctx) (appContextRandomSource ctx)
      (appContextServerKey ctx) () Nothing

authCheck :: Maybe Session -> AppM (Maybe UserR)
authCheck ms =
  withSession ms $ \(Session uid) -> do
  r <- userFetch uid =<< getConn
  case r of
    Nothing  -> return Nothing
    Just usr -> return . Just . hsRToUserR $ usr

