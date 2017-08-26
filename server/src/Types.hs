{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeOperators
#-}

module Types where

import Control.Monad.Catch (MonadThrow, try)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Log (LoggingT, MonadLog, WithSeverity(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Pool (Pool, withResource)
import Network.Wai (Request)
import Servant (AuthProtect, Headers, Header, ServantErr, err403, throwError)
import Servant.Server.Experimental.Auth (AuthHandler(..), mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie
  ( AuthCookieData
  , AuthCookieException
  , AuthCookieSettings
  , EncryptedSession
  , RandomSource
  , ServerKey
  , getSession
  )
import Text.PrettyPrint.Leijen.Text (Doc)
import Tisch (Conn')

import Uncanny.Prelude
import Session

data Db1

data AppContext
  = AppContext
  { appContextAuthSettings :: AuthCookieSettings
  , appContextRandomSource :: RandomSource
  , appContextServerKey    :: ServerKey
  , appContextDBPool       :: Pool (Conn' Db1)
  }

newtype AppM a
  = AppM
  { runAppM :: ReaderT AppContext (ExceptT ServantErr (LoggingT (WithSeverity Doc) IO)) a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadThrow, MonadError ServantErr
           , MonadReader AppContext, MonadLog (WithSeverity Doc))

type instance AuthCookieData = Maybe Session
type AppAuth = AuthProtect "cookie-auth"

type Cookied a = Headers '[Header "Set-Cookie" EncryptedSession] a

getConnFromPool :: AppContext -> AppM (Conn' Db1)
getConnFromPool cxt = liftIO $ withResource (appContextDBPool cxt) pure

getConn :: AppM (Conn' Db1)
getConn = ask >>= getConnFromPool

cookieAuthCheck :: AuthCookieSettings ->
                   ServerKey ->
                   AuthHandler Request (Maybe Session)
cookieAuthCheck auth key =
  mkAuthHandler $ \req -> do
    res <- try (getSession auth key req)
    case res :: Either AuthCookieException (Maybe Session) of
      Left _  -> pure Nothing
      Right s -> pure s

withSession :: Maybe Session -> (Session -> AppM a) -> AppM a
withSession ms a = maybe (throwError err403) a ms
