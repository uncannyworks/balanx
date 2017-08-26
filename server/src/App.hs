{-# LANGUAGE
    DataKinds 
  , TypeOperators
#-}

module App
  ( runApp
  ) where

import Control.Monad.Log
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import System.IO (stdout)
import Text.PrettyPrint.Leijen.Text (Doc)
import Tisch

import Uncanny.Prelude

import Api.AuthApi
import Api.UserApi
import Session
import Types

type Routes = "auth" :> AuthApi 
         :<|> "user" :> UserApi

-- http://stackoverflow.com/a/31098944/70852
type RoutesAndStatic = Routes :<|> Raw

routes :: ServerT Routes AppM
routes = authServer :<|> userServer

app :: AppContext -> Wai.Application
app ctx =
  corsWithContentType $
    serveWithContext
    (Proxy :: Proxy RoutesAndStatic)
    ((cookieAuthCheck (appContextAuthSettings ctx) (appContextServerKey ctx)
       :: AuthHandler Wai.Request (Maybe Session))
     :. EmptyContext)
    (server ctx)
  where
    corsWithContentType :: Wai.Middleware
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
                 { corsRequestHeaders = ["Content-Type"]
                 , corsMethods = "PUT" : simpleMethods }

toHandler :: AppContext -> (AppM :~> ExceptT ServantErr IO)
toHandler ctx = Nat toHandler'
  where
    toHandler' :: AppM a -> ExceptT ServantErr IO a
    toHandler' =
      ExceptT . doLogging . runExceptT . flip runReaderT ctx . runAppM

doLogging :: LoggingT (WithSeverity Doc) IO (Either ServantErr a)
          -> IO (Either ServantErr a)
doLogging b =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \h ->
    runLoggingT b $ \m -> h . renderWithSeverity id $ m

server :: AppContext -> Server RoutesAndStatic
server ctx = enter (toHandler ctx) routes :<|> serveDirectory "./dist"

openConnection :: IO (Conn' Db1)
openConnection =
  connect PGS.defaultConnectInfo
    { PGS.connectHost     = "127.0.0.1"
    , PGS.connectUser     = "balanx"
    , PGS.connectPassword = "balanx"
    , PGS.connectDatabase = "balanx"
    }

runApp :: IO ()
runApp = do
  r <- mkRandomSource drgNew 2000
  k <- mkServerKey 16 (Just $ fromIntegral (86400 :: Integer))
  p <- P.createPool openConnection close 1 10 5
  let s = AuthCookieSettings
          { acsSessionField     = "Session"
          , acsCookieFlags      = ["HttpOnly"]
          , acsMaxAge           = fromIntegral (6 * 3600 :: Integer)
          , acsExpirationFormat = "%0Y%m%d%H%M%S"
          , acsPath             = "/"
          , acsHashAlgorithm    = Proxy :: Proxy SHA256
          , acsCipher           = Proxy :: Proxy AES256
          , acsEncryptAlgorithm = ctrCombine
          , acsDecryptAlgorithm = ctrCombine
          }
      ctx = AppContext s r k p
  Warp.run 8080 (app ctx)
