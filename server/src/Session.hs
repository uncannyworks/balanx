module Session where

import Data.Serialize

import Uncanny.Prelude hiding (get, put)

import User.Types (UserId, mkUserId)

data Session
  = Session
  { sessionUserId :: UserId
  }

instance Serialize Session where
  put (Session userId) = put . untag $ userId
  get = Session . mkUserId <$> get
