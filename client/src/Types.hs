module Types where

import Uncanny.Prelude

data View
  = VHome
  | VLogin
  | VRegister
  | VLogout
  deriving (Eq, Show)
