module Shared where

import Uncanny.Prelude

data ValidationError
  = Invalid Text
  | TooLong Text Int
  | TooShort Text Int
  deriving (Show)

data AppError
  = ErrorQuery QueryError
  deriving (Eq, Show)

data QueryError
  = DoesNotExist
  | Exists Text Text
  | PasswordMismatch
  | TokenMismatch
  deriving (Eq, Show)
