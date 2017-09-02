module Shared where

import Data.Tagged
import Data.Text as T (length)

import Uncanny.Prelude

data ValidationError
  = Invalid Text
  | TooLong Text Int
  | TooShort Text Int
  deriving (Eq, Show)

data QueryError
  = DoesNotExist
  | Exists Text Text
  | PasswordMismatch
  | TokenMismatch
  deriving (Eq, Show)

data AppError
  = ErrorQuery QueryError
  | ErrorValidation ValidationError
  deriving (Eq, Show)

data ValProps = ValProps Int Int Text -- min, max, tag

validateLength :: ValProps -> Maybe (Tagged s Text)
               -> Either ValidationError (Maybe (Tagged s Text))
validateLength (ValProps min max n) o@(Just i)
  | l <= min  = Left $ TooShort n l
  | l >= max  = Left $ TooLong  n l
  | otherwise = Right o
  where
    l = T.length . untag $ i
validateLength _ _ = Right Nothing
