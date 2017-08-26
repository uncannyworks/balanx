module Logger where

import Data.Text.Lazy (pack)
import Text.PrettyPrint.Leijen.Text (Doc, text)

import Uncanny.Prelude

toLog :: Show a => a -> Doc
toLog = text . pack . show
