module Extras where

import Data.Text (Text)
import Reflex.Dom

import Uncanny.Prelude

style :: DomBuilder t m => Text -> m ()
style = el "style" . text

styleSheet :: DomBuilder t m => Text -> m ()
styleSheet link = elAttr "link" ss $ pure ()
    where ss = mconcat [ "rel"  =: "stylesheet"
                       , "type" =: "text/css"
                       , "href" =: link ]
