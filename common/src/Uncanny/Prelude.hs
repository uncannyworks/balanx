{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Uncanny.Prelude
  ( module X
  ) where

import qualified Data.Text as Text
import qualified Prelude

import Prelude as X
  ( Bounded(..)
  , Enum(..)
  , iterate
  , maxBound
  )

import Control.Applicative as X
  ( Applicative(..)
  , (<$)
  , (<$>)
  , (<*)
  , (*>)
  , pure
  )

import Control.Monad as X
  ( Functor
  , Monad
  , forever
  , fmap
  , liftM
  , return
  , when
  , (>>=)
  , (=<<) 
  , (>>)
  )

import Control.Monad.State as X
  ( State
  , get
  , put
  , runState
  )

import Control.Exception.Base as X
  ( SomeException(..)
  , handle
  )

import Control.Concurrent as X

import Debug.Trace as X
  ( trace
  , traceIO
  , traceShow
  )

import Safe as X
  ( headMay
  , abort
  )

import Data.Eq as X
import Data.Functor as X
import Data.Ord as X
import Data.Monoid as X
import Data.Traversable as X
import Data.Foldable as X hiding
  ( foldr1
  , foldl1
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  )

import Data.Int as X
import Data.Bits as X
import Data.Word as X
import Data.Bool as X hiding (bool)
import Data.Char as X (Char)
import Data.Maybe as X hiding (fromJust)
import Data.Either as X

import Data.Function as X
  ( id
  , const
  , (.)
  , ($)
  , flip
  , fix
  , on
  )

import Data.Tuple as X
import Data.List as X
  ( filter
  , intersect
  , iterate
  , reverse
  , take
  , takeWhile
  , (\\)
  , (++)
  )

import Data.Text as X (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.IO
import Data.String.Conv as X
  ( toS
  )

import Data.Tagged as X

import GHC.IO as X (IO)
import GHC.Num as X
import GHC.Real as X
import GHC.Float as X
import GHC.Generics as X
import GHC.Show as X
