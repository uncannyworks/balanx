{-# LANGUAGE
    Arrows
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , OverloadedLabels
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}

module Database.Channel where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Tisch

import Channel.Types
import Common.Types
import Shared
import Types
import Uncanny.Prelude

data TChannel
data instance Table TChannel      = TChannel
type instance Database TChannel   = Db1
type instance SchemaName TChannel = "public"
type instance TableName TChannel  = "channels"
type instance Columns TChannel    =
  [ 'Column "channel_id" 'W  'R  PGInt4        ChannelId
  , 'Column "name"       'W  'R  PGText        Name
  , 'Column "title"      'W  'R  PGText        Title
  , 'Column "access"     'WD 'R  PGInt4        Access
  , 'Column "kind"       'WD 'R  PGInt4        Kind
  , 'Column "creator_id" 'W  'R  PGInt4        CreatorId
  , 'Column "created"    'WD 'R  PGTimestamptz Created
  , 'Column "updated"    'WD 'R  PGTimestamptz Updated
  ]
