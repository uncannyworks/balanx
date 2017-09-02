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

module Database.Message where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Tisch

import Common.Types
import Message.Types
import Shared
import Types
import Uncanny.Prelude

data TMessage
data instance Table TMessage      = TMessage
type instance Database TMessage   = Db1
type instance SchemaName TMessage = "public"
type instance TableName TMessage  = "messages"
type instance Columns TMessage    =
  [ 'Column "message_id" 'W  'R  PGInt4        ChannelId
  , 'Column "parent_id"  'W  'R  PGText        ParentId
  , 'Column "channel_id" 'W  'R  PGText        ChannelId
  , 'Column "creator_id" 'W  'R  PGInt4        CreatorId
  , 'Column "title"      'W  'R  PGText        Title
  , 'Column "message"    'W  'R  PGText        Message
  , 'Column "kind"       'WD 'R  PGInt4        Kind
  , 'Column "created"    'WD 'R  PGTimestamptz Created
  , 'Column "updated"    'WD 'R  PGTimestamptz Updated
  ]
