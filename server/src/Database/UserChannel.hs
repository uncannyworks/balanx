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

module Database.UserChannel where

import Control.Arrow
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Tisch

import Common.Types
import Shared
import Types
import Uncanny.Prelude
import UserChannel.Types

data TUserChannel
data instance Table TUserChannel      = TUserChannel
type instance Database TUserChannel   = Db1
type instance SchemaName TUserChannel = "public"
type instance TableName TUserChannel  = "user_channels"
type instance Columns TUserChannel    =
  [ 'Column "user_id"     'W  'R  PGInt4 UserId
  , 'Column "channel_id"  'W  'R  PGInt4 ChannelId
  , 'Column "permissions" 'WD 'R  PGInt4 Permissions
  ]
