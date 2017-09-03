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
import Data.Maybe (fromMaybe)
import Tisch

import Common.Types
import Message.Json
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
  [ 'Column "message_id" 'WD 'R  PGInt4        MessageId
  , 'Column "parent_id"  'W  'RN PGInt4        ParentId
  , 'Column "channel_id" 'W  'R  PGInt4        ChannelId
  , 'Column "creator_id" 'W  'R  PGInt4        CreatorId
  , 'Column "title"      'W  'RN PGText        Title
  , 'Column "message"    'W  'R  PGText        Message
  , 'Column "kind"       'W  'R  PGInt4        Kind
  , 'Column "created"    'WD 'R  PGTimestamptz Created
  , 'Column "updated"    'WD 'R  PGTimestamptz Updated
  ]

hsRToMessageR :: HsR TMessage -> MessageR
hsRToMessageR m = MessageR
  (view #message_id m)
  (view #parent_id m)
  (view #channel_id m)
  (view #creator_id m)
  (view #title m)
  (view #message m)
  (view #kind m)
  (view #created m)
  (view #updated m)

messageWToHsI :: CreatorId -> Kind -> MessageW -> HsI TMessage
messageWToHsI i k m =
  mkHsI TMessage
    (hsi #message_id WDef)
    (hsi #parent_id  (m ^. mwParentId))
    (hsi #channel_id (m ^. mwChannelId))
    (hsi #creator_id i)
    (hsi #title      (m ^. mwTitle))
    (hsi #message    (m ^. mwMessage))
    (hsi #kind       k)
    (hsi #created    WDef)
    (hsi #updated    WDef)

messageQueryById :: MessageId -> Query Db1 () (PgR TMessage)
messageQueryById i = proc () -> do
  m <- query TMessage -< ()
  restrict -< eq (#message_id m) (kol i)
  returnA -< m

messageQueryByChannelId :: ChannelId -> Query Db1 () (PgR TMessage)
messageQueryByChannelId i = proc () -> do
  m <- query TMessage -< ()
  restrict -< eq (#channel_id m) (kol i)
  returnA -< m

messageQueryByCreatorId :: CreatorId -> Query Db1 () (PgR TMessage)
messageQueryByCreatorId i = proc () -> do
  m <- query TMessage -< ()
  restrict -< eq (#creator_id m) (kol i)
  returnA -< m

messageFetchById :: (MonadIO m, MonadThrow m)
                 => MessageId
                 -> Conn' Db1
                 -> m (Maybe (HsR TMessage))
messageFetchById i conn =
  runQuery1 conn (messageQueryById i)

messageFetchByChannelId :: (MonadIO m, MonadThrow m)
                        => ChannelId
                        -> Conn' Db1
                        -> m (Maybe (HsR TMessage))
messageFetchByChannelId i conn =
  runQuery1 conn (messageQueryByChannelId i)

messageFetchByCreatorId :: (MonadIO m, MonadThrow m)
                        => CreatorId
                        -> Conn' Db1
                        -> m (Maybe (HsR TMessage))
messageFetchByCreatorId i conn =
  runQuery1 conn (messageQueryByCreatorId i)

messageInsert :: (MonadIO m, MonadThrow m)
              => CreatorId
              -> Kind
              -> MessageW
              -> Conn' Db1
              -> m (Either QueryError (HsR TMessage))
messageInsert i k c conn = do
  runInsertReturning1 conn TMessage id (messageWToHsI i k c) >>= return . Right

messageUpdate :: (MonadIO m, MonadThrow m)
              => MessageU
              -> Conn' Db1
              -> m (Either QueryError ())
messageUpdate m conn = do
  chk <- runQuery1 conn (messageQueryById (m ^. muMessageId))
  case (chk :: Maybe (HsR TMessage)) of
    Nothing ->
      return . Left $ DoesNotExist
    Just _ ->
      runUpdate conn TMessage upd fid >> return (Right ())
  where
    upd = updTitle (m ^. muTitle) .
          updMessage (m ^. muMessage) .
          updUpdated

    fid :: PgR TMessage -> Kol PGBool
    fid mu = eq (kol $ m ^. muMessageId) (#message_id mu)

    updTitle :: Maybe Title -> PgW TMessage -> PgW TMessage
    updTitle t = set #title (koln t)

    updMessage :: Maybe Message -> PgW TMessage -> PgW TMessage
    updMessage (Just m) = set #message (kol m)
    updMessage _ = id

    updUpdated :: PgW TMessage -> PgW TMessage
    updUpdated = set #updated WDef
