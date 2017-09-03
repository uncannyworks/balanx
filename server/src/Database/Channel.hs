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

import Channel.Json
import Channel.Types
import Common.Types
import Shared
import Types
import Uncanny.Prelude
import User.Types

data TChannel
data instance Table TChannel      = TChannel
type instance Database TChannel   = Db1
type instance SchemaName TChannel = "public"
type instance TableName TChannel  = "channels"
type instance Columns TChannel    =
  [ 'Column "channel_id" 'WD 'R  PGInt4        ChannelId
  , 'Column "name"       'W  'R  PGText        Name
  , 'Column "title"      'W  'R  PGText        Title
  , 'Column "access"     'W  'R  PGInt4        Access
  , 'Column "kind"       'W  'R  PGInt4        Kind
  , 'Column "creator_id" 'W  'R  PGInt4        CreatorId
  , 'Column "created"    'WD 'R  PGTimestamptz Created
  , 'Column "updated"    'WD 'R  PGTimestamptz Updated
  ]

hsRToChannelR :: HsR TChannel -> ChannelR
hsRToChannelR c = ChannelR
  (view #channel_id c)
  (view #name c)
  (view #title c)
  (view #access c)
  (view #kind c)
  (view #creator_id c)
  (view #created c)
  (view #updated c)

channelWToHsI :: CreatorId -> ChannelW -> HsI TChannel
channelWToHsI i c =
  mkHsI TChannel
    (hsi #channel_id WDef)
    (hsi #name       (c ^. cwName))
    (hsi #title      (c ^. cwTitle))
    (hsi #access     (c ^. cwAccess))
    (hsi #kind       (c ^. cwKind))
    (hsi #creator_id i)
    (hsi #created    WDef)
    (hsi #updated    WDef)

channelQueryAll :: Query Db1 () (PgR TChannel)
channelQueryAll = proc () -> do
  c <- query TChannel -< ()
  returnA -< c

channelQueryById :: ChannelId -> Query Db1 () (PgR TChannel)
channelQueryById i = proc () -> do
  c <- query TChannel -< ()
  restrict -< eq (#channel_id c) (kol i)
  returnA -< c

channelQueryByName :: Name -> Query Db1 () (PgR TChannel)
channelQueryByName n = proc () -> do
  c <- query TChannel -< ()
  restrict -< eq (#name c) (kol n)
  returnA -< c

channelFetchAll :: (MonadIO m, MonadThrow m)
                => Conn' Db1
                -> m [HsR TChannel]
channelFetchAll conn =
  runQuery conn channelQueryAll

channelFetchById :: (MonadIO m, MonadThrow m)
                 => ChannelId
                 -> Conn' Db1
                 -> m (Maybe (HsR TChannel))
channelFetchById i conn =
  runQuery1 conn (channelQueryById i)

channelFetchByName :: (MonadIO m, MonadThrow m)
                   => Name
                   -> Conn' Db1
                   -> m (Maybe (HsR TChannel))
channelFetchByName n conn =
  runQuery1 conn (channelQueryByName n)

channelInsert :: (MonadIO m, MonadThrow m)
              => CreatorId
              -> ChannelW
              -> Conn' Db1
              -> m (Either QueryError (HsR TChannel))
channelInsert i c conn = do
  chk <- runQuery1 conn (channelQueryByName (c ^. cwName))
  case (chk :: Maybe (HsR TChannel)) of
    Just _ ->
      return . Left . Exists "name" $ untag (c ^. cwName)
    Nothing ->
      runInsertReturning1 conn TChannel id (channelWToHsI i c) >>= return . Right

channelUpdate :: (MonadIO m, MonadThrow m)
              => ChannelU
              -> Conn' Db1
              -> m (Either QueryError ())
channelUpdate c conn = do
  chk <- runQuery1 conn (channelQueryById (c ^. cuChannelId))
  case (chk :: Maybe (HsR TChannel)) of
    Nothing ->
      return . Left $ DoesNotExist
    Just _ ->
      runUpdate conn TChannel upd fid >> return (Right ())
  where
    upd = updName (c ^. cuName) .
          updTitle (c ^. cuTitle) .
          updAccess (c ^. cuAccess) .
          updUpdated

    fid :: PgR TChannel -> Kol PGBool
    fid cu = eq (kol $ c ^. cuChannelId) (#channel_id cu)

    updName :: Maybe Name -> PgW TChannel -> PgW TChannel
    updName (Just n) = set #name (kol n)
    updName _ = id

    updTitle :: Maybe Title -> PgW TChannel -> PgW TChannel
    updTitle (Just t) = set #title (kol t)
    updTitle _ = id

    updAccess :: Maybe Access -> PgW TChannel -> PgW TChannel
    updAccess (Just a) = set #access (kol a)
    updAccess _ = id

    updUpdated :: PgW TChannel -> PgW TChannel
    updUpdated = set #updated WDef
