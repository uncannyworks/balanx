{-# LANGUAGE TemplateHaskell #-}

module Channel.Json where

import Control.Lens hiding ((.=))
import Control.Monad (fail, mzero)
import Data.Aeson
import Data.Maybe (fromJust)

import Channel.Types
import Common.Types
import Shared
import Uncanny.Prelude

data ChannelR
  = ChannelR
  { _crChannelId :: ChannelId
  , _crName      :: Name
  , _crTitle     :: Title
  , _crAccess    :: Access
  , _crKind      :: Kind
  , _crCreatorId :: CreatorId
  , _crCreated   :: Created
  , _crUpdated   :: Updated
  } deriving (Show, Eq)

makeLenses ''ChannelR

instance ToJSON ChannelR where
  toJSON c =
    object [ "channel_id" .= (c ^. crChannelId)
           , "name"       .= (c ^. crName)
           , "title"      .= (c ^. crTitle)
           , "access"     .= (c ^. crAccess)
           , "kind"       .= (c ^. crKind)
           , "creator_id" .= (c ^. crCreatorId)
           , "created"    .= (c ^. crCreated)
           , "updated"    .= (c ^. crUpdated)
           ]

instance FromJSON ChannelR where
  parseJSON (Object v) = ChannelR
    <$>  v .: "channel_id"
    <*> (v .: "name" >>= vName)
    <*> (v .: "title" >>= vTitle)
    <*>  v .: "access"
    <*>  v .: "kind"
    <*>  v .: "creator_id"
    <*>  v .: "created"
    <*>  v .: "updated"
    where
      val    = either (fail . show) (pure . fromJust)
      vName  = val . validateLength (ValProps 3 30 "name")
      vTitle = val . validateLength (ValProps 0 256 "title")
  parseJSON _ = mzero

data ChannelW
  = ChannelW
  { _cwName   :: Name
  , _cwTitle  :: Title
  , _cwAccess :: Access
  , _cwKind   :: Kind
  } deriving (Show, Eq)

makeLenses ''ChannelW

instance ToJSON ChannelW where
  toJSON c =
    object [ "name"   .= (c ^. cwName)
           , "title"  .= (c ^. cwTitle)
           , "access" .= (c ^. cwAccess)
           , "kind"   .= (c ^. cwKind)
           ]

instance FromJSON ChannelW where
  parseJSON (Object v) = ChannelW
    <$> v .: "name"
    <*> v .: "title"
    <*> v .: "access"
    <*> v .: "kind"
  parseJSON _ = mzero

data ChannelU
  = ChannelU
  { _cuChannelId :: ChannelId
  , _cuName      :: Maybe Name
  , _cuTitle     :: Maybe Title
  , _cuAccess    :: Maybe Access
  } deriving (Show, Eq)

makeLenses ''ChannelU

instance ToJSON ChannelU where
  toJSON c =
    object [ "channel_id" .= (c ^. cuChannelId)
           , "name"       .= (c ^. cuName)
           , "title"      .= (c ^. cuTitle)
           , "access"     .= (c ^. cuAccess)
           ]

instance FromJSON ChannelU where
  parseJSON (Object v) = ChannelU
    <$> v .:  "channel_id"
    <*> v .:? "name"
    <*> v .:? "title"
    <*> v .:? "access"
  parseJSON _ = mzero
