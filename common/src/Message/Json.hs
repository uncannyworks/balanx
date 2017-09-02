{-# LANGUAGE TemplateHaskell #-}

module Message.Json where

import Control.Lens hiding ((.=))
import Control.Monad (fail, mzero)
import Data.Aeson
import Data.Maybe (fromJust)

import Message.Types
import Common.Types
import Shared
import Uncanny.Prelude

data MessageR
  = MessageR
  { _mrMessageId :: MessageId
  , _mrParentId  :: ParentId
  , _mrChannelId :: ChannelId
  , _mrCreatorId :: CreatorId
  , _mrTitle     :: Title
  , _mrMessage   :: Message
  , _mrKind      :: Kind
  , _mrCreated   :: Created
  , _mrUpdated   :: Updated
  } deriving (Show, Eq)

makeLenses ''MessageR

instance ToJSON MessageR where
  toJSON m =
    object [ "message_id" .= (m ^. mrMessageId)
           , "parent_id"  .= (m ^. mrParentId)
           , "channel_id" .= (m ^. mrChannelId)
           , "creator_id" .= (m ^. mrCreatorId)
           , "title"      .= (m ^. mrTitle)
           , "message"    .= (m ^. mrMessage)
           , "kind"       .= (m ^. mrKind)
           , "created"    .= (m ^. mrCreated)
           , "updated"    .= (m ^. mrUpdated)
           ]

instance FromJSON MessageR where
  parseJSON (Object v) = MessageR
    <$> v .: "channel_id"
    <*> v .: "parent_id"
    <*> v .: "channel_id"
    <*> v .: "creator_id"
    <*> v .: "title"
    <*> v .: "message"
    <*> v .: "kind"
    <*> v .: "created"
    <*> v .: "updated"
  parseJSON _ = mzero

data MessageW
  = MessageW
  { _mwParentId  :: Maybe ParentId
  , _mwChannelId :: ChannelId
  , _mwTitle     :: Maybe Title
  , _mwMessage   :: Message
  } deriving (Show, Eq)

makeLenses ''MessageW

instance ToJSON MessageW where
  toJSON m =
    object [ "parent_id"  .= (m ^. mwParentId)
           , "channel_id" .= (m ^. mwChannelId)
           , "title"      .= (m ^. mwTitle)
           , "message"    .= (m ^. mwMessage)
           ]

instance FromJSON MessageW where
  parseJSON (Object v) = MessageW
    <$> v .:? "parent_id"
    <*> v .:  "channel_id"
    <*> v .:? "title"
    <*> v .:  "message"
  parseJSON _ = mzero

data MessageU
  = MessageU
  { _muMessageId :: MessageId
  , _muTitle     :: Maybe Title
  , _muMessage   :: Maybe Message
  } deriving (Show, Eq)

makeLenses ''MessageU

instance ToJSON MessageU where
  toJSON m =
    object [ "message_id" .= (m ^. muMessageId)
           , "title"      .= (m ^. muTitle)
           , "message"    .= (m ^. muMessage)
           ]

instance FromJSON MessageU where
  parseJSON (Object v) = MessageU
    <$> v .:  "message_id"
    <*> v .:? "title"
    <*> v .:? "message"
  parseJSON _ = mzero
