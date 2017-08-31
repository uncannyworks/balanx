{-# LANGUAGE
    CPP
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeFamilies
  , TypeSynonymInstances
#-}

module Channel.Types
  ( ChannelId
  , mkChannelId
  , Name
  , mkName
  , Access
  , mkAccess
  ) where

#ifndef __GHCJS__
import Tisch
#endif

import Uncanny.Prelude

data ChannelIdTag
type ChannelId = Tagged ChannelIdTag Int32

mkChannelId :: Int32 -> ChannelId
mkChannelId = Tagged

#ifndef __GHCJS__
instance PgTyped ChannelId where
  type PgType ChannelId = PGInt4
instance PgEq ChannelId
instance QueryRunnerColumnDefault PGInt4 ChannelId where
  queryRunnerColumnDefault = qrcWrapped
#endif

data NameTag
type Name = Tagged NameTag Text

mkName :: Text -> Name
mkName = Tagged

#ifndef __GHCJS__
instance PgTyped Name where
  type PgType Name = PGText
instance PgEq Name
instance QueryRunnerColumnDefault PGText Name where
  queryRunnerColumnDefault = qrcWrapped
#endif

data AccessTag
type Access = Tagged AccessTag Int32

mkAccess :: Int32 -> Access
mkAccess = Tagged

#ifndef __GHCJS__
instance PgTyped Access where
  type PgType Access = PGInt4
instance PgEq Access
instance QueryRunnerColumnDefault PGInt4 Access where
  queryRunnerColumnDefault = qrcWrapped
#endif
