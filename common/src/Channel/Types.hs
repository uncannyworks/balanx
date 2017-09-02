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
  ( Name
  , mkName
  , Access
  , mkAccess
  ) where

#ifndef __GHCJS__
import Tisch
#endif

import Uncanny.Prelude

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
