{-# LANGUAGE
    CPP
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeFamilies
  , TypeSynonymInstances
#-}

module Common.Types
  ( Created
  , mkCreated
  , CreatorId
  , mkCreatorId
  , Kind
  , mkKind
  , Permissions
  , mkPermissions
  , Title
  , mkTitle
  , Updated
  , mkUpdated
  ) where

import Data.Time.Clock
#ifndef __GHCJS__
import Tisch
#endif

import Uncanny.Prelude

data CreatedTag
type Created = Tagged CreatedTag UTCTime

mkCreated :: UTCTime -> Created
mkCreated = Tagged

#ifndef __GHCJS__
instance PgTyped Created where
  type PgType Created = PGTimestamptz
instance PgEq Created
instance QueryRunnerColumnDefault PGTimestamptz Created where
  queryRunnerColumnDefault = qrcWrapped
#endif

data CreatorIdTag
type CreatorId = Tagged CreatorIdTag Int32

mkCreatorId :: Int32 -> CreatorId
mkCreatorId = Tagged

#ifndef __GHCJS__
instance PgTyped CreatorId where
  type PgType CreatorId = PGInt4
instance PgEq CreatorId
instance QueryRunnerColumnDefault PGInt4 CreatorId where
  queryRunnerColumnDefault = qrcWrapped
#endif

data KindTag
type Kind = Tagged KindTag Int32

mkKind :: Int32 -> Kind
mkKind = Tagged

#ifndef __GHCJS__
instance PgTyped Kind where
  type PgType Kind = PGInt4
instance PgEq Kind
instance QueryRunnerColumnDefault PGInt4 Kind where
  queryRunnerColumnDefault = qrcWrapped
#endif

data PermissionsTag
type Permissions = Tagged PermissionsTag Int32

mkPermissions :: Int32 -> Permissions
mkPermissions = Tagged

#ifndef __GHCJS__
instance PgTyped Permissions where
  type PgType Permissions = PGInt4
instance PgEq Permissions
instance QueryRunnerColumnDefault PGInt4 Permissions where
  queryRunnerColumnDefault = qrcWrapped
#endif

data TitleTag
type Title = Tagged TitleTag Text

mkTitle :: Text -> Title
mkTitle = Tagged

#ifndef __GHCJS__
instance PgTyped Title where
  type PgType Title = PGText
instance PgEq Title
instance QueryRunnerColumnDefault PGText Title where
  queryRunnerColumnDefault = qrcWrapped
#endif

data UpdatedTag
type Updated = Tagged UpdatedTag UTCTime

mkUpdated :: UTCTime -> Updated
mkUpdated = Tagged

#ifndef __GHCJS__
instance PgTyped Updated where
  type PgType Updated = PGTimestamptz
instance PgEq Updated
instance QueryRunnerColumnDefault PGTimestamptz Updated where
  queryRunnerColumnDefault = qrcWrapped
#endif
