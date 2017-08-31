{-# LANGUAGE
    CPP
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeFamilies
  , TypeSynonymInstances
#-}

module Message.Types
  ( MessageId
  , mkMessageId
  , ParentId
  , mkParentId
  , Message
  , mkMessage
  ) where

#ifndef __GHCJS__
import Tisch
#endif

import Uncanny.Prelude

data MessageIdTag
type MessageId = Tagged MessageIdTag Int32

mkMessageId :: Int32 -> MessageId
mkMessageId = Tagged

#ifndef __GHCJS__
instance PgTyped MessageId where
  type PgType MessageId = PGInt4
instance PgEq MessageId
instance QueryRunnerColumnDefault PGInt4 MessageId where
  queryRunnerColumnDefault = qrcWrapped
#endif

data ParentIdTag
type ParentId = Tagged ParentIdTag Int32

mkParentId :: Int32 -> ParentId
mkParentId = Tagged

#ifndef __GHCJS__
instance PgTyped ParentId where
  type PgType ParentId = PGInt4
instance PgEq ParentId
instance QueryRunnerColumnDefault PGInt4 ParentId where
  queryRunnerColumnDefault = qrcWrapped
#endif

data MessageTag
type Message = Tagged MessageTag Text

mkMessage :: Text -> Message
mkMessage = Tagged

#ifndef __GHCJS__
instance PgTyped Message where
  type PgType Message = PGText
instance PgEq Message
instance QueryRunnerColumnDefault PGText Message where
  queryRunnerColumnDefault = qrcWrapped
#endif
