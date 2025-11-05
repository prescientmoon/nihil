module Nihil.Context (Context (..), FullPageTree) where

import Nihil.Config (Config)
import Nihil.Page.Meta (FullPageTree)
import Nihil.State (PersistentState)
import Relude

data Context = Context
  { pages ∷ FullPageTree
  , state ∷ PersistentState
  , config ∷ Config
  }
  deriving (Generic, Show)
