module Nihil.Context where

import Nihil.Config (Config)
import Nihil.Gen.Page (FullPage)
import Nihil.State (PersistentState)
import Relude

data Context = Context
  { pages ∷ Seq FullPage
  , state ∷ PersistentState
  , config ∷ Config
  }
  deriving (Generic, Show)
