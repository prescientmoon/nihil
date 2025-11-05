module Nihil.Context (Context (..), FullPageTree, echoes) where

import Nihil.Config (Config)
import Nihil.Page.Find (InputPage (..))
import Nihil.Page.Meta (FullPage (..), PageConfig (..), PageMetadata (..))
import Nihil.State (PersistentState)
import Nihil.Tree qualified as Tree
import Relude

type FullPageTree = Tree.Forest FullPage FilePath FilePath
data Context = Context
  { pages ∷ FullPageTree
  , state ∷ PersistentState
  , config ∷ Config
  }
  deriving (Generic, Show)

-- | Selects only the (non-hidden) echoes in the page list.
echoes ∷ Context → FullPageTree
echoes ctx =
  ctx.pages
    -- remove hidden files
    & Tree.filterNodes (\p → not p.meta.config.hidden)
    & Tree.filterNodes -- only keep echoes
      (\p → isPrefixOf "echoes/" p.input.route)
