module Nihil.Context (Context (..), echoes) where

import Data.Sequence qualified as Seq
import Nihil.Config (Config)
import Nihil.Page.Find (InputPage (..))
import Nihil.Page.Meta (FullPage (..), PageConfig (..), PageMetadata (..))
import Nihil.Route (Route (Echo))
import Nihil.State (PersistentState)
import Relude

data Context = Context
  { pages ∷ Seq FullPage
  , state ∷ PersistentState
  , config ∷ Config
  }
  deriving (Generic, Show)

-- | Selects only the (non-hidden) echoes in the page list.
echoes ∷ Context → Seq FullPage
echoes ctx =
  ctx.pages
    -- remove hidden files
    & Seq.filter (\p → not p.meta.config.hidden)
    & Seq.filter -- only keep echoes
      ( \p → case p.input.route of
          Echo _ → True
          _ → False
      )
