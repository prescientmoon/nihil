module Nihil.Config
  ( Config (..)
  , getConfig
  ) where

import Data.Text qualified as Text
import Relude
import System.Directory (canonicalizePath)
import System.Environment (getEnv)
import System.Environment.Blank (getEnvDefault)

data Config = Config
  { baseUrl ∷ Text
  , contentPath ∷ FilePath
  , publicPath ∷ FilePath
  , outPath ∷ FilePath
  , statePath ∷ FilePath
  , mutateState ∷ Bool
  , includeDrafts ∷ Bool
  , lmodernPath ∷ FilePath
  , cmodernPath ∷ FilePath
  }
  deriving (Generic, Show)

getConfig ∷ IO Config
getConfig = do
  contentPath ← getEnv "NIHIL_CONTENT" >>= canonicalizePath
  publicPath ← getEnv "NIHIL_PUBLIC" >>= canonicalizePath
  outPath ← getEnv "NIHIL_OUT" >>= canonicalizePath
  baseUrl ← getEnv "NIHIL_BASE_URL"
  statePath ← getEnv "NIHIL_STATE"
  mutateState ← getEnvDefault "NIHIL_MUTATE" "0"
  includeDrafts ← getEnvDefault "NIHIL_DRAFTS" "0"
  lmodernPath ← getEnv "NIHIL_LMODERN"
  cmodernPath ← getEnv "NIHIL_CMODERN"
  pure $
    Config
      { baseUrl = Text.pack baseUrl
      , contentPath = contentPath
      , publicPath = publicPath
      , outPath = outPath
      , statePath = statePath
      , mutateState = mutateState == "1"
      , includeDrafts = includeDrafts == "1"
      , lmodernPath = lmodernPath
      , cmodernPath = cmodernPath
      }
