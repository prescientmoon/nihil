module Nihil.Config
  ( Config (..)
  , getConfig
  ) where

import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Relude
import System.Directory (canonicalizePath)
import System.Environment (getEnv)
import System.Environment.Blank (getEnvDefault)

data Config = Config
  { baseUrl ∷ Text
  , contentPaths ∷ Seq FilePath
  , outPath ∷ FilePath
  , statePath ∷ FilePath
  , mutateState ∷ Bool
  , includeDrafts ∷ Bool
  , pulldownLatexAssetPath ∷ FilePath
  }
  deriving (Generic, Show)

getConfig ∷ IO Config
getConfig = do
  contentPaths ←
    getEnv "NIHIL_CONTENT"
      <&> Text.pack
      <&> Text.splitOn ","
      <&> Seq.fromList
      >>= traverse (canonicalizePath . Text.unpack)
  outPath ← getEnv "NIHIL_OUT" >>= canonicalizePath
  baseUrl ← getEnv "NIHIL_BASE_URL"
  statePath ← getEnv "NIHIL_STATE"
  mutateState ← getEnvDefault "NIHIL_MUTATE" "0"
  includeDrafts ← getEnvDefault "NIHIL_DRAFTS" "0"
  pulldownLatexAssetPath ← getEnv "NIHIL_MATH_ASSETS"
  pure $
    Config
      { baseUrl = Text.pack baseUrl
      , contentPaths = contentPaths
      , outPath = outPath
      , statePath = statePath
      , mutateState = mutateState == "1"
      , includeDrafts = includeDrafts == "1"
      , pulldownLatexAssetPath = pulldownLatexAssetPath
      }
