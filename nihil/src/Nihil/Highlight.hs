module Nihil.Highlight (highlight) where

import Data.Text qualified as Text
import GHC.IO (unsafePerformIO)
import Relude
import System.Process qualified as Process

highlightIO ∷ Text → Text → IO Text
highlightIO lang content = do
  let process =
        Process.proc
          "nihil-highlighter"
          [Text.unpack lang]

  output ← Process.readCreateProcess process $ Text.unpack content
  pure $ Text.pack output

{-# NOINLINE highlight #-}
highlight ∷ Text → Text → Text
highlight lang content = force $ unsafePerformIO $ highlightIO lang content
