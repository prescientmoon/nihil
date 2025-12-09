module Nihil.Math (renderMath) where

import Data.Text qualified as Text
import Djot qualified
import GHC.IO (unsafePerformIO)
import Relude
import System.Process qualified as Process

renderMathIO ∷ Text → Text → IO Text
renderMathIO kind content = do
  let process =
        Process.proc
          "nihil-math-renderer"
          [Text.unpack kind]

  output ← Process.readCreateProcess process $ Text.unpack content
  pure $ Text.pack output

{-# NOINLINE renderMath #-}
renderMath ∷ Djot.MathStyle → Text → Text
renderMath kind content = force $ unsafePerformIO $ renderMathIO kind' content
 where
  kind' = case kind of
    Djot.DisplayMath → "block"
    Djot.InlineMath → "inline"
