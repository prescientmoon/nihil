module Nihil.Djot (hasClass, getAttr) where

import Data.List qualified as List
import Data.Text qualified as Text
import Djot qualified as Djot
import Relude

hasClass ∷ Text → Djot.Attr → Bool
hasClass name (Djot.Attr attrs) =
  maybe False (elem name . Text.words . decodeUtf8) $
    List.lookup "class" attrs

getAttr ∷ Text → Djot.Attr → Maybe ByteString
getAttr name (Djot.Attr attrs) = List.lookup (encodeUtf8 name) attrs
