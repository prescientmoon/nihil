module Nihil.Gen.Html
  ( HtmlAttrs
  , HtmlGenT
  , HtmlGen
  , tag
  , singleTag
  , content
  , rawContent
  , attr
  , valuelessAttr
  , genRaw
  , genTRaw
  ) where

import Data.ByteString.Builder qualified as Builder
import Nihil.Gen.Xml
  ( XmlAttrs
  , XmlGenState (..)
  , XmlGenT (..)
  , attr
  , content
  , rawContent
  , singleTag
  , tag
  , valuelessAttr
  )
import Relude

type HtmlAttrs = XmlAttrs
type HtmlGenT = XmlGenT
type HtmlGen = HtmlGenT Identity

genRaw ∷ HtmlGen () → ByteString
genRaw = runIdentity . genTRaw

genTRaw ∷ ∀ m. (Monad m) ⇒ HtmlGenT m () → m (ByteString)
genTRaw (XmlGenT action) = do
  st ← execStateT action mempty
  pure . fromLazy . Builder.toLazyByteString $ st.out
