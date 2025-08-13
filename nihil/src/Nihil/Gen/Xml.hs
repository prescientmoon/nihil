module Nihil.Gen.Xml
  ( XmlAttrs
  , HtmlAttrs
  , XmlGenState (..)
  , XmlGenT (..)
  , XmlGen
  , HtmlGenT
  , HtmlGen
  , genRaw
  , genTRaw
  , tag
  , singleTag
  , content
  , rawContent
  , attr
  , valuelessAttr
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Sequence ((|>))
import Data.Text qualified as Text
import Relude

-- {{{ State
type XmlAttrs = Seq (Text, Maybe ByteString)
type HtmlAttrs = XmlAttrs

data XmlGenState
  = XmlGenState
  { out ∷ Builder
  , attrs ∷ XmlAttrs
  }

instance Semigroup XmlGenState where
  a <> b = XmlGenState{out = a.out <> b.out, attrs = a.attrs <> b.attrs}
instance Monoid XmlGenState where
  mempty = XmlGenState mempty mempty

-- }}}
-- {{{ The monad
newtype XmlGenT m a
  = XmlGenT (StateT XmlGenState m a)
  deriving newtype (Functor, Applicative, Monad)

type XmlGen = XmlGenT Identity
type HtmlGenT = XmlGenT
type HtmlGen = HtmlGenT Identity

instance (Monad m, a ~ ()) ⇒ IsString (XmlGenT m a) where
  fromString = content . Text.pack

genRaw ∷ HtmlGen () → Text
genRaw = runIdentity . genTRaw

genTRaw ∷ ∀ m. (Monad m) ⇒ HtmlGenT m () → m Text
genTRaw (XmlGenT action) = do
  st ← execStateT action mempty
  pure . decodeUtf8 . fromLazy . Builder.toLazyByteString $ st.out

-- }}}
-- {{{ Internal helpers
getState ∷ ∀ m. (Monad m) ⇒ XmlGenT m XmlGenState
getState = XmlGenT get

setAttrs ∷ ∀ m. (Monad m) ⇒ XmlAttrs → XmlGenT m ()
setAttrs v = XmlGenT $ modify \x → x{attrs = v}

setBuilder ∷ ∀ m. (Monad m) ⇒ Builder → XmlGenT m ()
setBuilder v = XmlGenT $ modify \x → x{out = v}

-- Original version was taken from hsdjot
attrsToBuilder ∷ XmlAttrs → Builder
attrsToBuilder pairs = foldMap go pairs
 where
  go (k, Nothing) = Builder.byteString $ " " <> encodeUtf8 k
  go (k, Just v) =
    fold
      [ " "
      , Builder.byteString $ encodeUtf8 k
      , "=\""
      , escapeXml v
      , "\""
      ]

-- Original version was taken from hsdjot
escapeXml ∷ ByteString → Builder
escapeXml bs
  | hasEscapable bs = BS.foldl' (\b c → b <> replace c) mempty bs
  | otherwise = Builder.byteString bs
 where
  hasEscapable = BS.any (\w → w == 38 || w == 60 || w == 62 || w == 34)

  replace 38 = Builder.byteString "&amp;"
  replace 60 = Builder.byteString "&lt;"
  replace 62 = Builder.byteString "&gt;"
  replace 34 = Builder.byteString "&quot;"
  replace c = Builder.word8 c

-- }}}
-- {{{ Tag
tag ∷ ∀ m a. (Monad m) ⇒ Text → XmlGenT m a → XmlGenT m a
tag name action = do
  prev ← getState
  setAttrs mempty
  setBuilder mempty

  res ← action
  inner ← getState

  setAttrs prev.attrs
  setBuilder . fold $ -- Original version was taken from hsdjot
    [ prev.out
    , "<"
    , Builder.byteString $ encodeUtf8 name
    , attrsToBuilder inner.attrs
    , ">"
    , inner.out
    , "</"
    , Builder.byteString $ encodeUtf8 name
    , ">"
    ]

  pure res

-- }}}
-- {{{ Single tag
singleTag ∷ ∀ m a. (Monad m) ⇒ Text → XmlGenT m a → XmlGenT m a
singleTag name action = do
  prev ← getState
  setAttrs mempty
  setBuilder mempty

  res ← action
  inner ← getState

  setAttrs prev.attrs
  setBuilder . fold $
    [ prev.out
    , "<"
    , Builder.byteString $ encodeUtf8 name
    , attrsToBuilder inner.attrs
    , "/>\n"
    ]

  pure res

-- }}}
-- {{{ Content
content ∷ ∀ m. (Monad m) ⇒ Text → XmlGenT m ()
content rest = do
  st ← getState
  setBuilder $ st.out <> escapeXml (encodeUtf8 rest)

-- | Similar to @`content`, except the xml does not get escaped.
rawContent ∷ ∀ m. (Monad m) ⇒ Text → XmlGenT m ()
rawContent rest = do
  st ← getState
  setBuilder $ st.out <> Builder.byteString (encodeUtf8 rest)

-- }}}
-- {{{ Attrs
attr ∷ ∀ m. (Monad m) ⇒ Text → Text → XmlGenT m ()
attr key value = do
  st ← getState
  setAttrs $ st.attrs |> (key, Just $ encodeUtf8 value)

valuelessAttr ∷ ∀ m. (Monad m) ⇒ Text → XmlGenT m ()
valuelessAttr key = do
  st ← getState
  setAttrs $ st.attrs |> (key, Nothing)

-- }}}
