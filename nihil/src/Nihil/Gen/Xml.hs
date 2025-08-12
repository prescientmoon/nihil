module Nihil.Gen.Xml
  ( XmlAttrs
  , XmlGenState (..)
  , XmlGenT (..)
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
import Relude

type XmlAttrs = Seq (Text, Maybe ByteString)

data XmlGenState
  = XmlGenState
  { out ∷ Builder
  , attrs ∷ XmlAttrs
  }

instance Semigroup XmlGenState where
  a <> b = XmlGenState{out = a.out <> b.out, attrs = a.attrs <> b.attrs}
instance Monoid XmlGenState where
  mempty = XmlGenState mempty mempty

newtype XmlGenT m a
  = XmlGenT (StateT XmlGenState m a)
  deriving newtype (Functor, Applicative, Monad)

getState ∷ ∀ m. (Monad m) ⇒ XmlGenT m XmlGenState
getState = XmlGenT get

setAttrs ∷ ∀ m. (Monad m) ⇒ XmlAttrs → XmlGenT m ()
setAttrs v = XmlGenT $ modify \x → x{attrs = v}

setBuilder ∷ ∀ m. (Monad m) ⇒ Builder → XmlGenT m ()
setBuilder v = XmlGenT $ modify \x → x{out = v}

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

content ∷ ∀ m. (Monad m) ⇒ ByteString → XmlGenT m ()
content rest = do
  st ← getState
  setBuilder $ st.out <> escapeXml rest

-- | Similar to @`content`, except the xml does not get escaped.
rawContent ∷ ∀ m. (Monad m) ⇒ ByteString → XmlGenT m ()
rawContent rest = do
  st ← getState
  setBuilder $ st.out <> Builder.byteString rest

attr ∷ ∀ m. (Monad m) ⇒ Text → ByteString → XmlGenT m ()
attr key value = do
  st ← getState
  setAttrs $ st.attrs |> (key, Just value)

valuelessAttr ∷ ∀ m. (Monad m) ⇒ Text → XmlGenT m ()
valuelessAttr key = do
  st ← getState
  setAttrs $ st.attrs |> (key, Nothing)
