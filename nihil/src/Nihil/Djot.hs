module Nihil.Djot
  ( hasClass
  , getAttr
  , blocksToText
  , inlinesToText
  , inlinesWithoutLinks
  ) where

import Data.List qualified as List
import Data.Text qualified as Text
import Djot qualified as Djot
import Relude

hasClass ∷ Text → Djot.Attr → Bool
hasClass name (Djot.Attr attrs) =
  maybe False (elem name . Text.words . decodeUtf8) $
    List.lookup "class" attrs

getAttr ∷ Text → Djot.Attr → Maybe Text
getAttr name (Djot.Attr attrs) =
  decodeUtf8 <$> List.lookup (encodeUtf8 name) attrs

blocksToText ∷ Djot.Blocks → Text
blocksToText = foldMap go . Djot.unMany
 where
  go (Djot.Node _ attrs x) = case x of
    Djot.Para inlines → inlinesToText inlines
    Djot.Section blocks → blocksToText blocks
    Djot.Heading _ inlines → inlinesToText inlines
    Djot.BlockQuote blocks → blocksToText blocks
    Djot.CodeBlock _ _ → mempty
    Djot.Div blocks
      | hasClass "comment" attrs → mempty
      | hasClass "description" attrs → mempty
      | hasClass "figure" attrs → mempty
      | hasClass "caption" attrs → mempty
      | hasClass "image-figure" attrs → mempty
      | hasClass "embed-description" attrs → mempty
      | hasClass "echo-list" attrs → mempty
      | hasClass "toc" attrs → mempty
      | otherwise → blocksToText blocks
    Djot.OrderedList _ _ blocks → foldMap blocksToText blocks
    Djot.BulletList _ blocks → foldMap blocksToText blocks
    Djot.ThematicBreak → "\n"
    Djot.RawBlock _ _ → mempty
    Djot.DefinitionList _ _ → mempty -- I don't wanna bother aaaa
    Djot.TaskList _ _ → error "Task lists are not implemented"
    Djot.Table _ _ → error "Tables are not implemented"

inlinesToText ∷ Djot.Inlines → Text
inlinesToText = foldMap go . Djot.unMany
 where
  go (Djot.Node _ _ x) = case x of
    Djot.Str bs → decodeUtf8 bs
    Djot.Emph ils → inlinesToText ils
    Djot.Strong ils → inlinesToText ils
    Djot.Highlight ils → inlinesToText ils
    Djot.Insert ils → inlinesToText ils
    Djot.Delete ils → inlinesToText ils
    Djot.Superscript ils → inlinesToText ils
    Djot.Subscript ils → inlinesToText ils
    Djot.Quoted Djot.SingleQuotes ils →
      "'" <> inlinesToText ils <> "'"
    Djot.Quoted Djot.DoubleQuotes ils →
      "\"" <> inlinesToText ils <> "\""
    Djot.Verbatim bs → decodeUtf8 bs
    Djot.Math Djot.DisplayMath bs → "$$" <> decodeUtf8 bs <> "$$"
    Djot.Math Djot.InlineMath bs → "$" <> decodeUtf8 bs <> "$"
    Djot.Symbol bs → ":" <> decodeUtf8 bs <> ":"
    Djot.Link ils _url → inlinesToText ils
    Djot.Image ils _url → inlinesToText ils -- TODO: is this correct?
    Djot.Span ils → inlinesToText ils
    Djot.UrlLink url → decodeUtf8 url
    Djot.EmailLink email → decodeUtf8 email
    Djot.RawInline _ _ → mempty
    Djot.FootnoteReference bs → "[" <> decodeUtf8 bs <> "]"
    Djot.SoftBreak → "\n"
    Djot.HardBreak → "\n"
    Djot.NonBreakingSpace → "\160"

-- We pass the contents of certain links through this to prevent nested links.
inlinesWithoutLinks ∷ Djot.Inlines → Djot.Inlines
inlinesWithoutLinks = Djot.Many . fmap (fmap go) . Djot.unMany
 where
  go = \case
    Djot.Link ils _ → Djot.Span $ inlinesWithoutLinks ils
    Djot.Emph ils → Djot.Emph $ inlinesWithoutLinks ils
    Djot.Strong ils → Djot.Strong $ inlinesWithoutLinks ils
    Djot.Highlight ils → Djot.Highlight $ inlinesWithoutLinks ils
    Djot.Insert ils → Djot.Insert $ inlinesWithoutLinks ils
    Djot.Delete ils → Djot.Delete $ inlinesWithoutLinks ils
    Djot.Superscript ils → Djot.Superscript $ inlinesWithoutLinks ils
    Djot.Subscript ils → Djot.Subscript $ inlinesWithoutLinks ils
    Djot.Quoted q ils → Djot.Quoted q $ inlinesWithoutLinks ils
    Djot.Image ils url → Djot.Image (inlinesWithoutLinks ils) url
    Djot.Span ils → Djot.Span  $ inlinesWithoutLinks ils
    other → other
