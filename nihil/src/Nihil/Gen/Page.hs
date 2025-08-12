module Nihil.Gen.Page
  ( FullPage (..)
  , PageMetadata (..)
  , Heading (..)
  , elabPage
  ) where

import Data.List qualified as List
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Djot qualified as Djot
import Nihil.Content.Config (PageConfig, configCodec)
import Nihil.Content.Find (InputPage (..))
import Nihil.Djot (hasClass)
import Nihil.Gen.Text (inlinesToText)
import Relude
import Toml qualified

data FullPage
  = FullPage
  { input ∷ InputPage
  , meta ∷ PageMetadata
  , lastModified ∷ Maybe UTCTime
  }
  deriving (Show, Generic)

data PageMetadata = PageMetadata
  { config ∷ PageConfig
  , title ∷ Maybe Heading
  , description ∷ Djot.Blocks
  , toc ∷ Seq Heading
  , wordCount ∷ Int
  }
  deriving (Show, Generic)

data Heading = Heading
  { level ∷ Int
  , id ∷ Text
  , contents ∷ Djot.Inlines
  }
  deriving (Show, Generic)

-- Page elaboration
-- {{{ Page
elabPage ∷ InputPage → FullPage
elabPage page =
  FullPage
    { input = page
    , meta = elabMeta page
    , lastModified = Nothing
    }

-- }}}
-- {{{ Metadata
elabMeta ∷ InputPage → PageMetadata
elabMeta page =
  almostComplete
    { title = Seq.lookup 0 almostComplete.toc
    }
 where
  almostComplete = goBlocks page.djot.docBlocks

  goBlocks (Djot.Many blocks) = foldMap goBlock blocks
  goBlock (Djot.Node _ attrs inner) = case inner of
    Djot.Para inlines → goInlines inlines
    Djot.Section (Djot.Many (Djot.Node _ _ (Djot.Heading level inlines) :<| rest)) → do
      let (Djot.Attr attrs') = attrs
      let headingId = case List.lookup "id" attrs' of
            Just res → decodeUtf8 res
            Nothing → error $ "Heading " <> show inner <> " has no id."

      fold
        [ mempty
            { toc = pure $ Heading level headingId inlines
            }
        , goInlines inlines
        , goBlocks (Djot.Many rest)
        ]
    Djot.Section blocks → goBlocks blocks
    Djot.Heading _ inlines →
      error $
        "Found heading outside section: `"
          <> inlinesToText inlines
          <> "`"
    Djot.BlockQuote blocks → goBlocks blocks
    Djot.CodeBlock _ _ → mempty
    Djot.Div blocks
      | hasClass "comment" attrs → mempty
      | hasClass "description" attrs →
          goBlocks blocks
            <> mempty
              { description = blocks
              }
      | otherwise → goBlocks blocks
    Djot.OrderedList _ _ blocks → foldMap goBlocks blocks
    Djot.BulletList _ blocks → foldMap goBlocks blocks
    Djot.TaskList _ blocks → foldMap (goBlocks . snd) blocks
    Djot.DefinitionList _ blocks →
      foldMap
        (\(k, v) → goInlines k <> goBlocks v)
        blocks
    Djot.ThematicBreak → mempty
    Djot.Table caption cells →
      foldMap (goBlocks . coerce) caption
        <> foldMap
          (\(Djot.Cell _ _ inlines) → goInlines inlines)
          (join cells)
    Djot.RawBlock (Djot.Format "toml") contents
      | hasClass "config" attrs → case Toml.decodeExact configCodec (decodeUtf8 contents) of
          Right config → mempty{config = config}
          Left errs →
            error $
              fold
                [ "Errors occurred when decoding config for page `"
                , Text.pack page.path
                , "`: "
                , Toml.prettyTomlDecodeErrors errs
                ]
    Djot.RawBlock _ _ → mempty

  goInlines (Djot.Many inlines) = foldMap goInline inlines
  goInline (Djot.Node _ _ inner) = case inner of
    Djot.Str s → mempty{wordCount = length $ Text.words $ decodeUtf8 s}
    Djot.Emph inlines → goInlines inlines
    Djot.Strong inlines → goInlines inlines
    Djot.Highlight inlines → goInlines inlines
    Djot.Insert inlines → goInlines inlines
    Djot.Delete inlines → goInlines inlines
    Djot.Superscript inlines → goInlines inlines
    Djot.Subscript inlines → goInlines inlines
    Djot.Verbatim s → mempty{wordCount = length $ Text.words $ decodeUtf8 s}
    Djot.Symbol _ → mempty
    Djot.Math _ _ → mempty
    Djot.Link inlines _ → goInlines inlines
    Djot.Image inlines _ → goInlines inlines
    Djot.Span inlines → goInlines inlines
    Djot.FootnoteReference _ → mempty
    Djot.UrlLink _ → mempty
    Djot.EmailLink _ → mempty
    Djot.RawInline _ _ → mempty
    Djot.NonBreakingSpace → mempty
    Djot.Quoted _ inlines → goInlines inlines
    Djot.SoftBreak → mempty
    Djot.HardBreak → mempty

-- }}}

-- Helpers
-- {{{ Instances
instance Semigroup PageMetadata where
  a <> b =
    PageMetadata
      { config = a.config <> b.config
      , title = a.title <|> b.title
      , description = a.description <> b.description
      , toc = a.toc <> b.toc
      , wordCount = a.wordCount + b.wordCount
      }

instance Monoid PageMetadata where
  mempty =
    PageMetadata
      { config = mempty
      , title = Nothing
      , description = mempty
      , toc = mempty
      , wordCount = 0
      }

-- }}}
