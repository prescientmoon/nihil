module Nihil.Page.Meta
  ( FullPage (..)
  , PageMetadata (..)
  , Heading (..)
  , PageConfig (..)
  , SitemapConfig (..)
  , FullPageTree
  , RssFeed (..)
  , PageFilters (..)
  , applyPageFilters
  , getPageFilters
  , elabForest
  , elabPage
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Djot qualified as Djot
import Nihil.Config (Config (..))
import Nihil.Djot (hasClass, inlinesToText)
import Nihil.Djot qualified as Djot
import Nihil.Page.Find (InputPage (..), InputPageTree)
import Nihil.Toml qualified as Toml
import Nihil.Tree qualified as Tree
import Relude
import System.FilePath (joinPath, splitDirectories)
import Toml ((.=))
import Toml qualified as Toml

-- Types
-- {{{ Page config

-- | The configuration for a page
data PageConfig
  = PageConfig
  { hidden ∷ Bool
  , draft ∷ Bool
  , compact ∷ Bool
  , createdAt ∷ Maybe UTCTime
  , sitemap ∷ SitemapConfig
  }
  deriving (Show, Generic)

data SitemapConfig
  = SitemapConfig
  { priority ∷ Maybe Float
  , changefreq ∷ Maybe Text
  , exclude ∷ Bool
  }
  deriving (Show, Generic, Eq)

instance Semigroup SitemapConfig where
  a <> b =
    SitemapConfig
      { priority = a.priority <|> b.priority
      , changefreq = a.changefreq <|> b.changefreq
      , exclude = a.exclude || b.exclude
      }

instance Monoid SitemapConfig where
  mempty =
    SitemapConfig
      { priority = Nothing
      , changefreq = Nothing
      , exclude = False
      }

instance Semigroup PageConfig where
  a <> b =
    PageConfig
      { hidden = a.hidden || b.hidden
      , draft = a.draft || b.draft
      , compact = a.compact || b.compact
      , createdAt = a.createdAt <|> b.createdAt
      , sitemap = a.sitemap <> b.sitemap
      }

instance Monoid PageConfig where
  mempty =
    PageConfig
      { hidden = False
      , draft = False
      , compact = False
      , createdAt = Nothing
      , sitemap = mempty
      }

configCodec ∷ Toml.TomlCodec PageConfig
configCodec =
  PageConfig
    <$> Toml.didefault False (Toml.bool "hidden") .= (\x → x.hidden)
    <*> Toml.didefault False (Toml.bool "draft") .= draft
    <*> Toml.didefault False (Toml.bool "compact") .= compact
    <*> Toml.dioptional (Toml.utcTime "created_at") .= createdAt
    <*> Toml.didefault mempty (Toml.table sitemapCodec "sitemap") .= sitemap

sitemapCodec ∷ Toml.TomlCodec SitemapConfig
sitemapCodec = do
  SitemapConfig
    <$> Toml.dioptional (Toml.float "priority") .= priority
    <*> Toml.dioptional (Toml.text "changefreq") .= changefreq
    <*> Toml.didefault False (Toml.bool "exclude") .= exclude

-- }}}
-- {{{ Filters & rss feeds
-- I might add tag support in the future
data PageFilters
  = PageFilters
  { dir ∷ FilePath
  -- ^ Selects direct children of a post matching the dir name
  , hidden ∷ Bool
  -- ^ Whether to keep hidden elements
  }
  deriving (Show, Generic)

data RssFeed = RssFeed
  { name ∷ Text
  , at ∷ FilePath
  , filters ∷ PageFilters
  , summary ∷ Djot.Blocks
  }
  deriving (Show, Generic)

-- }}}
-- {{{ Metadata
type FullPageTree = Tree.Forest FullPage FilePath FilePath
data FullPage
  = FullPage
  { input ∷ InputPage
  , meta ∷ PageMetadata
  }
  deriving (Show, Generic)

data PageMetadata = PageMetadata
  { config ∷ PageConfig
  , title ∷ Maybe Heading
  , description ∷ Djot.Blocks
  , toc ∷ Seq Heading
  , freshFeeds ∷ Seq RssFeed
  -- ^ The set of feeds declared by this page.
  , underFeeds ∷ Seq RssFeed
  -- ^ The set of feeds present while on this page (some declared here, some
  -- inherited from the page's ancestors).
  , wordCount ∷ Int
  , -- Assigns an unique integer to each footnote ID.
    footnoteOrder ∷ HashMap Text Int
  }
  deriving (Show, Generic)

data Heading = Heading
  { level ∷ Int
  , id ∷ Text
  , contents ∷ Djot.Inlines
  }
  deriving (Show, Generic)

-- }}}

-- Page elaboration
-- {{{ Trees
elabForest ∷ Config → InputPageTree → FullPageTree
elabForest cfg pages =
  pages
    & Tree.mapNodes elabPage
    & Tree.filterNodes
      (\p → cfg.includeDrafts || not p.meta.config.draft)
    & inheritForestFeeds mempty
 where
  inheritForestFeeds feeds (Tree.Forest hm) = do
    Tree.Forest $ inheritTreeFeeds feeds <$> hm
  inheritTreeFeeds _ (Tree.Leaf l) = Tree.Leaf l
  inheritTreeFeeds feeds (Tree.Node page forest) =
    Tree.Node page' $ inheritForestFeeds feeds' forest
   where
    page' =
      page
        { meta =
            page.meta
              { underFeeds = feeds'
              }
        }
    feeds' = feeds <> page.meta.freshFeeds

-- }}}
-- {{{ Page
elabPage ∷ InputPage → FullPage
elabPage page =
  FullPage
    { input = page
    , meta = elabMeta page
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
      | hasClass "page-index" attrs → mempty
      | hasClass "description" attrs →
          goBlocks blocks
            <> mempty
              { description = blocks
              }
      | hasClass "rss" attrs → do
          let filename = fromMaybe "rss" $ Djot.getAttr "at" attrs
          goBlocks blocks
            <> mempty
              { freshFeeds =
                  pure $
                    RssFeed
                      { name =
                          fromMaybe (error "No RSS feed name provided") $
                            Djot.getAttr "name" attrs
                      , at = page.route <> "/" <> Text.unpack filename <> ".xml"
                      , filters = getPageFilters attrs
                      , summary = blocks
                      }
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
    Djot.RawBlock (Djot.Format "toml") (decodeUtf8 → contents)
      | hasClass "config" attrs → case Toml.decodeExact configCodec contents of
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
    Djot.FootnoteReference (decodeUtf8 → to) →
      mempty{footnoteOrder = HashMap.singleton to 1}
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
      , freshFeeds = a.freshFeeds <> b.freshFeeds
      , underFeeds = a.underFeeds <> b.underFeeds
      , wordCount = a.wordCount + b.wordCount
      , footnoteOrder =
          HashMap.fromList
            $ flip zip [1 ..]
            $ sortOn
              ( \k →
                  HashMap.lookup k a.footnoteOrder
                    <|> (+ HashMap.size a.footnoteOrder)
                    <$> (HashMap.lookup k b.footnoteOrder)
              )
            $ List.nub
              (HashMap.keys a.footnoteOrder <> HashMap.keys b.footnoteOrder)
      }

instance Monoid PageMetadata where
  mempty =
    PageMetadata
      { config = mempty
      , title = Nothing
      , description = mempty
      , toc = mempty
      , freshFeeds = mempty
      , underFeeds = mempty
      , wordCount = 0
      , footnoteOrder = mempty
      }

-- }}}
-- {{{ Page filters
getPageFilters ∷ Djot.Attr → PageFilters
getPageFilters attrs = PageFilters{dir = dir, hidden = hasClass "hidden" attrs}
 where
  dir =
    Text.unpack
      . fromMaybe (error "Missing directory name in rss filter")
      $ Djot.getAttr "dir" attrs

applyPageFilters ∷ PageFilters → FullPageTree → FullPageTree
applyPageFilters (PageFilters{dir, hidden}) forest =
  Tree.filterNodes (\p → hidden || not p.meta.config.hidden) forest
    & getSubtreeAt dir
    & Tree.onlyHeads

getSubtreeAt ∷ FilePath → FullPageTree → FullPageTree
getSubtreeAt (splitDirectories → path) (Tree.Forest forest) = do
  HashMap.foldMapWithKey go forest
 where
  go _ (Tree.Leaf _) = mempty
  go (splitDirectories → edge) (Tree.Node _ cs)
    | Just [] ← List.stripPrefix path edge = cs
    | Just (joinPath → path') ← List.stripPrefix edge path =
        getSubtreeAt path' cs
    | otherwise = mempty

-- }}}
