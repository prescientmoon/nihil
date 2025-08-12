module Nihil.Content.Config
  ( PageConfig (..)
  , SitemapConfig (..)
  , configCodec
  ) where

import Data.Time (UTCTime)
import Nihil.Toml qualified as Toml
import Relude
import Toml ((.=))
import Toml qualified as Toml

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
    <$> Toml.didefault False (Toml.bool "hidden") .= hidden
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
