module Nihil.Content.Sitemap (genSitemap) where

import Data.Text qualified as Text
import Data.Time.Format.ISO8601 qualified as Time
import Nihil.Config (Config (..))
import Nihil.Context (Context (..))
import Nihil.Gen.Xml qualified as Xml
import Nihil.Page.Find (InputPage (..))
import Nihil.Page.Meta
  ( FullPage (..)
  , PageConfig (..)
  , PageMetadata (..)
  , SitemapConfig (..)
  )
import Nihil.State (PerPageState (..), pageStateFor)
import Nihil.Tree qualified as Tree
import Relude
import System.FilePath ((</>))

genSitemap ∷ Context → Text
genSitemap ctx = Xml.genRaw do
  Xml.rawContent "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  Xml.tag "urlset" do
    Xml.attr "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9"
    for_ (Tree.nodes ctx.pages) \page → do
      unless (page.meta.config.hidden || page.meta.config.sitemap.exclude) do
        let pageState = pageStateFor page.input.route ctx.state

        Xml.tag "url" do
          Xml.tag "loc" . Xml.content . Text.pack $
            Text.unpack ctx.config.baseUrl </> page.input.route

          Xml.tag "lastmod" . Xml.content . Text.pack . fold $
            [ Time.formatShow
                Time.iso8601Format
                pageState.lastUpdated
            ]

          for_ page.meta.config.sitemap.priority \priority → do
            Xml.tag "priority" . Xml.content $ show priority

          for_ page.meta.config.sitemap.changefreq \changefreq → do
            Xml.tag "changefreq" $ Xml.content changefreq
