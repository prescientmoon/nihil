module Nihil.Content.Rss (genRssFeed) where

import Data.Foldable1 (foldl1)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Nihil.Config (Config (..))
import Nihil.Context (Context (..))
import Nihil.Djot qualified as Djot
import Nihil.Gen.Xml qualified as Xml
import Nihil.Page.Find (InputPage (..))
import Nihil.Page.Meta
  ( FullPage (..)
  , Heading (..)
  , PageConfig (..)
  , PageMetadata (..)
  , RssFeed (..)
  )
import Nihil.State (PerPageState (..), pageStateFor)
import Relude
import System.FilePath ((</>))

genRssFeed ∷ Context → RssFeed → Text → Seq FullPage → Text
genRssFeed ctx feed url pages = Xml.genRaw do
  Xml.rawContent "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  Xml.tag "rss" do
    Xml.attr "version" "2.0"
    Xml.tag "channel" do
      Xml.attr "xmlns:atom" "http://www.w3.org/2005/Atom"
      Xml.tag "title" $ Xml.content $ "Moonythm | " <> feed.name
      Xml.tag "link" $ Xml.content ctx.config.baseUrl
      Xml.tag "description" $ Xml.content $ Djot.blocksToText feed.summary
      Xml.tag "language" "en"
      Xml.tag "webMaster" "hi@moonythm.dev (prescientmoon)"
      Xml.tag "generator" "nihil"

      Xml.singleTag "atom:link" do
        Xml.attr "href" url
        Xml.attr "rel" "self"
        Xml.attr "type" "application/rss+xml"

      let getLastUpdate p = lastUpdated (pageStateFor p.input.route ctx.state)
      let lastUpdate =
            foldl1 max
              . fmap getLastUpdate
              . fromMaybe (error "No pages found")
              . nonEmpty
              $ toList pages

      Xml.tag "lastBuildDate" $ goDatetime lastUpdate

      for_ pages \page → Xml.tag "item" do
        Xml.tag "author" "hi@moonythm.dev (prescientmoon)"
        Xml.tag "title" do
          let heading = fromMaybe (error "Article has no title") page.meta.title
          Xml.content $ Djot.inlinesToText heading.contents
        Xml.tag "description" do
          Xml.content $ Djot.blocksToText page.meta.description

        for_ page.meta.config.createdAt \createdAt → do
          Xml.tag "pubDate" $ goDatetime createdAt

        let link = Text.unpack ctx.config.baseUrl </> page.input.route
        Xml.tag "link" . Xml.content $ Text.pack link
        Xml.tag "guid" do
          Xml.attr "isPermaLink" "true"
          Xml.content $ Text.pack link
 where
  goDatetime =
    Xml.content
      . Text.pack
      . Time.formatTime Time.defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z"
      . Time.utcToZonedTime (Time.TimeZone 0 False "GMT")
