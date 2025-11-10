{-# LANGUAGE TemplateHaskell #-}

module Nihil.Content.Html (genSite) where

import Data.FileEmbed (embedStringFile)
import Data.HashMap.Strict qualified as HashMap
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Djot qualified
import Nihil.Config (Config (..))
import Nihil.Content.Rss (genRssFeed)
import Nihil.Content.Sitemap (genSitemap)
import Nihil.Context (Context (..))
import Nihil.Djot qualified as Djot
import Nihil.File.In qualified as File
import Nihil.File.Out (FileGen)
import Nihil.File.Out qualified as Gen
import Nihil.Gen.Xml qualified as Html
import Nihil.Highlight (highlight)
import Nihil.Math (renderMath)
import Nihil.Page.Find (InputPage (..))
import Nihil.Page.Meta
  ( FullPage (..)
  , FullPageTree
  , Heading (..)
  , PageConfig (..)
  , PageMetadata (..)
  , RssFeed (..)
  , applyPageFilters
  , getPageFilters
  )
import Nihil.State
  ( GitChange (..)
  , PerPageState (..)
  , pageStateFor
  )
import Nihil.Tree qualified as Tree
import Relude
import System.FilePath (takeFileName, (</>))

genSite ∷ Context → FileGen ()
genSite ctx = do
  Gen.dir "fonts" do
    -- Computer modern fonts
    Gen.copy "cm-regular.otf" $
      ctx.config.cmodernPath </> "cmunss.otf"
    Gen.copy "cm-bold.otf" $
      ctx.config.cmodernPath </> "cmunsx.otf"
    Gen.copy "cm-italic.otf" $
      ctx.config.cmodernPath </> "cmunsi.otf"
    Gen.copy "cm-bolditalic.otf" $
      ctx.config.cmodernPath </> "cmunso.otf"
    Gen.copy "cm-math.otf" $
      ctx.config.cmodernPath </> "cmunbmo.otf"

  -- pulldown_latex assets
  Gen.dir "math" do
    multiple ← File.run (pure ctx.config.pulldownLatexAssetPath) do
      File.atDirectory "share" do
        files ← File.atDirectory "fonts/woff2" $ File.collect File.ls
        let fonts = files <&> snd
        stylesheet ← File.atFile "css/styles.css" File.absolutePath
        pure (fonts, stylesheet)

    let (fonts, stylesheet) =
          head
            . fromMaybe (error "font finder diverged / couldn't find the files")
            . nonEmpty
            $ toList multiple

    Gen.copy "styles.css" stylesheet
    Gen.dir "font" do
      for_ fonts \font → do
        Gen.copy (Text.pack $ takeFileName font) font

  Gen.file "sitemap.xml" $ genSitemap ctx

  genForest ctx ctx.pages

genForest ∷ Context → FullPageTree → FileGen ()
genForest ctx (Tree.Forest cs) = do
  for_ (HashMap.toList cs) \(edge, node) → genTree edge node
 where
  genTree edge (Tree.Leaf path) = do
    Gen.copy (Text.pack edge) path
  genTree edge (Tree.Node page forest) = do
    Gen.dir (Text.pack edge) do
      genPage ctx page
      genForest ctx forest

genPage ∷ Context → FullPage → FileGen ()
genPage ctx page = do
  let template = Text.pack $(embedStringFile "./templates/page.html")

  for_ page.meta.freshFeeds \rss → do
    Gen.file (Text.pack $ takeFileName rss.at)
      . genRssFeed ctx rss (fold [ctx.config.baseUrl, "/", Text.pack rss.at])
      . Tree.nodes
      $ applyPageFilters rss.filters ctx.pages

  Gen.fileBS "index.dj" do
    page.input.contentBS

  Gen.file "index.html" do
    template
      & goMetadata page
      & replaceHtml "{{content}}" mainContent
      & replaceHtml "{{epilogue}}" mainEpilogue

  Gen.dir "changelog" $ Gen.file "index.html" do
    let desc = goMetadata page "Chronicling the history of \"{{text_title}}\"."

    template
      & replaceHtml "{{content}}" changelogContent
      & replaceHtml "{{epilogue}}" ""
      & replaceHtml "{{text_description}}" (Html.content desc)
      . replaceHtml "{{meta}}" extraMeta
      & Text.replace "{{text_title}}" "Scroll of alterations"
      & Text.replace "{{url}}" (url <> "/changelog/")
 where
  mainContent = goBlocks page.input.djot.docBlocks
  mainEpilogue
    | HashMap.null page.meta.footnoteOrder = pure ()
    | otherwise = do
        let noteMap = page.input.djot.docFootnotes
        let orderedKeys = sortOn snd $ HashMap.toList page.meta.footnoteOrder
        Html.singleTag "hr" $ pure ()
        Html.tag "div" do
          -- Using <section> here make VNU complain
          Html.attr "role" "doc-endnotes"
          Html.tag "ol" do
            for_ orderedKeys \(key, num) → do
              Html.tag "li" do
                Html.attr "id" $ "footnote-" <> show num
                case Djot.lookupNote (encodeUtf8 key) noteMap of
                  Just blocks → goBlocks blocks
                  Nothing →
                    error $ "Footnote definition for " <> key <> " not found!"

                Html.tag "a" do
                  Html.attr "href" $ "#footnote-reference-" <> show num
                  Html.attr "role" "doc-backlink"
                  Html.content "Return to content ↩︎"

              pure ()

  changelogContent = Html.tag "main" do
    Html.attr "aria-labelledby" "changelog"
    goAnchoredHeading "changelog" 1
      . Djot.Many
      . pure
      . Djot.Node Djot.NoPos mempty
      $ Djot.Str "Changelog"

    Html.tag "p" do
      Html.content "This list does not currently contain the git diffs. Sowwy ;-;"

    if
      | null pageState.changes → do
          Html.tag "blockquote" $ Html.tag "p" do
            Html.tag "em" do
              Html.content "The page manifests in front of your eyes, yet its contents are nowhere to be seen. None can tell the peculiar accident that lead to its complete annihilation. "
            Html.content "(read: I likely messed up one of my scripts....)"
      | otherwise → Html.tag "ol" do
          Html.attr "class" "change-list"
          Html.valuelessAttr "reversed"
          for_ (Seq.reverse pageState.changes) \change → do
            Html.tag "li" do
              Html.content "At commit "
              Html.tag "code" $ Html.content change.hash
              Html.content " on "
              goDatetime change.at
              Html.content ":"
              Html.tag "blockquote" $ Html.tag "em" do
                Html.content change.message
                when (change.body /= "") do
                  Html.singleTag "br" $ pure ()
                  Html.singleTag "br" $ pure ()
                  Html.content change.body

    Html.tag "p" do
      Html.tag "a" do
        Html.attr "href" $ url <> "/"
        Html.content "↩ Back to content"

  url = Text.pack $ Text.unpack ctx.config.baseUrl </> page.input.route
  pageState = pageStateFor page.input.route ctx.state

  goBlocks (Djot.Many blocks) = traverse_ goBlock blocks
  goBlock (Djot.Node _ attrs inner) = case inner of
    Djot.Section (Djot.Many blocks)
      | Just sectionId == titleId && page.meta.config.compact → Html.tag "main" do
          Html.attr "aria-labelledby" sectionId
          renderTitle
          renderNonTitle
          Html.rawContent "{{epilogue}}"
      | Just sectionId == titleId && not page.meta.config.compact →
          Html.tag "main" $ Html.tag "article" do
            Html.attr "aria-labelledby" sectionId
            let template = Text.pack $(embedStringFile "./templates/post.html")
            template
              & goMetadata page
              & replaceHtml "{{heading}}" renderTitle
              & replaceHtml "{{content}}" renderNonTitle
              & Html.rawContent
      | otherwise → Html.tag "section" do
          Html.attr "aria-labelledby" sectionId
          renderTitle
          renderNonTitle
     where
      titleId = fmap (\x → x.id) page.meta.title
      sectionId =
        fromMaybe (error "Sections must have an ID.") $
          Djot.getAttr "id" attrs

      renderTitle = for_ blocks \case
        Djot.Node _ _ (Djot.Heading level inlines) → do
          goAnchoredHeading sectionId level inlines
        _ → pure ()

      renderNonTitle = for_ blocks \case
        Djot.Node _ _ (Djot.Heading _ _) → pure ()
        other → goBlock other
    Djot.Heading level inlines → Html.tag ("h" <> show level) do
      goInlines inlines
    Djot.Para inlines → Html.tag "p" $ goInlines inlines
    Djot.CodeBlock lang content → goCode lang content
    Djot.BlockQuote blocks → Html.tag "blockquote" $ goBlocks blocks
    Djot.Div blocks
      | Djot.hasClass "comment" attrs → pure ()
      | Djot.hasClass "rss" attrs → pure ()
      | Djot.hasClass "description" attrs → pure ()
      | Djot.hasClass "figure" attrs → Html.tag "figure" $ goBlocks blocks
      | Djot.hasClass "caption" attrs → Html.tag "figcaption" $ goBlocks blocks
      | Djot.hasClass "image-figure" attrs → Html.tag "figure" do
          Html.singleTag "img" do
            Html.attr "src"
              . fromMaybe (error "image-figure must have a source attibute")
              $ Djot.getAttr "src" attrs
            Html.attr "alt"
              . fromMaybe (error "image-figure must have an alt attibute")
              $ Djot.getAttr "alt" attrs
            for_ (Djot.getAttr "width" attrs) \w → do
              -- the width is not always an int!
              if
                | isJust (readMaybe @Int $ Text.unpack w) →
                    Html.attr "width" w
                | otherwise → do
                    Html.attr "style" $ "width: " <> w
          Html.tag "figcaption" do
            goBlocks blocks
      | Djot.hasClass "embed-description" attrs → do
          goBlocks page.meta.description
      | Djot.hasClass "toc" attrs → do
          let template = Text.pack $(embedStringFile "./templates/table-of-contents.html")
          template
            & replaceHtml
              "{{content}}"
              (goToc [2] page.meta.toc)
            & Html.rawContent
      | Djot.hasClass "page-index" attrs → do
          let pages =
                ctx.pages
                  & applyPageFilters (getPageFilters attrs)
                  & Tree.nodes
                  & Seq.sortOn
                    ( \p → case p.meta.config.createdAt of
                        Just at → (0 ∷ Int, Just at)
                        Nothing → (1, Nothing)
                    )
                  & Seq.reverse

          let template = Text.pack $(embedStringFile "./templates/post-summary.html")
          Html.tag "ol" do
            Html.attr "class" "article-list"
            for_ pages \page' → do
              template
                & goMetadata page'
                & Html.rawContent
      | Djot.hasClass "aside" attrs
          || Djot.hasClass "long-aside" attrs
          || Djot.hasClass "char-aside" attrs → do
          let template
                | Djot.hasClass "aside" attrs = Text.pack $(embedStringFile "./templates/aside.html")
                | Djot.hasClass "long-aside" attrs = Text.pack $(embedStringFile "./templates/long-aside.html")
                | Djot.hasClass "char-aside" attrs = Text.pack $(embedStringFile "./templates/char-aside.html")
                | otherwise = error "Impossible"

          let asideId =
                fromMaybe (error "No ID found on non-simple aside") $
                  Djot.getAttr "id" attrs
          let asideTitle =
                fromMaybe (error $ "No title found on non-simple aside " <> asideId) $
                  Djot.getAttr "title" attrs
          let asideChar =
                fromMaybe (error $ "No character found for non-simple aside " <> asideId) $
                  Djot.getAttr "character" attrs

          let notSimple = not $ Djot.hasClass "aside" attrs
          template
            & endoIf notSimple (Text.replace "{{id}}" asideId)
            & endoIf notSimple (Text.replace "{{title}}" asideTitle)
            & endoIf notSimple (Text.replace "{{character}}" asideChar)
            & replaceHtml "{{content}}" (goBlocks blocks)
            & Html.rawContent
      | otherwise → Html.tag "div" $ goBlocks blocks
     where
      -- {{{ TOC algorithm
      -- Sometimes we can have TOCs that look like this:
      -- # foo
      --   ## bar
      --     ### goo
      -- # help
      --
      -- In this case, we need to close two different sublists when
      -- going from ### goo to # help. To achieve this, we use this
      -- vec as a stack of all the different levels we are yet to
      -- close out.
      --
      -- Note that the list for the initial level is included in the
      -- template, as it would never get opened/closed out otherwise
      -- (there can be no level smaller than 1).
      goToc ∷ [Int] → Seq Heading → Html.HtmlGen ()
      goToc _ Seq.Empty = pure ()
      goToc stack (heading :<| rest)
        | heading.level == 1 = goToc stack rest
        | otherwise = do
            Html.rawContent "<li>"
            Html.tag "a" do
              Html.attr "href" $ "#" <> heading.id
              goInlines $ Djot.inlinesWithoutLinks heading.contents

            let nextLevel = fromMaybe 2 do
                  asList ← nonEmpty $ toList rest
                  let nextHeading = head asList
                  pure nextHeading.level

            case compare heading.level nextLevel of
              EQ → do
                Html.rawContent "</li>" -- close this level
                goToc stack rest
              LT → do
                Html.rawContent "<ol>" -- go deeper
                goToc (nextLevel : stack) rest
              GT → do
                Html.rawContent "</li>"
                go stack
               where
                -- Close the level, then keep closing further up the stack.
                go (h : stack') | h > nextLevel = do
                  Html.rawContent "</ol></li>"
                  go stack'
                go other = goToc other rest
    -- }}}
    Djot.OrderedList listAttrs listSpacing items → Html.tag "ol" do
      let start = Djot.orderedListStart listAttrs
      let typ = case Djot.orderedListStyle listAttrs of
            Djot.Decimal → "1"
            Djot.LetterUpper → "A"
            Djot.LetterLower → "a"
            Djot.RomanUpper → "I"
            Djot.RomanLower → "i"

      when (start /= 1) $ Html.attr "start" $ show start
      when (typ /= "1") $ Html.attr "type" typ
      for_ items \item →
        Html.tag "li" $ goListItemBlocks listSpacing item
    Djot.BulletList listSpacing items → Html.tag "ul" do
      for_ items \item →
        Html.tag "li" $ goListItemBlocks listSpacing item
    Djot.DefinitionList listSpacing items → Html.tag "dl" do
      for_ items \(term, def) → do
        Html.tag "dt" $ goInlines term
        Html.tag "dd" $ goListItemBlocks listSpacing def
    Djot.TaskList _ _ → error "Task lists are not supported"
    Djot.ThematicBreak → Html.singleTag "hr" $ pure ()
    Djot.Table _ _ → error "Tables are not currently supported"
    Djot.RawBlock (Djot.Format "html") (decodeUtf8 → s) → Html.rawContent s
    Djot.RawBlock _ _ → pure ()

  goListItemBlocks listSpacing (Djot.Many blocks) =
    traverse_ (goListItemBlock listSpacing) blocks

  goListItemBlock listSpacing (Djot.Node _ _ (Djot.Para inlines))
    | listSpacing == Djot.Tight = goInlines inlines
  goListItemBlock _ block = goBlock block

  goInlines (Djot.Many inlines) = traverse_ goInline inlines
  goInline (Djot.Node pos attrs inner) = case inner of
    Djot.Str (decodeUtf8 → s) → Html.content s
    Djot.Emph inlines → Html.tag "em" $ goInlines inlines
    Djot.Strong inlines → Html.tag "strong" $ goInlines inlines
    Djot.Highlight inlines → Html.tag "mark" $ goInlines inlines
    Djot.Insert inlines → Html.tag "ins" $ goInlines inlines
    Djot.Delete inlines → Html.tag "del" $ goInlines inlines
    Djot.Superscript inlines → Html.tag "sup" $ goInlines inlines
    Djot.Subscript inlines → Html.tag "sub" $ goInlines inlines
    Djot.Verbatim (decodeUtf8 → s) → Html.tag "code" $ Html.content s
    Djot.Symbol (decodeUtf8 → sym) → Html.content $ ":" <> sym <> ":"
    Djot.SoftBreak → Html.content "\n"
    Djot.HardBreak → Html.singleTag "br" $ pure ()
    Djot.NonBreakingSpace → Html.content "&nbsp"
    Djot.RawInline (Djot.Format "html") s → Html.rawContent $ decodeUtf8 s
    Djot.RawInline _ _ → pure ()
    Djot.Quoted Djot.SingleQuotes ils → do
      Html.content "‘"
      goInlines ils
      Html.content "’"
    Djot.Quoted Djot.DoubleQuotes ils → do
      Html.content "“"
      goInlines ils
      Html.content "”"
    Djot.Span inlines
      -- {{{ Date/Datetime
      | Djot.hasClass "datetime" attrs → do
          let t = Djot.inlinesToText inlines
          let fmt = Time.iso8601Format
          case Time.formatParseM fmt $ Text.unpack t of
            Nothing → error $ "Invalid datetime `" <> t <> "`"
            Just (datetime ∷ Time.UTCTime) → goDatetime datetime
      | Djot.hasClass "date" attrs → do
          let t = Djot.inlinesToText inlines
          let fmt = Time.calendarFormat Time.ExtendedFormat
          case Time.formatParseM fmt $ Text.unpack t of
            Nothing → error $ "Invalid date `" <> t <> "`"
            Just (date ∷ Time.Day) → Html.tag "time" do
              Html.attr "datetime" . Text.pack $
                Time.formatShow
                  Time.iso8601Format
                  date
              Html.content . Text.pack $
                Time.formatTime
                  Time.defaultTimeLocale
                  "%a, %d %b %Y"
                  date
      -- }}}
      | otherwise → Html.tag "span" $ goInlines inlines
    Djot.Image inlines target → Html.singleTag "img" do
      Html.attr "alt" $ Djot.inlinesToText inlines
      Html.attr "src" $ fst $ getReference page.input.djot target
    Djot.EmailLink mail →
      goInline
        . Djot.Node pos attrs
        . Djot.Link (Djot.str mail)
        . Djot.Direct
        $ "mailto:" <> mail
    Djot.UrlLink target →
      goInline
        . Djot.Node pos attrs
        . Djot.Link (Djot.str target)
        $ Djot.Direct target
    Djot.Link inlines target → Html.tag "a" do
      Html.attr "href" $ fst $ getReference page.input.djot target
      goInlines inlines
    -- Djot.Math _ _ → error "Math not implemented!"
    Djot.Math display (decodeUtf8 → string) → do
      Html.rawContent $ renderMath display string
    -- TODO: encode `to` as a valid ID?
    Djot.FootnoteReference (decodeUtf8 → to) → do
      let num =
            fromMaybe (error $ "Footnote " <> to <> " not found in order") $
              HashMap.lookup to page.meta.footnoteOrder

      Html.tag "sup" $ Html.tag "a" do
        Html.attr "role" "doc-noteref"
        Html.attr "id" $ "footnote-reference-" <> show num
        Html.attr "href" $ "#footnote-" <> show num
        Html.content $ show num

  goDatetime ∷ Time.UTCTime → Html.HtmlGen ()
  goDatetime datetime = Html.tag "time" do
    Html.attr "datetime" . Text.pack $
      Time.formatShow
        Time.iso8601Format
        datetime
    Html.content . Text.pack $
      Time.formatTime
        Time.defaultTimeLocale
        "%a, %d %b %Y"
        datetime

  goCode ∷ ByteString → ByteString → Html.HtmlGen ()
  goCode (decodeUtf8 → lang) (decodeUtf8 → content) =
    Html.tag "pre" $ Html.tag "code" do
      Html.attr "data-language" lang
      Html.rawContent $ highlight lang content

  goAnchoredHeading ∷ Text → Int → Djot.Inlines → Html.HtmlGen ()
  goAnchoredHeading sectionId level inlines = Html.tag ("h" <> show level) do
    Html.attr "id" sectionId
    Html.tag "a" do
      Html.attr "class" "heading-anchor"
      Html.attr "href" $ "#" <> sectionId
      Html.content "◇"
    Html.content " "
    goInlines inlines

  goMetadata page' =
    replaceHtml "{{title}}" title
      . replaceHtml "{{text_title}}" (Html.content textTitle)
      . replaceHtml "{{description}}" (goBlocks page'.meta.description)
      . replaceHtml "{{text_description}}" (Html.content textDescription)
      . replaceHtml "{{posted_on}}" postedOn
      . replaceHtml "{{updated_on}}" (goDatetime pageState'.lastUpdated)
      . replaceHtml "{{meta}}" extraMeta
      . Text.replace "{{changelog_url}}" (url' <> "/changelog/")
      . Text.replace "{{source_url}}" (url' <> "/index.dj")
      . Text.replace "{{url}}" (url' <> "/")
      . Text.replace "{{base_url}}" ctx.config.baseUrl
      . Text.replace "{{word_count}}" (showWordCount page')
      . Text.replace "{{reading_duration}}" (showReadingDuration page')
   where
    title = case page'.meta.title of
      Just heading → goInlines heading.contents
      Nothing → Html.content "???"
    textTitle = case page'.meta.title of
      Just heading → Djot.inlinesToText heading.contents
      Nothing → "???"
    textDescription = Djot.blocksToText page'.meta.description
    url' = Text.pack $ Text.unpack ctx.config.baseUrl </> page'.input.route
    pageState' = pageStateFor page'.input.route ctx.state
    postedOn = case page'.meta.config.createdAt of
      Nothing → Html.content "Being conjured "
      Just at → do
        Html.content "Posted on "
        goDatetime at

  extraMeta = do
    for_ page.meta.underFeeds \feed → do
      Html.singleTag "link" do
        Html.attr "rel" "alternate"
        Html.attr "type" "application/rss+xml"
        Html.attr "title" feed.name
        Html.attr "href" $ Text.pack feed.at

getReference ∷ Djot.Doc → Djot.Target → (Text, Djot.Attr)
getReference _ (Djot.Direct b) = (decodeUtf8 b, mempty)
getReference doc (Djot.Reference label)
  | Just (u, as) ← Djot.lookupReference label refs = (decodeUtf8 u, as)
  | otherwise = error $ "Link definition for " <> decodeUtf8 label <> " not found."
 where
  refs =
    Djot.docReferences doc
      <> Djot.docAutoReferences doc

replaceHtml ∷ Text → Html.HtmlGen () → (Text → Text)
replaceHtml k v = Text.replace k $ Html.genRaw v

showWordCount ∷ FullPage → Text
showWordCount page
  | wc < 400 = show wc
  | wc < 1000 = show $ wc `div` 10 * 10
  | wc < 2000 = show $ wc `div` 100 * 100
  | otherwise = show (wc `div` 1000) <> " thousand"
 where
  wc = page.meta.wordCount

showReadingDuration ∷ FullPage → Text
showReadingDuration page
  | minutes == 0 = "very short " <> show seconds <> " second"
  | minutes < 10 = "short " <> show minutes <> " minute"
  | minutes < 20 = "somewhat short " <> show minutes <> " minute"
  | minutes < 30 = "somewhat long " <> show minutes <> " minute"
  | minutes < 60 = "long " <> show minutes <> " minute"
  | otherwise =
      "very long "
        <> show hours
        <> " and "
        <> show (minutes `mod` 60)
        <> " minute"
 where
  hours = minutes `div` 60
  minutes = wc `div` 200
  seconds = wc * 60 `div` 200
  wc = page.meta.wordCount

endoIf ∷ ∀ a. Bool → (a → a) → (a → a)
endoIf True e = e
endoIf False _ = Relude.id
