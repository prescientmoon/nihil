module Main where

import Data.Sequence qualified as Seq
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Nihil.Config (Config (..), getConfig)
import Nihil.Content.Html qualified as Html
import Nihil.Context (Context (..))
import Nihil.File.Out qualified as Gen
import Nihil.Page.Find (findAssets, findPages)
import Nihil.Page.Meta (FullPage (..), PageConfig (..), PageMetadata (..), elabPage)
import Nihil.State (conjureState)
import Relude
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

main ∷ IO ()
main = do
  -- Force UTF-8 for all text I/O
  -- (otherwise, I get nix sandboxing errors)
  setLocaleEncoding utf8

  cfg ← getConfig
  pages ← findPages cfg
  persistent ← conjureState cfg pages

  let ctx =
        Context
          { pages =
              Seq.fromList pages
                <&> elabPage
                & Seq.filter (\page → cfg.includeDrafts || not page.meta.config.draft)
          , config = cfg
          , state = persistent
          }

  Gen.gen cfg.outPath do
    Gen.dir "web" do
      Html.genSite ctx

  assets ← foldMapM findAssets cfg.contentPaths
  for_ assets \(from, to) → do
    createDirectoryIfMissing True (cfg.outPath </> "web" </> takeDirectory to)
    copyFile from (cfg.outPath </> "web" </> to)
