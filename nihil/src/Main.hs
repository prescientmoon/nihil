module Main where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Nihil.Config (Config (..), getConfig)
import Nihil.Content.Html qualified as Html
import Nihil.Context (Context (..))
import Nihil.File.Out qualified as Gen
import Nihil.Page.Find (InputPage (..), InputPageTree, findPages)
import Nihil.Page.Meta (FullPage (..), PageConfig (..), PageMetadata (..), elabPage)
import Nihil.State (conjureState)
import Nihil.Tree qualified as Tree
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
              pages
                & Tree.mapNodes elabPage
                & Tree.filterNodes
                  (\page → cfg.includeDrafts || not page.meta.config.draft)
          , config = cfg
          , state = persistent
          }

  Gen.gen cfg.outPath do
    Gen.dir "web" do
      Html.genSite ctx

  putTextLn "Pages:"
  showForest pages
  putTextLn "Done!"
 where
  showForest ∷ InputPageTree → IO ()
  showForest (Tree.Forest hm) = do
    for_ (HashMap.toList hm) \(edge, tree) → showTree edge tree
  showTree edge (Tree.Leaf leaf) = do
    putTextLn $ "Asset " <> Text.pack edge <> ": " <> show leaf
  showTree _ (Tree.Node (n ∷ InputPage) cs) = do
    putTextLn $ "Page " <> Text.pack n.route
    showForest cs
