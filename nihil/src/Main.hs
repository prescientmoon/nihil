module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Nihil.Config (Config (..), getConfig)
import Nihil.Content.Html qualified as Html
import Nihil.Context (Context (..))
import Nihil.File.Out qualified as Gen
import Nihil.Page.Find (findPages)
import Nihil.Page.Meta (elabForest)
import Nihil.State (conjureState)
import Nihil.Tree qualified as Tree
import Relude

main ∷ IO ()
main = do
  -- Force UTF-8 for all text I/O
  -- (otherwise, I get nix sandboxing errors)
  setLocaleEncoding utf8

  cfg ← getConfig
  pages ← findPages cfg
  persistent ← conjureState cfg pages

  let fullPages = elabForest cfg pages
  let ctx =
        Context
          { pages = fullPages
          , config = cfg
          , state = persistent
          }

  Gen.gen cfg.outPath do
    Gen.dir "web" do
      Html.genSite ctx

  putTextLn $ "Processed " <> show (length $ Tree.nodes fullPages) <> " pages."
 where

-- showForest ∷ FullPageTree → IO ()
-- showForest (Tree.Forest hm) = do
--   when (not $ null hm) do
--     putTextLn "Starting forest"
--     for_ (HashMap.toList hm) \(edge, tree) → showTree edge tree
--     putTextLn "Ending forest"
-- showTree edge (Tree.Leaf leaf) = do
--   putTextLn $ "Asset " <> Text.pack edge <> ": " <> show leaf
--   pure ()
-- showTree edge (Tree.Node (n ∷ FullPage) cs) = do
--   putTextLn $ "Page " <> show (Text.pack edge) <> ": " <> show (Text.pack n.input.route)
--   showForest cs
