module Nihil.Page.Find
  ( InputPage (..)
  , InputPageTree
  , findPages
  ) where

import Data.Text qualified as Text
import Djot qualified as Djot
import Nihil.Config (Config (..))
import Nihil.File.In (FileParserT)
import Nihil.File.In qualified as File
import Nihil.Tree (TreeGenT)
import Nihil.Tree qualified as Tree
import Relude
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, stripExtension, takeFileName, (</>))

data InputPage
  = InputPage
  { path ∷ FilePath
  -- ^ Path to main .djot file
  , route ∷ FilePath
  -- ^ Where are we planning to place this?
  , content ∷ Text
  , contentBS ∷ ByteString
  , djot ∷ Djot.Doc
  }
  deriving (Show)

type InputPageTree = Tree.Forest InputPage FilePath FilePath

--------- Page finding
type PageFindingM = TreeGenT InputPage FilePath FilePath (FileParserT IO)

findPages ∷ Config → IO InputPageTree
findPages config = do
  forests ←
    File.run config.contentPaths $
      snd <$> Tree.run do
        getPage ""
  pure $ fold forests

getPages ∷ PageFindingM ()
getPages = do
  (kind, path) ← File.ls
  let filename = takeFileName path
  case kind of
    File.File
      | filename == "index.dj" → pure ()
      | Just stripped ← stripExtension "dj" filename →
          getPage stripped
      | otherwise → File.atFile filename do
          apath ← File.absolutePath
          rpath ← File.relativePath
          Tree.parent >>= \case
            Nothing → Tree.leaf rpath apath
            Just (_, page) → do
              Tree.leaf (makeRelative page.route rpath) apath
    File.Directory → getPage filename

getPage ∷ String → PageFindingM ()
getPage at = do
  rpath ← File.relativePath
  let route = rpath </> at
  edge ←
    Tree.parent <&> \case
      Nothing → ""
      Just (_, parent) → makeRelative parent.route (rpath </> at)

  asFile edge route <|> asDir edge route
 where
  page path edge route contentBS next = do
    Tree.node edge thePage next
   where
    thePage =
      InputPage
        { path = path
        , route = route
        , content = decodeUtf8 contentBS
        , contentBS = contentBS
        , djot = parseDoc path contentBS
        }

  asFile edge route = File.atFile (at <> ".dj") do
    path ← File.absolutePath
    contentBS ← liftIO $ readFileBS path
    page path edge route contentBS $ pure ()

  asDir edge route = File.atDirectory at do
    base ← File.absolutePath
    let path = base </> "index.dj"
    indexExists ← liftIO $ doesFileExist path
    if indexExists
      then do
        contentBS ← readFileBS path
        page path edge route contentBS getPages
      else getPages

  parseDoc path =
    either (handleParseError path) id
      . Djot.parseDoc (Djot.ParseOptions{sourcePositions = Djot.NoSourcePos})

  handleParseError path err =
    error . Text.pack . fold $
      [ "Error while parsing DJOT at `"
      , path
      , "`: "
      , err
      ]
