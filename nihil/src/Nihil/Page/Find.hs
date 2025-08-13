module Nihil.Page.Find
  ( InputPage (..)
  , findPages
  , findAssets
  ) where

import Data.Text qualified as Text
import Djot qualified as Djot
import Nihil.Config (Config (..))
import Nihil.File.In qualified as File
import Nihil.Route (Route (..))
import Relude
import System.FilePath (makeRelative, takeBaseName, takeExtension, (</>))

data InputPage
  = InputPage
  { path ∷ FilePath
  -- ^ Path to main .djot file
  , route ∷ Route
  -- ^ Where are we planning to place this?
  , content ∷ Text
  , contentBS ∷ ByteString
  , djot ∷ Djot.Doc
  }
  deriving (Show)

findPages ∷ Config → IO [InputPage]
findPages config = do
  File.run config.contentPath do
    index ← getPage "index" Home
    notFound ← getPage "404" NotFound
    echoes ← getPage "echoes" Echoes
    echoPages ← File.at "echoes" do
      entries ← File.ls
      foldMap toList <$> forM entries \case
        (_, path)
          | echoes.path /= path → do
              let base = takeBaseName path
              Just <$> getPage base (Echo $ Text.pack base)
          | otherwise → pure Nothing

    pure $ [index, notFound, echoes] <> echoPages

getPage ∷ String → Route → File.FileParser InputPage
getPage at route = do
  file ← File.at (at <> ".dj") $ File.getInfo
  dir ← File.at at $ File.getInfo

  case (file, dir) of
    ((File.File, path), _) → do
      contentBS ← readFileBS path
      let content = decodeUtf8 contentBS
      pure $
        InputPage
          { path = path
          , route = route
          , content = content
          , contentBS = contentBS
          , djot = parseDoc path contentBS
          }
    (_, (File.Directory, base)) → do
      let path = base </> "index.dj"
      contentBS ← readFileBS path
      let content = decodeUtf8 contentBS
      pure $
        InputPage
          { path = path
          , route = route
          , content = content
          , contentBS = contentBS
          , djot = parseDoc path contentBS
          }
    _ → error $ "Cannot find file for route `" <> show route <> "`."
 where
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

-- | Returns a list of assets, relative to the content dir.
findAssets ∷ FilePath → IO [(FilePath, FilePath)]
findAssets base = do
  File.run base do
    files ← File.tree
    foldMap toList <$> forM files \case
      (_, path)
        | takeExtension path == ".dj" → pure Nothing
        | otherwise → do
            pure $ Just $ (path, makeRelative base path)
