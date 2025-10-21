module Nihil.File.In
  ( FileParser
  , FileKind (..)
  , at
  , currentPath
  , run
  , getInfo
  , ls
  , tree
  ) where

import Relude
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (isAbsolute, (</>))

newtype FileParser a
  = FileParser (ReaderT Context IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Context, MonadIO)

data Context
  = Context
  -- TODO: refactor this to hold a list of paths at every step
  { current ∷ FilePath
  }

at ∷ ∀ a. FilePath → FileParser a → FileParser a
at nested = local \c →
  c
    { current =
        if isAbsolute nested
          then nested
          else
            c.current </> nested
    }

currentPath ∷ FileParser FilePath
currentPath = asks \c → c.current

data FileKind = File | Directory | Other

ls ∷ FileParser [(FileKind, FilePath)]
ls = do
  base ← currentPath
  entries ← liftIO $ listDirectory base
  forM entries \name → do
    let path = base </> name
    isDir ← liftIO $ doesDirectoryExist path
    if isDir
      then return (Directory, path)
      else return (File, path)

run ∷ ∀ m a. (MonadIO m) ⇒ FilePath → FileParser a → m a
run base (FileParser inner) = do
  base' ← liftIO $ canonicalizePath base
  liftIO $ runReaderT inner $ Context{current = base'}

getInfo ∷ FileParser (FileKind, FilePath)
getInfo = do
  path ← currentPath

  isDir ← liftIO $ doesDirectoryExist path
  isFile ← liftIO $ doesFileExist path

  if
    | isDir → pure (Directory, path)
    | isFile → pure (File, path)
    | otherwise → pure (Other, path)

-- | Recursively calls `ls`, returning every file.
tree ∷ FileParser [(FileKind, FilePath)]
tree = do
  entries ← ls
  fold <$> forM entries \case
    (File, path) → pure [(File, path)]
    (Other, path) → pure [(Other, path)]
    (Directory, path) → at path tree
