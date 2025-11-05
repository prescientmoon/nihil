module Nihil.File.Out
  ( FileEntry (..)
  , DirEntry
  , FileGen
  , entry
  , file
  , fileBS
  , dir
  , copy
  , gen
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Relude
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

data FileEntry
  = File ByteString
  | Copy FilePath
  | Directory DirEntry

type DirEntry = (HashMap Text FileEntry)

newtype FileGen a
  = FileGen (StateT DirEntry IO a)
  deriving newtype (Functor, Applicative, Monad, MonadState DirEntry, MonadIO)

entry ∷ Text → FileEntry → FileGen ()
entry k v = modify $ HashMap.insert k v

file ∷ Text → Text → FileGen ()
file name content = entry name $ File $ encodeUtf8 content

fileBS ∷ Text → ByteString → FileGen ()
fileBS name content = entry name $ File content

copy ∷ Text → FilePath → FileGen ()
copy name content = entry name $ Copy content

dir ∷ ∀ a. Text → FileGen a → FileGen a
dir "" mkContents = mkContents
dir name mkContents = do
  pre ← get

  put mempty
  res ← mkContents
  contents ← get

  put $ HashMap.insert name (Directory contents) pre
  pure res

gen ∷ ∀ a m. (MonadIO m) ⇒ FilePath → FileGen a → m a
gen base (FileGen act) = do
  liftIO $ createDirectoryIfMissing True base

  (res, files) ← liftIO $ runStateT act mempty
  goDir base files

  pure res
 where
  goDir ∷ FilePath → DirEntry → m ()
  goDir path d = do
    for_ (HashMap.toList d) \(k, v) → do
      go (path </> Text.unpack k) v

  go ∷ FilePath → FileEntry → m ()
  go to (Copy from) = liftIO do
    createDirectoryIfMissing True $ takeDirectory to
    copyFile from to
  go to (File contents) = do
    liftIO $ writeFileBS to contents
  go to (Directory contents) = do
    liftIO $ createDirectoryIfMissing True to
    goDir to contents
