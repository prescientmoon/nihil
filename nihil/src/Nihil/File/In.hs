module Nihil.File.In
  ( FileParserT
  , FileParser
  , FileKind (..)
  , MonadFileExplorer (..)
  , at
  , atFile
  , atDirectory
  , collect
  , run
  ) where

import Data.Sequence qualified as Seq
import ListT (ListT, fromFoldable, toList)
import Relude
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

type FileParser = FileParserT IO
newtype FileParserT m a
  = FileParser (ListT (ReaderT Context m) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , Alternative
    , MonadPlus
    )

instance MonadTrans FileParserT where
  lift = FileParser . lift . lift

data Context = Context {relative ∷ FilePath, absolute ∷ FilePath}
data FileKind = File | Directory
  deriving (Show)

class (Monad m) ⇒ MonadFileExplorer m where
  absolutePath ∷ m FilePath

  -- | Returns the path relative to the base path the computation started at
  relativePath ∷ m FilePath

  -- | When a kind is given, the fiber will wither away if the file at the given
  -- path does not match said kind. The fiber is discarded if no file exists in
  -- the first place as well.
  atKinded ∷ ∀ a. Maybe FileKind → FilePath → m a → m a

  -- | Splits the execution into one fiber per file in the current directory.
  -- The fiber will get discarded if the given path is not a directory.
  ls ∷ m (FileKind, FilePath)

at ∷ ∀ a m. (MonadFileExplorer m) ⇒ FilePath → m a → m a
at = atKinded Nothing

atDirectory ∷ ∀ a m. (MonadFileExplorer m) ⇒ FilePath → m a → m a
atDirectory = atKinded (Just Directory)

atFile ∷ ∀ a m. (MonadFileExplorer m) ⇒ FilePath → m a → m a
atFile = atKinded (Just File)

instance (MonadIO m) ⇒ MonadFileExplorer (FileParserT m) where
  relativePath = FileParser $ asks \c → c.relative
  absolutePath = FileParser $ asks \c → c.absolute
  atKinded kind nested computation = local' updateContext do
    path ← absolutePath

    isDir ← liftIO $ doesDirectoryExist path
    isFile ← liftIO $ doesFileExist path

    case kind of
      Nothing → guard (isDir || isFile)
      Just Directory → guard isDir
      Just File → guard isFile

    computation
   where
    local' f (FileParser i) = FileParser $ local f i
    updateContext c =
      c
        { relative = c.relative </> nested
        , absolute = c.absolute </> nested
        }

  ls = do
    base ← absolutePath
    isDir ← liftIO $ doesDirectoryExist base
    guard isDir
    entries ← liftIO $ listDirectory base
    name ← FileParser $ ListT.fromFoldable entries
    at name do
      let path = base </> name
      isDir' ← liftIO $ doesDirectoryExist path
      if isDir'
        then pure (Directory, path)
        else pure (File, path)

instance (MonadFileExplorer m) ⇒ MonadFileExplorer (StateT s m) where
  relativePath = lift relativePath
  absolutePath = lift absolutePath
  atKinded k p (StateT f) = StateT \s → atKinded k p (f s)
  ls = lift ls

instance (MonadFileExplorer m) ⇒ MonadFileExplorer (ReaderT s m) where
  relativePath = lift relativePath
  absolutePath = lift absolutePath
  atKinded k p (ReaderT f) = ReaderT \c → atKinded k p (f c)
  ls = lift ls

collect ∷ ∀ m a. (Monad m) ⇒ FileParserT m a → FileParserT m [a]
collect (FileParser f) = FileParser . lift $ ListT.toList f

run ∷ ∀ m a. (MonadIO m) ⇒ Seq FilePath → FileParserT m a → m (Seq a)
run paths computation = do
  paths' ← forM paths \path → liftIO $ canonicalizePath path
  let (FileParser inner) = do
        path ← FileParser $ ListT.fromFoldable paths'
        at path $ local' noRelative $ computation
  list ← runReaderT (ListT.toList inner) $ Context{relative = mempty, absolute = mempty}
  pure $ Seq.fromList list
 where
  local' f (FileParser i) = FileParser $ local f i
  noRelative c = c{relative = mempty}
