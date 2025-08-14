module Nihil.State
  ( PersistentState (..)
  , PerPageState (..)
  , GitChange (..)
  , conjureState
  , pageStateFor
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Nihil.Config (Config (..))
import Nihil.Page.Find (InputPage (..))
import Nihil.Route (Route, pathToRoute, routeToPath)
import Nihil.Toml qualified as Toml
import Relude
import System.FilePath (takeDirectory)
import System.Process qualified as Process
import Toml ((.=))
import Toml qualified as Toml

data PersistentState
  = PersistentState
  { pages ∷ HashMap Route PerPageState
  }
  deriving (Show, Generic)

instance Semigroup PersistentState where
  a <> b = PersistentState{pages = a.pages <> b.pages}
instance Monoid PersistentState where
  mempty = PersistentState{pages = mempty}

persistentState ∷ Toml.TomlCodec PersistentState
persistentState = do
  PersistentState
    <$> Toml.dimap
      (Seq.fromList . HashMap.toList)
      (HashMap.fromList . toList)
      (Toml.seq' perPageState "pages")
      .= pages

data PerPageState
  = PerPageState
  { lastUpdated ∷ UTCTime
  , changes ∷ Seq GitChange
  }
  deriving (Show, Generic)

instance Semigroup PerPageState where
  a <> b =
    PerPageState
      { lastUpdated = max a.lastUpdated b.lastUpdated
      , changes = a.changes <> b.changes
      }

perPageState ∷ Toml.TomlCodec (Route, PerPageState)
perPageState = do
  (\route lastUpdated changes → (route, PerPageState lastUpdated changes))
    <$> Toml.dimap routeToPath pathToRoute (Toml.text "route") .= fst
    <*> Toml.utcTime "lastUpdated" .= lastUpdated . snd
    <*> Toml.seq' gitChangeCodec "changes" .= changes . snd

data GitChange
  = GitChange
  { hash ∷ Text
  , at ∷ UTCTime
  , message ∷ Text
  }
  deriving (Show, Generic)

gitChangeCodec ∷ Toml.TomlCodec GitChange
gitChangeCodec = do
  GitChange
    <$> Toml.text "hash" .= hash
    <*> Toml.utcTime "at" .= at
    <*> Toml.text "message" .= message

---------- Reading the state
conjureState ∷ Config → [InputPage] → IO PersistentState
conjureState cfg pages
  -- Gen state, and write it to the given path
  | cfg.mutateState = do
      pageStates ← forM pages \page → do
        st ← genStateFor page
        pure (page.route, st)
      let res = PersistentState{pages = HashMap.fromList pageStates}

      void $ Toml.encodeToFile persistentState cfg.statePath res
      pure res

  -- Read state from the given path
  | otherwise = Toml.decodeFile persistentState cfg.statePath

genStateFor ∷ InputPage → IO PerPageState
genStateFor page = do
  let process =
        Process.proc
          "git"
          [ "-C"
          , takeDirectory page.path
          , "log"
          , "--pretty=format:%h %ad %s"
          , "--date=iso-strict"
          , "--follow"
          , "--"
          , page.path
          ]

  output ← Process.readCreateProcess process "" -- no stdin
  changes ←
    output
      & Text.pack
      & Text.lines
      & Seq.fromList
      & traverse \(cut → (hash, cut → (rawDate, message))) → do
        let err = error $ "Cannot parse date returned by git: " <> rawDate
        pure $
          GitChange
            { hash = hash
            , message = message
            , at =
                Time.zonedTimeToUTC
                  . fromMaybe err
                  . Time.formatParseM Time.iso8601Format
                  $ Text.unpack rawDate
            }

  lastUpdated ←
    maybe Time.getCurrentTime pure
      . fmap (at . head)
      . nonEmpty
      $ toList changes

  pure $
    PerPageState
      { lastUpdated = lastUpdated
      , changes = Seq.reverse changes
      }
 where
  -- Cut some text into the chunk before the first space, and the rest.
  cut =
    second (\t → fromMaybe t $ Text.stripPrefix " " t)
      . Text.breakOn " "

-- | Selects the state for a particular page. Errors out if the state
-- cannot be found.
pageStateFor ∷ Route → PersistentState → PerPageState
pageStateFor route st =
  fromMaybe
    (error $ "No persistent state for route " <> show route)
    $ HashMap.lookup route st.pages
