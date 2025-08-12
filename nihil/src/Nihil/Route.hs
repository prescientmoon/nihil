module Nihil.Route
  ( Route (..)
  , routeToPath
  , pathToRoute
  ) where

import Data.Text qualified as Text
import Relude

data Route
  = Home
  | NotFound
  | Echoes
  | Echo Text
  deriving (Show, Generic, Eq, Hashable)

-- | Convert a route to a relative file path (for url purposes).
routeToPath ∷ Route → Text
routeToPath Home = ""
routeToPath NotFound = "/404"
routeToPath Echoes = "/echoes"
routeToPath (Echo name) = "/echoes/" <> name

-- | Convert a relative file path (URL) to a Route.
pathToRoute ∷ Text → Route
pathToRoute "" = Home
pathToRoute "/404" = NotFound
pathToRoute "/echoes" = Echoes
pathToRoute path
  | Just name ← Text.stripPrefix "/echoes/" path
  , not (Text.null name) =
      Echo name
  | otherwise = error $ "Unrecognised route " <> path
