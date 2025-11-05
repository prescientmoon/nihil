module Nihil.Tree
  ( Tree (..)
  , Forest (..)
  , MonadTreeGen (..)
  , TreeGenT
  , run
  , mapNodes
  , filterNodes
  , nodes
  ) where

import Data.HashMap.Strict qualified as HashMap
import Data.Sequence ((<|))
import Data.Sequence qualified as Seq
import Nihil.File.In (MonadFileExplorer)
import Relude

data Tree node edge leaf
  = Node node (Forest node edge leaf)
  | Leaf leaf
  deriving (Functor, Show)

newtype Forest node edge leaf
  = Forest (HashMap edge (Tree node edge leaf))
  deriving (Functor, Show)

mapNodes ∷ ∀ a b edge leaf. (a → b) → Forest a edge leaf → Forest b edge leaf
mapNodes f (Forest hm) = Forest $ go <$> hm
 where
  go (Node n forest) = Node (f n) (mapNodes f forest)
  go (Leaf l) = Leaf l

filterNodes ∷ ∀ node edge leaf. (node → Bool) → Forest node edge leaf → Forest node edge leaf
filterNodes f (Forest hm) = Forest $ HashMap.mapMaybe go hm
 where
  go (Node n forest)
    | f n = Just $ Node n (filterNodes f forest)
    | otherwise = Nothing
  go (Leaf l) = Just $ Leaf l

nodes ∷ ∀ node edge leaf. Forest node edge leaf → Seq node
nodes (Forest hm) = do
  n ← Seq.fromList $ toList hm
  flattenNode n
 where
  flattenNode (Leaf _) = mempty
  flattenNode (Node n cs) = n <| nodes cs

instance (Eq edge) ⇒ Semigroup (Tree node edge leaf) where
  Node n cs <> Node _ cs' = Node n (cs <> cs')
  Node n cs <> Leaf _ = Node n cs
  Leaf a <> _ = Leaf a

instance (Eq edge) ⇒ Semigroup (Forest node edge leaf) where
  (Forest a) <> (Forest b) = Forest (HashMap.unionWith (<>) a b)

instance (Hashable edge) ⇒ Monoid (Forest node edge leaf) where
  mempty = Forest mempty

class
  (Hashable edge, Monad m) ⇒
  MonadTreeGen node edge leaf m
    | m → node edge leaf
  where
  leaf ∷ edge → leaf → m ()
  node ∷ edge → node → m a → m a

newtype TreeGenT node edge leaf m a
  = TreeGenT (StateT (Forest node edge leaf) m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFileExplorer
    , MonadIO
    , Alternative
    , MonadPlus
    )

instance MonadTrans (TreeGenT node edge leaf) where
  lift = TreeGenT . lift

instance
  (Monad m, Hashable edge)
  ⇒ MonadTreeGen node edge leaf (TreeGenT node edge leaf m)
  where
  leaf e l = TreeGenT do
    modify (<> Forest (HashMap.singleton e $ Leaf l))

  node e n computation = do
    s ← TreeGenT get
    TreeGenT $ put mempty
    inner ← computation
    forest ← TreeGenT get
    TreeGenT . put $ s <> Forest (HashMap.singleton e $ Node n forest)
    pure inner

run
  ∷ ∀ m edge node leaf a
   . (Monad m, Hashable edge)
  ⇒ TreeGenT node edge leaf m a
  → m (a, Forest node edge leaf)
run (TreeGenT inner) = runStateT inner mempty
