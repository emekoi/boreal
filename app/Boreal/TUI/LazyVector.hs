{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- inspired by the Purebred.LazyVector
-- https://github.com/purebred-mua/purebred/blob/master/src/Purebred/LazyVector.hs

module Boreal.TUI.LazyVector
  ( LazyList,
    LazyVector (..),
    fromList,
    fromChunks,
    splitAt,
    appendChunk,
    toVector,
  )
where

import qualified Brick.Widgets.List as Brick
import Control.Applicative (Applicative (..))
import qualified Control.Applicative as A
import Control.Monad (Monad (..), ap)
import Data.Bool (otherwise)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, length)
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Int (Int)
import qualified Data.List as L
import Data.Monoid (Monoid (..))
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable)
import qualified Data.Vector as V
import Text.Show (Show)
import Prelude (($), (-))

type LazyList n e = Brick.GenericList n LazyVector e

newtype LazyVector e = LazyVector [V.Vector e]
  deriving (Functor, Foldable, Traversable, Show)

instance Semigroup (LazyVector e) where
  LazyVector a <> LazyVector b = LazyVector (a <> b)

instance Applicative LazyVector where
  pure a = LazyVector [V.singleton a]
  (<*>) = ap

instance Monad LazyVector where
  return = A.pure
  (LazyVector s) >>= f = LazyVector [V.concat $ L.map (V.concatMap f') s]
    where
      f' a = toVector $ f a

instance Monoid (LazyVector a) where
  mempty = LazyVector []

instance Eq a => Eq (LazyVector a) where
  LazyVector a == LazyVector b = V.concat a == V.concat b

instance Ord a => Ord (LazyVector a) where
  LazyVector a `compare` LazyVector b = V.concat a `compare` V.concat b

instance Brick.Splittable LazyVector where
  splitAt = splitAt

fromList :: Int -> [a] -> LazyVector a
fromList n = LazyVector . go
  where
    go [] = []
    go xs = let (h, t) = L.splitAt (max 1 n) xs in V.fromList h : go t

fromChunks :: [V.Vector a] -> LazyVector a
fromChunks = LazyVector

appendChunk :: LazyVector a -> V.Vector a -> LazyVector a
appendChunk l v = l <> LazyVector [v]

toVector :: LazyVector a -> V.Vector a
toVector (LazyVector a) = V.concat a

splitAt :: Int -> LazyVector a -> (LazyVector a, LazyVector a)
splitAt i (LazyVector chunks) = case chunks of
  [] -> (LazyVector [], LazyVector [])
  (h : t)
    | i == length h -> (LazyVector [h], LazyVector t)
    | i < length h ->
      let (th, ht) = V.splitAt i h
       in (LazyVector [th], LazyVector (ht : t))
    | otherwise ->
      let (LazyVector h', LazyVector t') = splitAt (i - length h) (LazyVector t)
       in (LazyVector (h : h'), LazyVector t')
-- elemSL :: Eq a => a -> LazyVector a -> Bool
-- elemSL _ (LazyVector []) = False
-- elemSL x (LazyVector [v]) = Vec.elem x v
-- elemSL x (LazyVector (v : v')) = Vec.elem x v || elemSL x (LazyVector v')

-- notElemSL :: Eq a => a -> LazyVector a -> Bool
-- notElemSL x = not . elemSL x

-- findSL :: (a -> Bool) -> LazyVector a -> Maybe a
-- findSL _ (LazyVector []) = Nothing
-- findSL p (LazyVector [v]) = Vec.find p v
-- findSL p (LazyVector (v : v')) = case Vec.find p v of
--   Just f -> Just f
--   Nothing -> findSL p (LazyVector v')
