{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
    fromListChunked,
    toVector,
    toChunks,
  )
where

import qualified Brick.Widgets.List as Brick
import Control.Applicative (Applicative (..))
import qualified Control.Applicative as A
import Control.Monad (Monad (..), ap)
import Data.Bool (Bool (..), otherwise)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, length, null)
import Data.Function ((.))
import Data.Functor ((<$>), Functor)
import Data.Int (Int)
import qualified Data.List as L
import Data.Monoid (Monoid (..))
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable)
import qualified Data.Vector as V
import GHC.Base (($), (&&))
import GHC.Exts (IsList (..))
import GHC.Num ((+), (-))
import Lens.Micro
import Lens.Micro.Internal
import Text.Show (Show)

type LazyList n e = Brick.GenericList n LazyVector e

newtype LazyVector e = LazyVector [V.Vector e]
  deriving (Functor, Foldable, Traversable, Show)

instance Semigroup (LazyVector e) where
  (null -> True) <> LazyVector b = LazyVector b
  LazyVector a <> (null -> True) = LazyVector a
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

instance IsList (LazyVector a) where
  type Item (LazyVector a) = a
  fromList l = fromChunks [V.fromList l]
  fromListN n l = fromChunks [V.fromListN n l]
  toList (LazyVector a) = V.toList $ V.concat a

fromListChunked :: Int -> [a] -> LazyVector a
fromListChunked n = LazyVector . go
  where
    go [] = []
    go xs = let (h, t) = L.splitAt (max 1 n) xs in V.fromList h : go t

fromChunks :: [V.Vector a] -> LazyVector a
fromChunks = LazyVector
{-# INLINE fromChunks #-}

toChunks :: LazyVector a -> [V.Vector a]
toChunks (LazyVector a) = a
{-# INLINE toChunks #-}

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

merge :: Foldable t => t a -> [t a] -> [t a]
merge a b = if null a then b else a : b

instance Cons (LazyVector a) (LazyVector b) a b where
  _Cons _ (LazyVector []) = pure $ LazyVector []
  _Cons f (LazyVector xs) =
    let go (a, as) = fromChunks $ V.singleton a : toChunks as
     in go <$> f (V.head $ L.head xs, fromChunks $ merge (V.tail $ L.head xs) (L.tail xs))
  {-# INLINE _Cons #-}

instance Snoc (LazyVector a) (LazyVector b) a b where
  _Snoc _ (LazyVector []) = pure $ LazyVector []
  _Snoc f (LazyVector xs) =
    let go (as, a) = fromChunks $ toChunks as <> [V.singleton a]
     in go <$> f (fromChunks $ merge (V.init $ L.last xs) (L.init xs), V.last $ L.last xs)
  {-# INLINE _Snoc #-}

type instance Index (LazyVector a) = Int

type instance IxValue (LazyVector a) = a

instance Show a => Ixed (LazyVector a) where
  ix i f lv
    | 0 <= i && i < length lv = f v <&> \u ->
      let (h, t) = splitAt i lv
       in h <> fromChunks (V.singleton u : toChunks (t ^. _tail))
    | otherwise = pure lv
    where
      v = splitAt (i + 1) lv ^?! _1 . _last
  {-# INLINE ix #-}
