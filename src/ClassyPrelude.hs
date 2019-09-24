{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK hide     #-}
module ClassyPrelude
  ( module CP
  , module Data.List.NonEmpty
  , foldl1, foldl1', foldr1, maximum, minimum, maximumBy, minimumBy
  ) where

import "classy-prelude" ClassyPrelude as CP hiding (Handler, head, last, group, groupBy, maximum, minimum, maximumBy, minimumBy, init, tail)
import Data.List.NonEmpty (NonEmpty(..), head, last, init, tail, group, groupBy, groupWith, groupAllWith, group1, groupBy1, groupWith1, groupAllWith1)

import qualified Data.List as List

foldl1 :: ∀ a. (a -> a -> a) -> NonEmpty a -> a
foldl1 f (x:|xs) = List.foldl f x xs
{-# INLINE foldl1 #-}

foldl1' :: ∀ a. (a -> a -> a) -> NonEmpty a -> a
foldl1' f (x:|xs) = List.foldl' f x xs
{-# INLINE foldl1' #-}

foldr1 :: ∀ a. (a -> a -> a) -> NonEmpty a -> a
foldr1 f (x:|xs) = List.foldr f x xs
{-# INLINE foldr1 #-}

maximum :: ∀ a. Ord a => NonEmpty a -> a
maximum = foldl1 max
{-# INLINE maximum #-}

minimum :: ∀ a. Ord a => NonEmpty a -> a
minimum = foldl1 min
{-# INLINE minimum #-}

maximumBy :: ∀ a. (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy cmp = foldl1 \x y -> case cmp x y of
    GT -> x
    _  -> y

minimumBy :: ∀ a. (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy cmp = foldl1 \x y -> case cmp x y of
    GT -> y
    _  -> x
