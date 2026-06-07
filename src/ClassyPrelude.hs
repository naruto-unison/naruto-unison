{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The [classy-prelude](https://hackage.haskell.org/package/classy-prelude)
-- package, but using "Data.List.NonEmpty" instead of "Data.NonNull".
module ClassyPrelude
  ( module CP
  , module Prelude
  , module Data.List.NonEmpty
  , module Data.Kind
  , maximum, minimum, maximumBy, minimumBy
  , pattern Empty, pattern (:<), pattern (:>)
  ) where

import "classy-prelude" ClassyPrelude as CP hiding (head, last, group, groupBy, maximum, minimum, maximumBy, minimumBy, init, tail)
import Prelude (type (~), MonadFail(..), errorWithoutStackTrace, foldl1, foldr1, ShowS, Show(..), shows, showChar, showString, showParen, ReadS, Read(..), reads, readParen, read, lex)
import Data.List.NonEmpty (NonEmpty(..), head, last, init, tail, group, groupBy, groupWith, groupAllWith, group1, groupBy1, groupWith1, groupAllWith1)
import Data.Kind (Constraint, Type)

import qualified Data.Foldable as F

-- This isn't exported; it's a helper function for the following ones.
foldl1' :: ∀ a. (a -> a -> a) -> NonEmpty a -> a
foldl1' f (x:|xs) = F.foldl' f x xs
{-# INLINE foldl1' #-}

maximum :: ∀ a. Ord a => NonEmpty a -> a
maximum = foldl1' max
{-# INLINE maximum #-}

minimum :: ∀ a. Ord a => NonEmpty a -> a
minimum = foldl1' min
{-# INLINE minimum #-}

maximumBy :: ∀ a. (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy cmp = foldl1' \x y -> case cmp x y of
    GT -> x
    _  -> y
{-# INLINABLE maximumBy #-}

minimumBy :: ∀ a. (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy cmp = foldl1' \x y -> case cmp x y of
    GT -> y
    _  -> x
{-# INLINABLE minimumBy #-}

pattern Empty :: ∀ o. IsSequence o => o
pattern Empty <- (null -> True) where
    Empty = mempty

-- | 'cons' and 'uncons'
pattern (:<) :: ∀ o. IsSequence o => Element o -> o -> o
pattern x :< xs <- (uncons -> Just (x, xs)) where
    (:<) = cons
infixr 5 :<
{-# COMPLETE Empty, (:<) #-}

-- | 'snoc' and 'unsnoc'
pattern (:>) :: ∀ o. IsSequence o => o -> Element o -> o
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
    (:>) = snoc
infixl 5 :>
{-# COMPLETE Empty, (:>) #-}
