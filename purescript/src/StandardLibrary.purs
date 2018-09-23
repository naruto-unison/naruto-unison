-- | A "classy prelude" that augments Prelude with other necessary modules
-- | and includes a few missing helper functions such as `intersperse`.
module StandardLibrary
  ( module Prelude
  , module Control.Alternative
  , module Control.Bind
  , module Control.Monad.Except
  , module Control.MonadZero
  , module Control.Plus
  , module Data.Array
  , module Data.Array.NonEmpty
  , module Data.Either
  , module Data.Enum
  , module Data.Foldable
  , module Data.Function
  , module Data.Function.Memoize
  , module Data.Map
  , module Data.Maybe
  , module Data.Newtype
  , module Data.NonEmpty
  , module Data.Ord
  , module Data.String
  , module Data.String.Regex
  , module Data.Traversable
  , module Data.Tuple
  , module Effect
  , module Effect.Aff
  , module Effect.Class
  , module Effect.Aff.Class

  , (?), doIf
  , (??), justIf
  , mapLeft
  , unzeroMod
  , group, groupBy
  , consAfter
  , enumArray
  , compareThen
  , maybeDo
  , filterOut
  , intersperse
  , pairWith
  , groupBy'
  , zip3, zip4, zip5
  ) where

-----------
--- EXPORTS
-----------

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Control.Bind (bindFlipped)
import Control.Monad.Except (runExcept)
import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Array ((:), catMaybes, cons, delete, difference, drop, dropWhile, filter, head, index, init, intersect, last, mapMaybe, nub, nubBy, nubEq, nubByEq, partition, range, replicate, reverse, snoc, sort, sortBy, sortWith, span, tail, take, takeEnd, takeWhile, uncons, union, unionBy, unzip, zip, zipWith, (!!), (..), (\\))
import Data.Either (Either(Left, Right), choose, either, hush, isLeft, isRight)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Enum (class BoundedEnum, enumFromTo, toEnum)
import Data.Foldable (class Foldable, all, and, any, elem, find, fold, foldl, foldr, for_, intercalate, length, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, traverse_)
import Data.Function (on)
import Data.Maybe (Maybe(Just,Nothing), maybe, fromMaybe, fromMaybe', isJust, isNothing)
import Data.Map (Map)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Newtype (unwrap)
import Data.Ord (abs, signum)
import Data.String (Pattern(..), Replacement(..))
import Data.String.Regex (Regex)
import Data.Traversable (scanr, scanl, traverse)
import Data.Function.Memoize (memoize)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)

--------------------
--- HELPER FUNCTIONS
--------------------

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.String.CodePoints (fromCodePointArray, toCodePointArray)

infix 0 justIf as ??
infixr 9 doIf  as ?

mapLeft :: ∀ a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x

-- | ```
-- | justIf true  = Just
-- | justIf false = const Nothing
-- | ```
justIf :: ∀ a. Boolean -> a -> Maybe a
justIf true = Just
justIf false = const Nothing

groupBy :: ∀ a. (a -> a -> Boolean) -> Array a -> Array (NonEmpty Array a)
groupBy f = map NonEmptyArray.toNonEmpty <<< Array.groupBy f

group :: ∀ a. Eq a => Array a -> Array (NonEmpty Array a)
group = map NonEmptyArray.toNonEmpty <<< Array.group

consAfter :: ∀ a. a -> Array a -> Array a
consAfter = flip snoc

unzeroMod :: ∀ a. EuclideanRing a => Eq a => a -> a -> a
unzeroMod a b = if modded == zero then b else modded
  where modded = a `mod` b

-- | ```
-- | doIf true  = identity
-- | doIf false = const identity
-- | ```
doIf :: ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

-- | `maybeDo f x = fromMaybe x $ f x`
maybeDo :: ∀ a. (a -> Maybe a) -> a -> a
maybeDo f x = fromMaybe x $ f x

-- | `enumArray = enumFromTo bottom top`
enumArray :: ∀ a. BoundedEnum a => Array a
enumArray = flip memoize unit \_ -> enumFromTo bottom top

-- | Compares by a first function, then by a second if the first yielded EQ.
compareThen :: ∀ a b c. Ord b => Ord c => (a -> b) -> (a -> c) -> a -> a
            -> Ordering
compareThen f g x y = case compare (f x) (f y) of
                          EQ -> compare (g x) (g y)
                          a  -> a

-- | Removes all characters in a `Pattern` from a `String`.
filterOut :: Pattern -> String -> String
filterOut (Pattern p) = fromCodePointArray <<<
                        filter (flip notElem ps) <<<
                        toCodePointArray
  where
    ps = toCodePointArray p

-- | Adds an element between every element in an
-- | `intersperse 0 [1,2,3] == [1,0,2,0,3]`
intersperse :: ∀ a. a -> Array a -> Array a
intersperse _ x@[_] = x
intersperse sep xs = case uncons xs of
    Nothing           -> xs
    Just {head, tail} -> cons head <<< cons sep $ intersperse sep tail

-- | Tuple builder from a constructor
pairWith :: ∀ a b c f. Functor f => a -> (b -> c) -> f b -> f (Tuple c a)
pairWith val construc = map $ (flip Tuple val) <<< construc

-- | Sorted `groupBy`. Analogous to `group'` vs `group`.
groupBy' :: ∀ a. (a -> a -> Boolean) -> Array a -> Array (NonEmpty Array a)
groupBy' pred xs' = case uncons xs' of
  Nothing -> []
  Just {head, tail} -> (head :| filter (pred head) tail)
                     : (groupBy' pred $ filter (not <<< pred head) tail)

zip3 :: ∀ a b c d. (a -> b -> c -> d) 
     -> Array a -> Array b -> Array c -> Array d
zip3 f as bs cs = zipWith ($) (zipWith f as bs) cs
zip4 :: ∀ a b c d e. (a -> b -> c -> d -> e) 
     -> Array a -> Array b -> Array c -> Array d -> Array e
zip4 f as bs cs ds = zipWith ($) (zip3 f as bs cs) ds
zip5 :: ∀ a b c d e f. (a -> b -> c -> d -> e -> f) 
     -> Array a -> Array b -> Array c -> Array d -> Array e -> Array f
zip5 f as bs cs ds es = zipWith ($) (zip4 f as bs cs ds) es
