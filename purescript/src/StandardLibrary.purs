-- | A "classy prelude" that augments Prelude with other necessary modules
-- | and includes a few missing helper functions such as `zip3`.
module StandardLibrary
  ( module Prelude
  , module Control.Monad.Except
  , module Data.Array
  , module Data.Array.NonEmpty
  , module Data.Either
  , module Data.Foldable
  , module Data.Function
  , module Data.Function.Memoize
  , module Data.Map
  , module Data.Maybe
  , module Data.Newtype
  , module Data.NonEmpty
  , module Data.String
  , module Data.Tuple
  , module Effect
  , module Effect.Aff
  , module Effect.Class
  , module Effect.Aff.Class
  , module Foreign

  , (?), doIf
  , (??), justIf
  , mapLeft
  , unzeroMod
  , group, groupBy
  , consAfter
  , maybeDo
  , groupBy'
  , zip3, zip4, zip5
  ) where

-----------
--- EXPORTS
-----------

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array ((:), catMaybes, cons, delete, difference, drop, dropWhile, filter, head, index, init, intersect, last, mapMaybe, nub, nubBy, nubEq, nubByEq, partition, range, replicate, reverse, snoc, sort, sortBy, sortWith, span, tail, take, takeEnd, takeWhile, uncons, union, unionBy, unzip, zip, zipWith, (!!), (..), (\\))
import Data.Either (Either(Left, Right), choose, either, hush, isLeft, isRight)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (class Foldable, all, and, any, elem, find, fold, foldl, foldr, for_, intercalate, length, maximum, maximumBy, minimum, minimumBy, notElem, null, oneOf, oneOfMap, or, product, sequence_, sum, traverse_)
import Foreign (Foreign)
import Data.Function (on)
import Data.Maybe (Maybe(Just,Nothing), maybe, fromMaybe, fromMaybe', isJust, isNothing)
import Data.Map (Map)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..))
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
