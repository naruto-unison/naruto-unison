module Util
  ( (?)
  , doIf, appendIf
  , shorten
  , groupBy, groupBy'
  , unzeroMod
  , zip3, zip4, zip5
  ) where

import Prelude
import Data.Array as Array
import Data.Array ((:), filter, snoc, uncons, zipWith)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (notElem)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Function.Memoize (memoize)
import Data.String.CodeUnits as CodeUnits

shorten :: String -> String
shorten = memoize $
          CodeUnits.fromCharArray <<< (shorten' <$> _) <<<
          filter (_ `notElem` [' ','-',':','(',')','®','.','/','?', '\'']) <<<
          CodeUnits.toCharArray
  where shorten' 'ō' = 'o'
        shorten' 'Ō' = 'O'
        shorten' 'ū' = 'u'
        shorten' 'Ū' = 'U'
        shorten' 'ä' = 'a'
        shorten' a   = a

groupBy :: ∀ a. (a -> a -> Boolean) -> Array a -> Array (NonEmpty Array a)
groupBy f = (NonEmptyArray.toNonEmpty <$> _) <<< Array.groupBy f

-- | Sorted `groupBy`. Analogous to `group'` vs `group`.
groupBy' :: ∀ a. (a -> a -> Boolean) -> Array a -> Array (NonEmpty Array a)
groupBy' pred xs' = case uncons xs' of
  Nothing -> []
  Just {head, tail} -> (head :| filter (pred head) tail)
                     : (groupBy' pred $ filter (not <<< pred head) tail)

unzeroMod :: ∀ a. EuclideanRing a => Eq a => a -> a -> a
unzeroMod x y = if modded == zero then y else modded
  where modded = x `mod` y

-- | ```
-- | doIf true  = identity
-- | doIf false = const identity
-- | ```
doIf :: ∀ a. Boolean -> (a -> a) -> (a -> a)
doIf true  = identity
doIf false = const identity

infixr 0 doIf as ?

appendIf :: ∀ a. a -> Boolean -> Array a -> Array a
appendIf x true  xs = xs `snoc` x
appendIf _ false xs = xs

zip3 :: ∀ a b c d. (a -> b -> c -> d)
     -> Array a -> Array b -> Array c -> Array d
zip3 f as bs cs = zipWith ($) (zipWith f as bs) cs
zip4 :: ∀ a b c d e. (a -> b -> c -> d -> e)
     -> Array a -> Array b -> Array c -> Array d -> Array e
zip4 f as bs cs ds = zipWith ($) (zip3 f as bs cs) ds
zip5 :: ∀ a b c d e f. (a -> b -> c -> d -> e -> f)
     -> Array a -> Array b -> Array c -> Array d -> Array e -> Array f
zip5 f as bs cs ds es = zipWith ($) (zip4 f as bs cs ds) es
