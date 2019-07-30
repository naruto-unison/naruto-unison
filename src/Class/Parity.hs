module Class.Parity
  ( Parity(..)
  , allied
  , allies, enemies
  , split
  ) where

import ClassyPrelude hiding (even)
import qualified Prelude

-- | Types that are either even or odd.
class Parity a where
    even :: a -> Bool
    default even :: Integral a => a -> Bool
    even = Prelude.even

-- | Splits a sequence into alternating evens and odds, based on index.
split :: ∀ o. (Monoid o, SemiSequence o) => o -> (o, o)
split = foldr (\x ~(xs, ys) -> (x `cons` ys, xs)) (mempty, mempty)

-- | True if both arguments have matching parity.
allied :: ∀ a b. (Parity a, Parity b) => a -> b -> Bool
allied x y = even x == even y

-- | Filters a sequence to elements with indices of matching parity.
allies :: ∀ a o. (Parity a, Monoid o, SemiSequence o) => a -> o -> o
allies (even -> True) = fst . split
allies _              = snd . split

-- | Filters a sequence to elements with indices of non-matching parity.
enemies :: ∀ a o. (Parity a, Monoid o, SemiSequence o) => a -> o -> o
enemies (even -> False) = fst . split
enemies _               = snd . split

instance Parity Bool where
    even = id
instance Parity Int
instance Parity Int32
instance Parity Int64
instance Parity Integer
instance Parity Word
instance Parity Word8
instance Parity Word32
instance Parity Word64
