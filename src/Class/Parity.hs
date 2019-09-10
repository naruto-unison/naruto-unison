module Class.Parity
  ( Parity(..)
  , allied
  , split
  , getOf
  ) where

import ClassyPrelude hiding (even)
import qualified Prelude

-- | Types that are either even or odd.
class Parity a where
    even :: a -> Bool

    default even :: Integral a => a -> Bool
    even = Prelude.even
    {-# INLINE even #-}

-- | True if both arguments have matching parity.
allied :: ∀ a b. (Parity a, Parity b) => a -> b -> Bool
allied x y = even x == even y

-- | Splits a sequence into alternating evens and odds, based on index.
split :: ∀ o. (Monoid o, SemiSequence o) => o -> (o, o)
split = foldr (\x ~(xs, ys) -> (x `cons` ys, xs)) (mempty, mempty)

getOf :: ∀ a b. Parity a => a -> (b, b) -> b
getOf x
  | even x    = fst
  | otherwise = snd

instance Parity Bool where
    even = id
    {-# INLINE even #-}
instance Parity Int
instance Parity Int32
instance Parity Int64
instance Parity Integer
instance Parity Word
instance Parity Word8
instance Parity Word32
instance Parity Word64
