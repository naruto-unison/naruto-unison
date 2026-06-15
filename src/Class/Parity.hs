module Class.Parity
  ( Parity(..)
  , allied
  , opponent
  , getOf, setOf, modifyOf
  , half
  , swap
  ) where

import ClassyPrelude hiding (even, swap)
import qualified Prelude
import qualified Data.Tuple as Tuple

-- | Types that are either even or odd.
--
--Instances should satisfy the following laws:
--
-- * @even a == even b && even b == even c <=> even a == even c@
-- * @even a == even b && even b /= even c <=> even a /= even c@
class Parity a where
    even :: a -> Bool

    default even :: Integral a => a -> Bool
    even = Prelude.even
    {-# INLINE even #-}

-- | True if both arguments have matching parity.
allied :: ∀ a b. (Parity a, Parity b) => a -> b -> Bool
allied x y = even x == even y
{-# INLINE allied #-}

-- | Returns a value which has the opposite parity as the input.
opponent :: ∀ a. Parity a => a -> Bool
opponent = not . even
{-# INLINE opponent #-}

-- | If 'even', takes the first half. Otherwise, drops the first half.
half :: ∀ o p. (IsSequence o, Parity p) => p -> o -> o
half p xs = splitHalf (lengthIndex xs `quot` 2) xs
  where
    splitHalf
      | even p    = take
      | otherwise = drop

-- | 'fst' if 'even', otherwise 'snd'.
getOf :: ∀ a b. Parity a => a -> (b, b) -> b
getOf x
  | even x    = fst
  | otherwise = snd
{-# INLINE getOf #-}

-- | 'first' if 'even', otherwise 'second'.
modifyOf :: ∀ a b. Parity a => a -> (b -> b) -> (b, b) -> (b, b)
modifyOf x
  | even x    = first
  | otherwise = second
{-# INLINE modifyOf #-}

-- | 'first . const' if 'even', otherwise 'second . const'.
setOf :: ∀ a b. Parity a => a -> b -> (b, b) -> (b, b)
setOf x = modifyOf x . const
{-# INLINE setOf #-}

-- | 'id' if 'even', otherwise 'swap'.
swap :: ∀ a b. Parity a => a -> (b, b) -> (b, b)
swap x
  | even x    = id
  | otherwise = Tuple.swap
{-# INLINE swap #-}


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
