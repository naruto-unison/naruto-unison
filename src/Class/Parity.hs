module Class.Parity
  ( Parity(..)
  , allied
  , getOf
  , half
  ) where

import ClassyPrelude hiding (even)
import qualified Prelude

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

half :: ∀ p a. Parity p => p -> Vector a -> Vector a
half p xs = splitHalf (length xs `quot` 2) xs
  where
    splitHalf
      | even p    = take
      | otherwise = drop

getOf :: ∀ a b. Parity a => a -> (b, b) -> b
getOf x
  | even x    = fst
  | otherwise = snd
{-# INLINE getOf #-}

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
