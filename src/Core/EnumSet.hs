{-
Some of the code in this module is modified from the
[EdisonCore](https://hackage.haskell.org/package/EdisonCore-1.3.2.1) package's
[Data.Edison.Coll.EnumSet](https://hackage.haskell.org/package/EdisonCore-1.3.2.1/docs/Data-Edison-Coll-EnumSet.html).
As such, the copyright from that file is reproduced below.
-}

{-
Copyright (c) 2006, 2008, David F. Place
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:


* Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

* Neither the name of David F. Place nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
-- | Efficient sets over enumerations represented by bitwise operations.
-- For type @EnumSet A B@, @A@ should be a 'Word'-like type that implements
-- 'Bits' and 'Num', and @B@ should be a type that implements 'Eq' and 'Enum'
-- equivalently and is a bijection to 'Int'.
--
-- @EnumSet A B@ will not store any @B@ with a 'fromEnum' less than 0 or greater
-- than the number of bits in @A@, so it is best for @B@ to be a type that
-- derives 'Eq' and 'Enum' and for @A@ to have at least as many bits as the
-- number of constructors of @B@.
--
-- Depending on the number of constructors, one of 'Data.Word.Word8',
-- 'Data.Word.Word16', 'Data.Word.Word32', or 'Data.Word.Word64' is a good
-- default representation type. For enumerations with many constructors, there
-- is also [Word128](https://hackage.haskell.org/package/wide-word-0.1.0.8/docs/Data-WideWord-Word128.html)
-- from the [wide-word](https://hackage.haskell.org/package/wide-word-0.1.0.8/)
-- package.
module Core.EnumSet
  ( -- * Set type
    EnumSet

    -- * Construction
  , empty
  , singleton
  , fromFoldable

  -- * Insertion
  , insert

  -- * Deletion
  , delete

  -- * Query
  , member
  , notMember
  , null
  , size
  , isSubsetOf

  -- * Combine
  , union
  , difference
  , (\\)
  , symmetricDifference
  , intersection

  -- * Filter
  , filter
  , partition

  -- * Map
  , map

  -- * Folds
  , foldl, foldl', foldr, foldr'
  , foldl1, foldl1', foldr1, foldr1'

  -- ** Special folds
  , foldMap
  , any
  , all

   -- * Min/Max
  , minimum
  , maximum
  , deleteMin
  , deleteMax
  , minView
  , maxView

  -- * Conversion
  , toList
  , toBits

  -- * Debugging
  , appropriate
  ) where

import qualified ClassyPrelude
import qualified GHC.Exts
import qualified Data.Foldable as F

import ClassyPrelude hiding ((\\), all, any, delete, difference, empty, filter, fold, foldl', foldMap, foldr, fromList, intersection, null, map, maximum, member, minimum, notMember, lookup, partition, singleton, toList, union)
import Data.Aeson (ToJSON(..))
import Data.Bits
import GHC.Exts (IsList(Item))
import Data.Monoid (Monoid(..))
import Text.Read
import Text.Show

{--------------------------------------------------------------------
  Set type
--------------------------------------------------------------------}

-- | A set of values @a@ with representation @word@,
-- implemented as bitwise operations.
newtype EnumSet word a = EnumSet { toBits :: word } deriving (Eq, Ord)

instance Bits w => Semigroup (EnumSet w a) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Bits w => Monoid (EnumSet w a) where
    mempty = empty
    {-# INLINE mempty #-}

instance (Bits w, Num w, Enum a) => IsList (EnumSet w a) where
    type Item (EnumSet w a) = a
    fromList = fromFoldable
    {-# INLINE fromList #-}
    toList = toList
    {-# INLINE toList #-}

instance (Bits w, Num w, Enum a, ToJSON a) => ToJSON (EnumSet w a) where
    toJSON = toJSON . toList
    {-# INLINE toJSON #-}

type instance Element (EnumSet w a) = a

instance (Bits w, Num w, Enum a) => MonoFunctor (EnumSet w a) where
    omap = map
    {-# INLINE omap #-}

instance (Bits w, Num w, Enum a) => MonoFoldable (EnumSet w a) where
    ofoldMap = foldMap
    {-# INLINE ofoldMap #-}
    ofoldr = foldr
    {-# INLINE ofoldr #-}
    ofoldl' = foldl'
    {-# INLINE ofoldl' #-}
    ofoldr1Ex = foldr1
    {-# INLINE ofoldr1Ex #-}
    ofoldl1Ex' = foldl1'
    {-# INLINE ofoldl1Ex' #-}
    otoList = toList
    {-# INLINE otoList #-}
    oall = all
    {-# INLINE oall #-}
    oany = any
    {-# INLINE oany #-}
    onull = null
    {-# INLINE onull #-}
    olength64 (EnumSet w) = countBits w
    {-# INLINE olength64 #-}
    headEx = minimum
    {-# INLINE headEx #-}
    lastEx = maximum
    {-# INLINE lastEx #-}
    oelem = member
    {-# INLINE oelem #-}
    onotElem x = not . member x
    {-# INLINE onotElem #-}

instance (Bits w, Num w, Enum a) => GrowingAppend (EnumSet w a)

instance (Bits w, Num w, Eq a, Enum a) => SetContainer (EnumSet w a) where
    type ContainerKey (EnumSet w a) = a
    member = member
    {-# INLINE member #-}
    notMember = notMember
    {-# INLINE notMember #-}
    union = union
    {-# INLINE union #-}
    difference = difference
    {-# INLINE difference #-}
    intersection = intersection
    {-# INLINE intersection #-}
    keys = toList
    {-# INLINE keys #-}

instance (Bits w, Num w, Eq a, Enum a) => IsSet (EnumSet w a) where
    insertSet = insert
    {-# INLINE insertSet #-}
    deleteSet = delete
    {-# INLINE deleteSet #-}
    singletonSet = singleton
    {-# INLINE singletonSet #-}
    setFromList = fromFoldable
    {-# INLINE setFromList #-}
    setToList = toList
    {-# INLINE setToList #-}

instance (Bits w, Num w, Enum x, Show x) => Show (EnumSet w x) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)

instance (Bits w, Num w, Enum x, Read x) => Read (EnumSet w x) where
    readPrec = parens $ prec 10 do
        Ident "fromList" <- lexP
        xs :: [x] <- readPrec
        return (fromFoldable xs)
    readListPrec = readListPrecDefault

instance (Bits w, Bounded x, Enum x) => Bounded (EnumSet w x) where
    minBound = empty
    maxBound = EnumSet $ F.foldl' (flip $ (.|.) . bit) zeroBits
               [0..fromEnum (maxBound :: x)]

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty set.
empty :: ∀ w a. Bits w
      => EnumSet w a
empty = EnumSet zeroBits
{-# INLINE empty #-}

-- | /O(1)/. A set of one element.
singleton :: ∀ w a. (Bits w, Enum a)
          => a -> EnumSet w a
singleton = EnumSet . setBit zeroBits . fromEnum
{-# INLINE singleton #-}

-- | /O(n)/. Create a set from a foldable data structure.
fromFoldable :: ∀ f w a. (Foldable f, Bits w, Enum a)
             => f a -> EnumSet w a
fromFoldable = EnumSet . F.foldl' (flip $ (.|.) . bit . fromEnum) zeroBits

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(1)/. Add a value to the set.
insert :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> EnumSet w a
insert x (EnumSet w) = EnumSet . setBit w $ fromEnum x

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

-- | /O(1)/. Delete a value in the set.
delete :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> EnumSet w a
delete x (EnumSet w) = EnumSet . clearBit w $ fromEnum x

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the value a member of the set?
member :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> Bool
member x (EnumSet w) = testBit w $ fromEnum x

-- | /O(1)/. Is the value not in the set?
notMember :: ∀ w a. (Bits w, Enum a)
          => a -> EnumSet w a -> Bool
notMember x = not . member x

-- | /O(1)/. Is this the empty set?
null :: ∀ w a. Bits w
     => EnumSet w a -> Bool
null (EnumSet w) = zeroBits == w

-- | /O(1)/. The number of elements in the set.
size :: ∀ w a. (Bits w, Num w)
     => EnumSet w a -> Int
size (EnumSet w) = countBits w

-- | /O(1)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: ∀ w a. (Bits w)
           => EnumSet w a -> EnumSet w a -> Bool
isSubsetOf x y = x `union` y == y

{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}

-- | /O(1)/. The union of two sets.
union :: ∀ w a. Bits w
      => EnumSet w a -> EnumSet w a -> EnumSet w a
union (EnumSet x) (EnumSet y) = EnumSet $ x .|. y

-- | /O(1)/. Difference between two sets.
difference :: ∀ w a. Bits w
           => EnumSet w a -> EnumSet w a -> EnumSet w a
difference (EnumSet x) (EnumSet y) = EnumSet $ (x .|. y) `xor` y

-- | /O(1)/. See 'difference'.
(\\) :: ∀ w a. Bits w
     => EnumSet w a -> EnumSet w a -> EnumSet w a
(\\) = difference
infixl 9 \\
{-# INLINE (\\) #-}

-- | /O(1)/. Elements which are in either set, but not both.
symmetricDifference :: ∀ w a. Bits w
                    => EnumSet w a -> EnumSet w a -> EnumSet w a
symmetricDifference (EnumSet x) (EnumSet y) = EnumSet $ x `xor` y

-- | /O(1)/. The intersection of two sets.
intersection :: ∀ w a. Bits w
             => EnumSet w a -> EnumSet w a -> EnumSet w a
intersection (EnumSet x) (EnumSet y) = EnumSet $ x .&. y

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> Bool) -> EnumSet w a -> EnumSet w a
filter p (EnumSet w) = EnumSet $ foldlBits' f 0 w
    where
      f z i
        | p $ toEnum i = setBit z i
        | otherwise    = z
      {-# INLINE f #-}

-- | /O(n)/. Partition the set according to some predicate.
-- The first set contains all elements that satisfy the predicate,
-- the second all elements that fail the predicate.
partition :: ∀ w a. (Bits w, Num w, Enum a)
          => (a -> Bool) -> EnumSet w a -> (EnumSet w a, EnumSet w a)
partition p (EnumSet w) = (EnumSet yay, EnumSet nay)
    where
      (yay,nay) = foldlBits' f (0, 0) w
      f (x, y) i
          | p $ toEnum i = (setBit x i, y)
          | otherwise    = (x, setBit y i)
      {-# INLINE f #-}

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | /O(n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: ∀ w a b. (Bits w, Num w, Enum a, Enum b)
    => (a -> b) -> EnumSet w a -> EnumSet w b
map f0 (EnumSet w) = EnumSet $ foldlBits' f 0 w
    where
      f z i = setBit z $ fromEnum $ f0 (toEnum i)
      {-# INLINE f #-}

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Left fold.
foldl :: ∀ w a c. (Bits w, Num w, Enum a)
      => (c -> a -> c) -> c -> EnumSet w a -> c
foldl f z (EnumSet w) = foldlBits folder z w
  where
    folder h = f h . toEnum
    {-# INLINE folder #-}

-- | /O(n)/. Left fold with strict accumulator.
foldl' :: ∀ w a c. (Bits w, Num w, Enum a)
       => (c -> a -> c) -> c -> EnumSet w a -> c
foldl' f z (EnumSet w) = foldlBits' folder z w
  where
    folder h = f h . toEnum
    {-# INLINE folder #-}

-- | /O(n)/. Right fold.
foldr :: (Bits w, Num w, Enum a)
      => (a -> b -> b) -> b -> EnumSet w a -> b
foldr f z (EnumSet w) = foldrBits (f . toEnum) z w

-- | /O(n)/. Right fold with strict accumulator.
foldr' :: ∀ w a b. (Bits w, Num w,  Enum a)
       => (a -> b -> b) -> b -> EnumSet w a -> b
foldr' f z (EnumSet w) = foldrBits' (f . toEnum) z w

-- | /O(n)/. Left fold on non-empty sets.
foldl1 :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
foldl1 f (EnumSet w) = foldlBits folder (toEnum mininum) (clearBit w mininum)
  where
    mininum = lsb w
    folder z = f z . toEnum

-- | /O(n)/. Left fold on non-empty sets with strict accumulator.
foldl1' :: ∀ w a. (Bits w, Num w, Enum a)
        => (a -> a -> a) -> EnumSet w a -> a
foldl1' f (EnumSet w) = foldlBits' folder (toEnum mininum) (clearBit w mininum)
  where
    mininum = lsb w
    folder z = f z . toEnum

-- | /O(n)/. Right fold on non-empty sets.
foldr1 :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
foldr1 f (EnumSet w) = foldrBits (f . toEnum) (toEnum maxi) (clearBit w maxi)
    where
      maxi = msb w

-- | /O(n)/. Right fold on non-empty sets with strict accumulator.
foldr1' :: ∀ w a. (Bits w, Num w, Enum a)
        => (a -> a -> a) -> EnumSet w a -> a
foldr1' f (EnumSet w) = foldrBits' (f . toEnum) (toEnum maxi) (clearBit w maxi)
    where
      maxi = msb w

-- | /O(n)/. Map each element of the structure to a monoid, and combine the
-- results.
foldMap :: ∀ m w a. (Monoid m, Bits w, Num w, Enum a)
        => (a -> m) -> EnumSet w a -> m
foldMap f (EnumSet w) = foldrBits (mappend . f . toEnum) mempty w

-- | /O(n)/. Check if all elements satisfy some predicate.
all :: ∀ w a. (Bits w, Num w, Enum a)
    => (a -> Bool) -> EnumSet w a -> Bool
all f (EnumSet w) = go 0
  where
    end = countBits w
    c   = complement w
    go i
      | i >= end                    = True
      | testBit c i || f (toEnum i) = go $ i + 1
      | otherwise                   = False

-- | /O(n)/. Check if any element satisfies some predicate.
any :: ∀ w a. (Bits w, Num w, Enum a)
    => (a -> Bool) -> EnumSet w a -> Bool
any f (EnumSet w) = go 0
  where
    end = countBits w
    go i
      | i >= end                    = False
      | testBit w i && f (toEnum i) = True
      | otherwise                   = go $ 1 + i

{--------------------------------------------------------------------
  Min/Max
--------------------------------------------------------------------}

-- | /O(1)/. The minimal element of a non-empty set.
minimum :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> a
minimum (EnumSet w) = toEnum $ lsb w

-- | /O(1)/. The maximal element of a non-empty set.
maximum :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> a
maximum (EnumSet w) = toEnum $ msb w

-- | /O(1)/. Delete the minimal element.
deleteMin :: ∀ w a. (Bits w, Num w)
          => EnumSet w a -> EnumSet w a
deleteMin (EnumSet w)
  | w == 0    = empty
  | otherwise = EnumSet $ clearBit w $ lsb w

-- | /O(1)/. Delete the maximal element.
deleteMax :: ∀ w a. (Bits w, Num w)
          => EnumSet w a -> EnumSet w a
deleteMax (EnumSet w)
   | w == 0    = empty
   | otherwise = EnumSet $ clearBit w $ msb w

-- | /O(1)/. Retrieves the minimal element of the set,
-- and the set stripped of that element,
-- or Nothing if passed an empty set.
minView :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> Maybe (a, EnumSet w a)
minView (EnumSet w)
  | w == 0    = Nothing
  | otherwise = let i = lsb w in Just (toEnum i, EnumSet $ clearBit w i)

-- | /O(1)/. Retrieves the maximal element of the set,
-- and the set stripped of that element,
-- or Nothing if passed an empty set.
maxView :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> Maybe (a, EnumSet w a)
maxView (EnumSet w)
  | w == 0    = Nothing
  | otherwise = let i = msb w in Just (toEnum i, EnumSet $ clearBit w i)

{--------------------------------------------------------------------
  Conversion
--------------------------------------------------------------------}

-- | /O(n)/. Convert the set to a list of values.
toList :: ∀ w a. (Bits w, Num w, Enum a)
       => EnumSet w a -> [a]
toList (EnumSet w) = foldrBits ((:) . toEnum) [] w
{-# INLINE toList #-}

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}

-- | Verify whether for some representation @EnumSet w a@, @w@ has sufficient
-- bits to store @maxBound :: a@. The actual value of the argument is ignored.
--
-- This function does not verify that @minBound :: a >= 0@, nor that
--
-- > fromEnum x == fromEnum y <==> x == y
-- > toEnum x == toEnum y <==> x == y
appropriate :: ∀ w a. (FiniteBits w, Enum a, Bounded a) => EnumSet w a -> Bool
appropriate = const $ fromEnum (maxBound :: a) <= finiteBitSize (zeroBits :: w)

{--------------------------------------------------------------------
  Utility functions
--------------------------------------------------------------------}

countBits :: ∀ w i. (Bits w, Num w, Num i) => w -> i
countBits w = w `seq` bitcount 0 w
{-# INLINE countBits #-}

bitcount :: ∀ w i. (Bits w, Num w, Num i) => i -> w -> i
bitcount a 0 = a
bitcount a x = a `seq` bitcount (a+1) (x .&. (x - 1))

-- From http://aggregate.org/MAGIC/
-- by Henry Gordon Dietz at the University of Kentucky.
lsb :: ∀ w. (Bits w, Num w) => w -> Int
lsb x = countBits $ (x - 1) .&. complement x
{-# INLINE lsb #-}

msb :: ∀ w. (Bits w, Num w) => w -> Int
msb x0 = let
     x1 = x0 .|. (x0 `shiftR` 1)
     x2 = x1 .|. (x1 `shiftR` 2)
     x3 = x2 .|. (x2 `shiftR` 4)
     x4 = x3 .|. (x3 `shiftR` 8)
     x5 = x4 .|. (x4 `shiftR` 16)
     in countBits x5 - 1
{-# INLINE msb #-}

foldrBits :: ∀ w a. (Bits w, Num w)
          => (Int -> a -> a) -> a -> w -> a
foldrBits f z = foldrBitsAux f z 0
{-# INLINE foldrBits #-}

foldrBitsAux :: ∀ a w. (Bits w, Num w)
              => (Int -> a -> a) -> a -> Int -> w -> a
foldrBitsAux _ z _ 0 = z
foldrBitsAux f z i w = case (i `seq` w) .&. 0x0F of
     0x00 -> a
     0x01 -> f i a
     0x02 -> f (i+1) a
     0x03 -> f i $ f (i+1) a
     0x04 -> f (i+2) a
     0x05 -> f i $ f (i+2) a
     0x06 -> f (i+1) $ f (i+2) a
     0x07 -> f i $ f (i+1) $ f (i+2) a
     0x08 -> f (i+3) a
     0x09 -> f i $ f (i+3) a
     0x0A -> f (i+1) $ f (i+3) a
     0x0B -> f i $ f (i+1) $ f (i+3) a
     0x0C -> f (i+2) $ f (i+3) a
     0x0D -> f i $ f (i+2) $ f (i+3) a
     0x0E -> f (i+1) $ f (i+2) $ f (i+3) a
     0x0F -> f i $ f (i+1) $ f (i+2) $ f (i+3) a
     _ -> error "bug in foldrBitsAux"
  where
    a = foldrBitsAux f z (i+4) (shiftR w 4)
    {-# INLINE a #-}

foldrBits' :: ∀ a w. (Bits w, Num w)
           => (Int -> a -> a) -> a -> w -> a
foldrBits' f z = foldrBitsAux' f z 0
{-# INLINE foldrBits' #-}

foldrBitsAux' :: ∀ a w. (Bits w, Num w)
               => (Int -> a -> a) -> a -> Int -> w -> a
foldrBitsAux' _ z _ 0 = z
foldrBitsAux' f z i w = case (i `seq` w) .&. 0x0F of
    0x00 -> a
    0x01 -> f i $! a
    0x02 -> f (i+1) $! a
    0x03 -> f i $! f (i+1) $! a
    0x04 -> f (i+2) $! a
    0x05 -> f i $! f (i+2) $! a
    0x06 -> f (i+1) $! f (i+2) $! a
    0x07 -> f i $! f (i+1) $! f (i+2) $! a
    0x08 -> f (i+3) $! a
    0x09 -> f i $! f (i+3) $! a
    0x0A -> f (i+1) $! f (i+3) $! a
    0x0B -> f i $! f (i+1) $! f (i+3) $! a
    0x0C -> f (i+2) $! f (i+3) $! a
    0x0D -> f i $! f (i+2) $! f (i+3) $! a
    0x0E -> f (i+1) $! f (i+2) $! f (i+3) $! a
    0x0F -> f i $! f (i+1) $! f (i+2) $! f (i+3) $! a
    _ -> error "bug in foldrBitsAux'"
  where
    a = foldrBitsAux' f z (i+4) (shiftR w 4)
    {-# INLINE a #-}

foldlBits :: ∀ a w. (Bits w, Num w)
          => (a -> Int -> a) -> a -> w -> a
foldlBits f z = foldlBitsAux f z 0
{-# INLINE foldlBits #-}

foldlBitsAux :: ∀ a w. (Bits w, Num w)
              => (a -> Int -> a) -> a -> Int -> w -> a
foldlBitsAux _ z _ 0 = z
foldlBitsAux f z i w = case (i `seq` w) .&. 0x0F of
    0x00 -> a z
    0x01 -> a $ f z i
    0x02 -> a $ f z (i+1)
    0x03 -> a $ f (f z i) (i+1)
    0x04 -> a $ f z (i+2)
    0x05 -> a $ f (f z i) (i+2)
    0x06 -> a $ f (f z (i+1)) (i+2)
    0x07 -> a $ f (f (f z i) (i+1)) (i+2)
    0x08 -> a $ f z (i+3)
    0x09 -> a $ f (f z i) (i+3)
    0x0A -> a $ f (f z (i+1)) (i+3)
    0x0B -> a $ f (f (f z i) (i+1)) (i+3)
    0x0C -> a $ f (f z (i+2)) (i+3)
    0x0D -> a $ f (f (f z i) (i+2)) (i+3)
    0x0E -> a $ f (f (f z (i+1)) (i+2)) (i+3)
    0x0F -> a $ f (f (f (f z i) (i+1)) (i+2)) (i+3)
    _ -> error "bug in foldlBitsAux"
  where
    a b = foldlBitsAux f b (i + 4) (shiftR w 4)
    {-# INLINE a #-}

foldlBits' :: ∀ a w. (Bits w, Num w)
           => (a -> Int -> a) -> a -> w -> a
foldlBits' f z = foldlBitsAux' (\x i -> x `seq` f x i) z 0
{-# INLINE foldlBits' #-}

foldlBitsAux' :: ∀ a w. (Bits w, Num w)
               => (a -> Int -> a) -> a -> Int -> w -> a
foldlBitsAux' _ z _ 0 = z
foldlBitsAux' f z i w = case (i `seq` w) .&. 0x0F of
    0x00 -> a $! z
    0x01 -> a $! f z i
    0x02 -> a $! f z (i+1)
    0x03 -> a $! f (f z i) (i+1)
    0x04 -> a $! f z (i+2)
    0x05 -> a $! f (f z i) (i+2)
    0x06 -> a $! f (f z (i+1)) (i+2)
    0x07 -> a $! f (f (f z i) (i+1)) (i+2)
    0x08 -> a $! f z (i+3)
    0x09 -> a $! f (f z i) (i+3)
    0x0A -> a $! f (f z (i+1)) (i+3)
    0x0B -> a $! f (f (f z i) (i+1)) (i+3)
    0x0C -> a $! f (f z (i+2)) (i+3)
    0x0D -> a $! f (f (f z i) (i+2)) (i+3)
    0x0E -> a $! f (f (f z (i+1)) (i+2)) (i+3)
    0x0F -> a $! f (f (f (f z i) (i+1)) (i+2)) (i+3)
    _ -> error "bug in foldlBitsAux"
  where
    a b = foldlBitsAux' f b (i + 4) (shiftR w 4)
    {-# INLINE a #-}
