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
module Core.EnumSet
  ( -- * Set type
    EnumSet

  -- * CollX operations
  , empty
  , singleton
  , fromFoldable
  , toList
  , insert
  , union
  , delete
  , null
  , size
  , member
  , all
  , any
  , strict

  -- * OrdCollX operations
  , deleteMin
  , deleteMax
  , filterLT
  , filterLE
  , filterGT
  , filterGE
  , partitionLT_GE
  , partitionLE_GT
  , partitionLT_GT

  -- * SetX operations
  , intersection
  , difference
  , symmetricDifference
  , subset

  -- * Coll operations
  , fold, fold', fold1, fold1'
  , filter
  , partition

  -- * OrdColl operations
  , minView
  , minElem
  , maxView
  , maxElem
  , foldr, foldr', foldl, foldl'
  , foldr1, foldr1', foldl1, foldl1'
  , foldMap

  -- * Bonus operations
  , map
  , toBits
  , appropriate
  ) where

import qualified ClassyPrelude
import qualified GHC.Exts
import qualified Data.Foldable as F

import ClassyPrelude hiding (all, any, delete, difference, empty, filter, fold, foldl', foldMap, foldr, fromList, intersection, null, map, member, lookup, partition, singleton, toList, union)
import Data.Aeson (ToJSON(..))
import Data.Bits
import GHC.Exts (IsList(Item))
import Data.Monoid (Monoid(..))

-- | A set of values @a@ with representation @word@,
-- implemented as bitwise operations.
newtype EnumSet word a = EnumSet { toBits :: word } deriving (Eq)

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
    headEx = minElem
    {-# INLINE headEx #-}
    lastEx = maxElem
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
    notMember x = not . member x
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

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is this the empty set?
null :: ∀ w a. Bits w
     => EnumSet w a -> Bool
null (EnumSet w) = zeroBits == w

-- | /O(1)/. The number of elements in the set.
size :: ∀ w a. (Bits w, Num w)
     => EnumSet w a -> Int
size (EnumSet w) = countBits w

-- | /O(1)/. Is the element in the set?
member :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> Bool
member x (EnumSet w) = testBit w $ fromEnum x

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
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty set.
empty :: ∀ w a. Bits w
      => EnumSet w a
empty = EnumSet zeroBits

-- | /O(1)/. Create a singleton set.
singleton :: ∀ w a. (Bits w, Enum a)
          => a -> EnumSet w a
singleton = EnumSet . setBit zeroBits . fromEnum

fromFoldable :: ∀ f w a. (Foldable f, Bits w, Enum a)
             => f a -> EnumSet w a
fromFoldable = EnumSet . F.foldl' f zeroBits
  where
    f z = setBit z . fromEnum

toList :: ∀ w a. (Bits w, Num w, Enum a)
       => EnumSet w a -> [a]
toList (EnumSet w) = foldrBits ((:) . toEnum) [] w

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(1)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> EnumSet w a
insert x (EnumSet w) = EnumSet . setBit w $ fromEnum x

-- | /O(1)/. Delete an element from a set.
delete :: ∀ w a. (Bits w, Enum a)
       => a -> EnumSet w a -> EnumSet w a
delete x (EnumSet w) = EnumSet . clearBit w $ fromEnum x

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}

-- | /O(1)/. Is this a subset?
-- @(s1 `subset` s2)@ tells whether @s1@ is a subset of @s2@.
subset :: ∀ w a. (Bits w)
       => EnumSet w a -> EnumSet w a -> Bool
subset x y = x `union` y == y

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(1)/. The minimal element of a non-empty set.
minElem :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> a
minElem (EnumSet w) = toEnum $ lsb w

-- | /O(1)/. The maximal element of a non-empty set.
maxElem :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> a
maxElem (EnumSet w) = toEnum $ msb w

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

minView :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> Maybe (a, EnumSet w a)
minView (EnumSet w)
  | w == 0    = Nothing
  | otherwise = let i = lsb w in Just (toEnum i, EnumSet $ clearBit w i)

maxView :: ∀ w a. (Bits w, Num w, Enum a)
        => EnumSet w a -> Maybe (a, EnumSet w a)
maxView (EnumSet w)
  | w == 0    = Nothing
  | otherwise = let i = msb w in Just (toEnum i, EnumSet $ clearBit w i)

filterLT :: ∀ w a. (Bits w, Num w, Enum a)
         => a -> EnumSet w a -> EnumSet w a
filterLT x (EnumSet w) = EnumSet (w .&. lowMask (fromEnum x))

filterLE :: ∀ w a. (Bits w, Num w, Enum a)
         => a -> EnumSet w a -> EnumSet w a
filterLE x (EnumSet w) = EnumSet (w .&. lowMask (fromEnum x + 1))

filterGT :: ∀ w a. (Bits w, Num w, Enum a)
         => a -> EnumSet w a -> EnumSet w a
filterGT x (EnumSet w) = EnumSet (w .&. highMask (fromEnum x + 1))

filterGE :: ∀ w a. (Bits w, Num w, Enum a)
         => a -> EnumSet w a -> EnumSet w a
filterGE x (EnumSet w) = EnumSet (w .&. highMask (fromEnum x))

partitionLT_GE :: ∀ w a. (Bits w, Num w, Enum a)
               => a -> EnumSet w a -> (EnumSet w a, EnumSet w a)
partitionLT_GE x s = (filterLT x s, filterGE x s)

partitionLE_GT :: ∀ w a. (Bits w, Num w, Enum a)
               => a -> EnumSet w a -> (EnumSet w a, EnumSet w a)
partitionLE_GT x s = (filterLE x s, filterGT x s)

partitionLT_GT :: ∀ w a. (Bits w, Num w, Enum a)
               => a -> EnumSet w a -> (EnumSet w a, EnumSet w a)
partitionLT_GT x s = (filterLT x s, filterGT x s)

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | /O(1)/. The union of two sets.
union :: ∀ w a. Bits w
      => EnumSet w a -> EnumSet w a -> EnumSet w a
union (EnumSet x) (EnumSet y) = EnumSet $ x .|. y

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(1)/. Difference of two sets.
difference :: ∀ w a. Bits w
           => EnumSet w a -> EnumSet w a -> EnumSet w a
difference (EnumSet x) (EnumSet y) = EnumSet $ (x .|. y) `xor` y

symmetricDifference :: ∀ w a. Bits w
                    => EnumSet w a -> EnumSet w a -> EnumSet w a
symmetricDifference (EnumSet x) (EnumSet y) = EnumSet $ x `xor` y

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(1)/. The intersection of two sets.
intersection :: ∀ w a. Bits w
             => EnumSet w a -> EnumSet w a -> EnumSet w a
intersection (EnumSet x) (EnumSet y) = EnumSet $ x .&. y

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> Bool) -> EnumSet w a -> EnumSet w a
filter p (EnumSet w) = EnumSet $ foldlBits' f 0 w
    where
      f z i
        | p $ toEnum i = setBit z i
        | otherwise = z

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: ∀ w a. (Bits w, Num w, Enum a)
          => (a -> Bool) -> EnumSet w a -> (EnumSet w a, EnumSet w a)
partition p (EnumSet w) = (EnumSet yay, EnumSet nay)
    where
      (yay,nay) = foldlBits' f (0, 0) w
      f (x, y) i
          | p $ toEnum i = (setBit x i, y)
          | otherwise    = (x, setBit y i)

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}
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

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

fold :: ∀ w a c. (Bits w, Num w, Enum a)
     => (a -> c -> c) -> c -> EnumSet w a -> c
fold f z (EnumSet w) = foldrBits (f . toEnum) z w

fold' :: ∀ w a c. (Bits w, Num w, Enum a)
      => (a -> c -> c) -> c -> EnumSet w a -> c
fold' f z (EnumSet w) = foldrBits' (f . toEnum) z w

fold1 :: ∀ w a. (Bits w, Num w, Enum a)
      => (a -> a -> a) -> EnumSet w a -> a
fold1 f (EnumSet w) = foldrBits (f . toEnum) (toEnum maxi) (clearBit w maxi)
    where
      maxi = msb w

fold1' :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
fold1' f (EnumSet w) = foldrBits' (f . toEnum) (toEnum maxi) (clearBit w maxi)
    where
      maxi = msb w

foldr :: (Bits w, Num w, Enum a)
      => (a -> b -> b) -> b -> EnumSet w a -> b
foldr f z (EnumSet w) = foldrBits (f . toEnum) z w

foldr' :: ∀ w a b. (Bits w, Num w,  Enum a)
       => (a -> b -> b) -> b -> EnumSet w a -> b
foldr' f z (EnumSet w) = foldrBits' (f . toEnum) z w

foldr1 :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
foldr1 f (EnumSet w) = foldrBits (f . toEnum) (toEnum maxi) (clearBit w maxi)
    where
      maxi = msb w

foldr1' :: ∀ w a. (Bits w, Num w, Enum a)
        => (a -> a -> a) -> EnumSet w a -> a
foldr1' f (EnumSet w) = foldrBits' (f . toEnum) (toEnum maxi) (clearBit w maxi)
    where
      maxi = msb w

foldl :: ∀ w a c. (Bits w, Num w, Enum a)
      => (c -> a -> c) -> c -> EnumSet w a -> c
foldl f z (EnumSet w) = foldlBits folder z w
  where
    folder h = f h . toEnum

foldl' :: ∀ w a c. (Bits w, Num w, Enum a)
       => (c -> a -> c) -> c -> EnumSet w a -> c
foldl' f z (EnumSet w) = foldlBits' folder z w
  where
    folder h = f h . toEnum


foldl1 :: ∀ w a. (Bits w, Num w, Enum a)
       => (a -> a -> a) -> EnumSet w a -> a
foldl1 f (EnumSet w) = foldlBits folder (toEnum mininum) (clearBit w mininum)
  where
    mininum = lsb w
    folder z = f z . toEnum

foldl1' :: ∀ w a. (Bits w, Num w, Enum a)
        => (a -> a -> a) -> EnumSet w a -> a
foldl1' f (EnumSet w) = foldlBits' folder (toEnum mininum) (clearBit w mininum)
  where
    mininum = lsb w
    folder z = f z . toEnum

foldMap :: ∀ m w a. (Monoid m, Bits w, Num w, Enum a)
        => (a -> m) -> EnumSet w a -> m
foldMap f (EnumSet w) = foldrBits (mappend . f . toEnum) mempty w

{----------------------------------------------------------------
  Strictness enhancement
----------------------------------------------------------------}

strict :: ∀ w a. EnumSet w a -> EnumSet w a
strict s@(EnumSet w) = w `seq` s

{----------------------------------------------------------------
  Representation checkking
----------------------------------------------------------------}

appropriate :: ∀ w a. (FiniteBits w, Enum a, Bounded a) => EnumSet w a -> Bool
appropriate = const $ fromEnum (maxBound :: a) <= finiteBitSize (zeroBits :: w)

{--------------------------------------------------------------------
  Utility functions
--------------------------------------------------------------------}

countBits :: ∀ w i. (Bits w, Num w, Num i) => w -> i
countBits w = w `seq` bitcount 0 w

bitcount :: ∀ w i. (Bits w, Num w, Num i) => i -> w -> i
bitcount a 0 = a
bitcount a x = a `seq` bitcount (a+1) (x .&. (x - 1))

-- stolen from http://aggregate.org/MAGIC/
lsb :: ∀ w. (Bits w, Num w) => w -> Int
lsb x = countBits $ (x - 1) .&. complement x

msb :: ∀ w. (Bits w, Num w) => w -> Int
msb x0 = let
     x1 = x0 .|. (x0 `shiftR` 1)
     x2 = x1 .|. (x1 `shiftR` 2)
     x3 = x2 .|. (x2 `shiftR` 4)
     x4 = x3 .|. (x3 `shiftR` 8)
     x5 = x4 .|. (x4 `shiftR` 16)
     in countBits x5 - 1

lowMask :: ∀ w. (Bits w, Num w) => Int -> w
lowMask x = bit x - 1

highMask :: ∀ w. (Bits w, Num w) => Int -> w
highMask = complement . lowMask

foldrBits :: ∀ w a. (Bits w, Num w)
          => (Int -> a -> a) -> a -> w -> a
foldrBits f z = foldrBits_aux f z 0

foldrBits_aux :: ∀ a w. (Bits w, Num w)
              => (Int -> a -> a) -> a -> Int -> w -> a
foldrBits_aux _ z _ 0 = z
foldrBits_aux f z i w = case (i `seq` w) .&. 0x0F of
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
     _ -> error "bug in foldrBits_aux"
 where a = foldrBits_aux f z (i+4) (shiftR w 4)


foldrBits' :: ∀ a w. (Bits w, Num w)
           => (Int -> a -> a) -> a -> w -> a
foldrBits' f z = foldrBits_aux' f z 0

foldrBits_aux' :: ∀ a w. (Bits w, Num w)
               => (Int -> a -> a) -> a -> Int -> w -> a
foldrBits_aux' _ z _ 0 = z
foldrBits_aux' f z i w = case (i `seq` w) .&. 0x0F of
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
    _ -> error "bug in foldrBits_aux'"
 where a = foldrBits_aux' f z (i+4) (shiftR w 4)


foldlBits :: ∀ a w. (Bits w, Num w)
          => (a -> Int -> a) -> a -> w -> a
foldlBits f z = foldlBits_aux f z 0

foldlBits_aux :: ∀ a w. (Bits w, Num w)
              => (a -> Int -> a) -> a -> Int -> w -> a
foldlBits_aux _ z _ 0 = z
foldlBits_aux f z i w = case (i `seq` w) .&. 0x0F of
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
    _ -> error "bug in foldlBits_aux"
 where a b = foldlBits_aux f b (i + 4) (shiftR w 4)

foldlBits' :: ∀ a w. (Bits w, Num w)
           => (a -> Int -> a) -> a -> w -> a
foldlBits' f z = foldlBits_aux' (\x i -> x `seq` f x i) z 0

foldlBits_aux' :: ∀ a w. (Bits w, Num w)
               => (a -> Int -> a) -> a -> Int -> w -> a
foldlBits_aux' _ z _ 0 = z
foldlBits_aux' f z i w = case (i `seq` w) .&. 0x0F of
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
    _ -> error "bug in foldlBits_aux"
  where a b = foldlBits_aux' f b (i + 4) (shiftR w 4)
