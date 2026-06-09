-- DO NOT EXPOSE ANY FUNCTION THAT COULD BE USED TO CONSTRUCT OR ALTER A SLOT.
-- It must be guaranteed that all Slots are within their 'Bounded' range.
module Game.Model.Slot
  ( Slot, toInt
  , teamSize
  , all, allies, enemies
  , toChar
  , SlotSet
  ) where

import ClassyPrelude hiding (all)

import           Data.Aeson (ToJSON)
import           Data.Bits
import           Data.Enum.Set.Base (EnumSet)
import qualified Data.Enum.Set.Base as EnumSet
import           GHC.Exts (IsList)
import qualified GHC.Exts
import           Text.Read
import           System.Random.Stateful (Uniform(..), UniformRange(..))

import           Class.Display (Display)
import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Parse (Parse)
import qualified Class.Parse as Parse

teamSize :: Int
teamSize = 3

maxVal :: Int
maxVal = teamSize * 2 - 1

-- | A @Slot@ represents an index in 'Model.Game.ninjas'.
-- It is hidden behind a newtype and cannot be constructed or modified outside
-- this module in order to prevent out-of-bound errors.
-- This has the added advantage of making function signatures more readable!
newtype Slot = Slot { toInt :: Int }
    deriving (Eq, Ord, Show, Display, Hashable, ToJSON)

trySlot :: ∀ f. Alternative f => Int -> f Slot
trySlot i
  | i >= 0 && i <= maxVal = pure $ Slot i
  | otherwise             = empty

instance Parity Slot where
    even (Slot x) = x < teamSize
    {-# INLINE even #-}

instance Read Slot where
    readPrec = trySlot =<< readPrec

instance Bounded Slot where
    minBound = Slot 0
    maxBound = Slot maxVal

instance Uniform Slot where
    uniformM = uniformRM (minBound, maxBound)
    {-# INLINE uniformM #-}

instance UniformRange Slot where
    uniformRM (Slot a, Slot b) g = Slot . fromEnum
                                  <$> uniformRM @Word32 (toEnum a, toEnum b) g
    {-# INLINE uniformRM #-}

all :: [Slot]
all = Slot <$> [0..maxVal]

-- | Slots with the same parity.
allies :: ∀ a. Parity a => a -> [Slot]
allies x
  | Parity.even x = Slot <$> [0..teamSize - 1]
  | otherwise     = Slot <$> [teamSize..maxVal]

-- | Slots with opposite parity.
enemies :: ∀ a. Parity a => a -> [Slot]
enemies x
  | Parity.even x = Slot <$> [teamSize..maxVal]
  | otherwise     = Slot <$> [0..teamSize - 1]

instance Parse Slot where
    parser = trySlot =<< Parse.parser

toChar :: Slot -> Char
toChar (Slot x) = toEnum $ x + 48


newtype SlotSet = SlotSet { toEnumSet :: EnumSet Word Int }
    deriving (Eq, Ord, Semigroup, Monoid)

type instance Element SlotSet = Slot

instance MonoPointed SlotSet where
    opoint = SlotSet . opoint . toInt
    {-# INLINE opoint #-}

instance IsList SlotSet where
    type Item SlotSet = Slot
    fromList = fromFoldable
    toList = otoList

instance MonoFunctor SlotSet where
    omap = wrapSet . omap . mapSlot
    {-# INLINE omap #-}

instance MonoFoldable SlotSet where
    ofoldMap f = ofoldMap (wrapSlot f) . toEnumSet
    {-# INLINE ofoldMap #-}
    ofoldr f acc = ofoldr (wrapSlot f) acc . toEnumSet
    {-# INLINE ofoldr #-}
    ofoldl' f acc = ofoldl' (\acc' x -> f acc' (Slot x)) acc . toEnumSet
    {-# INLINE ofoldl' #-}
    ofoldr1Ex f = Slot . ofoldr1Ex (mapSlot2 f) . toEnumSet
    {-# INLINE ofoldr1Ex #-}
    ofoldl1Ex' f = Slot . ofoldl1Ex' (mapSlot2 f) . toEnumSet
    {-# INLINE ofoldl1Ex' #-}
    otoList = wrapList otoList
    {-# INLINE otoList #-}
    oall f = oall (wrapSlot f) . toEnumSet
    {-# INLINE oall #-}
    oany f = oany (wrapSlot f) . toEnumSet
    {-# INLINE oany #-}
    onull = onull . toEnumSet
    {-# INLINE onull #-}
    olength = olength . toEnumSet
    {-# INLINE olength #-}
    olength64 = olength64 . toEnumSet
    {-# INLINE olength64 #-}
    headEx = Slot . headEx . toEnumSet
    {-# INLINE headEx #-}
    lastEx = Slot . lastEx . toEnumSet
    {-# INLINE lastEx #-}
    oelem x = oelem (toInt x) . toEnumSet
    {-# INLINE oelem #-}
    onotElem x = onotElem (toInt x) . toEnumSet
    {-# INLINE onotElem #-}

instance GrowingAppend SlotSet

instance MonoTraversable SlotSet where
    otraverse f xs = SlotSet <$> otraverse
                     (\x -> toInt <$> f (Slot x)) (toEnumSet xs)
    {-# INLINE otraverse #-}

instance SetContainer SlotSet where
    type ContainerKey SlotSet = Slot
    member x = member (toInt x) . toEnumSet
    {-# INLINE member #-}
    notMember x = notMember (toInt x) . toEnumSet
    {-# INLINE notMember #-}
    union = wrapSet2 union
    {-# INLINE union #-}
    difference = wrapSet2 difference
    {-# INLINE difference #-}
    intersection = wrapSet2 intersection
    {-# INLINE intersection #-}
    keys = wrapList keys
    {-# INLINE keys #-}

instance IsSet SlotSet where
    insertSet = wrapSet . insertSet . toInt
    {-# INLINE insertSet #-}
    deleteSet = wrapSet . deleteSet . toInt
    {-# INLINE deleteSet #-}
    singletonSet = SlotSet . singletonSet . toInt
    {-# INLINE singletonSet #-}
    setFromList = fromFoldable
    {-# INLINE setFromList #-}
    setToList = wrapList setToList
    {-# INLINE setToList #-}
    filterSet = wrapSet . filterSet . wrapSlot
    {-# INLINE filterSet #-}

instance Show SlotSet where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)
    {-# INLINABLE showsPrec #-}

fromFoldable :: ∀ o. (MonoFoldable o, Slot ~ Element o) => o -> SlotSet
fromFoldable = SlotSet . EnumSet.fromRaw
    . foldl' (flip $ (.|.) . bit  . toInt) 0
{-# INLINE fromFoldable #-}

mapSlot :: (Slot -> Slot) -> Int -> Int
mapSlot f = toInt . f . Slot
{-# INLINE mapSlot #-}

mapSlot2 :: (Slot -> Slot -> Slot) -> Int -> Int -> Int
mapSlot2 f a b = toInt $ f (Slot a) (Slot b)
{-# INLINE mapSlot2 #-}

wrapSet :: (EnumSet Word Int -> EnumSet Word Int) -> SlotSet -> SlotSet
wrapSet f = SlotSet . f . toEnumSet
{-# INLINE wrapSet #-}

wrapSet2 :: (EnumSet Word Int -> EnumSet Word Int -> EnumSet Word Int)
         -> SlotSet -> SlotSet -> SlotSet
wrapSet2 f (SlotSet x) (SlotSet y) = SlotSet $ f x y
{-# INLINE wrapSet2 #-}

wrapSlot :: ∀ a. (Slot -> a) -> Int -> a
wrapSlot f = f . Slot
{-# INLINE wrapSlot #-}

wrapList :: (EnumSet Word Int -> [Int]) -> SlotSet -> [Slot]
wrapList f = (Slot <$>) . f . toEnumSet
{-# INLINE wrapList #-}
