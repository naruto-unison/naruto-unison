{-# LANGUAGE DeriveAnyClass #-}

module Model.Chakra
  ( Chakra(..)
  , Chakras(..)
  , total
  , collect
  , classes
  , lack
  , toSequence, toChakras
  , random
  ) where

import ClassyPrelude hiding (fromList, sum, toList)
import Prelude (sum)

import           Data.Aeson (ToJSON)
import           Data.Enum.Set.Class (AsEnumSet(..), EnumSet)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import           GHC.Exts (IsList(..))
import           Yesod.Core.Dispatch (PathPiece(..))

import qualified Class.Random as R
import           Class.Random (MonadRandom)
import           Model.Class (Class(..))

-- | Collection of all chakra types.
data Chakras = Chakras { blood :: Int -- ^ Bloodline
                       , gen   :: Int -- ^ Genjutsu
                       , nin   :: Int -- ^ Ninjutsu
                       , tai   :: Int -- ^ Taijutsu
                       , rand  :: Int -- ^ Random
                       } deriving (Eq, Show, Read, Generic, ToJSON)

instance IsList Chakras where
    type Item Chakras = Chakra
    toList = toSequence
    {-# INLINE toList #-}
    fromList = collect
    {-# INLINE fromList #-}

instance PathPiece Chakras where
  toPathPiece Chakras{..} = intercalate "," $ tshow <$> [blood, gen, nin, tai]
  fromPathPiece raw = case pieces of
      [b, g, n, t] -> case makeChakras b g n t of
                          Right chakras -> Just chakras
                          Left  _       -> Nothing
      _            -> Nothing
    where
      pieces              = Text.splitOn "," raw
      makeChakras b g n t = [Chakras b' g' n' t' 0 | (b',_) <- Read.decimal b
                                                   , (g',_) <- Read.decimal g
                                                   , (n',_) <- Read.decimal n
                                                   , (t',_) <- Read.decimal t
                                                   ]

map1 :: (Int -> Int) -> Chakras -> Chakras
map1 f (Chakras b g n t r) = Chakras (f b) (f g) (f n) (f t) (f r)
{-# INLINE map1 #-}

map2 :: (Int -> Int -> Int) -> Chakras -> Chakras -> Chakras
map2 f (Chakras b g n t r) (Chakras b' g' n' t' r') =
    Chakras (f b b') (f g g') (f n n') (f t t') (f r r')
{-# INLINE map2 #-}

instance Num Chakras where
    (+)    = map2 (+)
    {-# INLINE (+) #-}
    (-)    = map2 (-)
    {-# INLINE (-) #-}
    (*)    = map2 (*)
    {-# INLINE (*) #-}
    negate = map1 negate
    {-# INLINE negate #-}
    abs    = map1 abs
    {-# INLINE abs #-}
    signum = map1 signum
    {-# INLINE signum #-}
    fromInteger (fromInteger -> x) = Chakras x x x x x
    {-# INLINE fromInteger #-}

total :: Chakras -> Int
total (Chakras b g n t r) = b + g + n + t + r

-- Any component is negative.
lack :: Chakras -> Bool
lack (Chakras b g n t r) = b < 0 || g < 0 || n < 0 || t < 0 || r < 0

data Chakra
    = Blood -- ^ Bloodline
    | Gen -- ^ Genjutsu
    | Nin -- ^ Ninjutsu
    | Tai -- ^ Taijutsu
    | Rand -- ^ Random
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance AsEnumSet Chakra where
    type EnumSetRep Chakra = Word8

toChakras :: Chakra -> Chakras
toChakras Blood = 0 { blood = 1 }
toChakras Gen   = 0 { gen   = 1 }
toChakras Nin   = 0 { nin   = 1 }
toChakras Tai   = 0 { tai   = 1 }
toChakras Rand  = 0 { rand  = 1 }

toSequence :: ∀ m. (IsSequence m, Index m ~ Int, Element m ~ Chakra)
           => Chakras -> m
toSequence (Chakras b g n t r) = replicate b Blood
                              ++ replicate g Gen
                              ++ replicate n Nin
                              ++ replicate t Tai
                              ++ replicate r Rand

collect :: ∀ f. (Foldable f, Functor f) => f Chakra -> Chakras
collect xs = sum $ toChakras <$> xs

classes :: Chakras -> EnumSet Class
classes (Chakras b g n t r) = fromList $ fst <$> filter snd
                              [ (Bloodline, b > 0)
                              , (Genjutsu,  g > 0)
                              , (Ninjutsu,  n > 0)
                              , (Taijutsu,  t > 0)
                              , (Random,    r > 0)
                              ]

-- | Randomly selects a @Chakra@.
random :: ∀ m. MonadRandom m => m Chakra
random = toEnum <$> R.random (fromEnum @Chakra minBound)
                             (fromEnum @Chakra maxBound - 1)
