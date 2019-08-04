{-# LANGUAGE DeriveAnyClass #-}

module Model.Chakra
  ( Chakra(..)
  , Chakras(..)
  , total
  , collect
  , classes
  , lack
  , fromChakras
  , random
  ) where

import ClassyPrelude hiding (fromList, sum, toList)
import Prelude (sum)

import           Data.Aeson (ToJSON)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import           GHC.Exts (IsList(..))
import           Yesod.Core.Dispatch (PathPiece(..))

import qualified Class.Random as R
import           Class.Random (MonadRandom)
import           Model.Class (Class(..), ClassSet)

-- | Collection of all chakra types.
data Chakras = Chakras { blood :: Int -- ^ Bloodline
                       , gen   :: Int -- ^ Genjutsu
                       , nin   :: Int -- ^ Ninjutsu
                       , tai   :: Int -- ^ Taijutsu
                       , rand  :: Int -- ^ Random
                       } deriving (Eq, Show, Read, Generic, ToJSON)

instance IsList Chakras where
    type Item Chakras = Chakra
    toList = fromChakras
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

map2 :: (Int -> Int -> Int) -> Chakras -> Chakras -> Chakras
map2 f (Chakras b g n t r) (Chakras b' g' n' t' r') =
    Chakras (f b b') (f g g') (f n n') (f t t') (f r r')

instance Num Chakras where
    (+)    = map2 (+)
    (-)    = map2 (-)
    (*)    = map2 (*)
    negate = map1 negate
    abs    = map1 abs
    signum = map1 signum
    fromInteger (fromInteger -> x) = Chakras x x x x x

total :: Chakras -> Int
total (Chakras b g n t r) = b + g + n + t + r

-- Any component is negative.
lack :: Chakras -> Bool
lack (Chakras b g n t r) = b < 0 || g < 0 || n < 0 || t < 0 || r < 0

data Chakra
    = Blood
    | Gen
    | Nin
    | Tai
    | Rand
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

toChakras :: Chakra -> Chakras
toChakras Blood = 0 { blood = 1 }
toChakras Gen   = 0 { gen   = 1 }
toChakras Nin   = 0 { nin   = 1 }
toChakras Tai   = 0 { tai   = 1 }
toChakras Rand  = 0 { rand  = 1 }

fromChakras :: ∀ m. (IsSequence m, Index m ~ Int, Element m ~ Chakra)
            => Chakras -> m
fromChakras (Chakras b g n t _) = replicate b Blood
                               ++ replicate g Gen
                               ++ replicate n Nin
                               ++ replicate t Tai

collect :: ∀ f. (Foldable f, Functor f) => f Chakra -> Chakras
collect = sum . (toChakras <$>)

classes :: Chakras -> ClassSet
classes (Chakras b g n t r) = fromList $ fst <$> filter snd
                              [ (Bloodline, b > 0)
                              , (Genjutsu,  g > 0)
                              , (Ninjutsu,  n > 0)
                              , (Taijutsu,  t > 0)
                              , (Random,    r > 0)
                              ]

-- | Randomly selects a 'Chakra'.
random :: ∀ m. MonadRandom m => m Chakra
random = toEnum <$> R.random (fromEnum (minBound :: Chakra))
                             (fromEnum (maxBound :: Chakra) - 1)
