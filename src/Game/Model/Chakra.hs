{-# LANGUAGE DeriveAnyClass #-}

module Game.Model.Chakra
  ( Chakra(..), chakraDesc
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
import qualified Data.Attoparsec.Text as Parser
import           Data.Enum.Set (AsEnumSet(..), EnumSet)
import           GHC.Exts (IsList(..))
import           Text.Blaze ((!))
import           Text.Blaze (ToMarkup(..))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as HTML
import           Yesod.Core.Dispatch (PathPiece(..))

import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Game.Model.Class (Class(..))
import           Util (hushedParse)

-- | Collection of all chakra types.
data Chakras = Chakras { blood :: Int -- ^ Bloodline
                       , gen   :: Int -- ^ Genjutsu
                       , nin   :: Int -- ^ Ninjutsu
                       , tai   :: Int -- ^ Taijutsu
                       , rand  :: Int -- ^ Random
                       } deriving (Eq, Show, Read, Generic, ToJSON)

instance Ord Chakras where
    compare = comparing total
    {-# INLINE compare #-}

instance IsList Chakras where
    type Item Chakras = Chakra
    toList = toSequence
    {-# INLINE toList #-}
    fromList = collect
    {-# INLINE fromList #-}

instance ToMarkup Chakras where
    toMarkup = concatMap toMarkup . toList


instance PathPiece Chakras where
    toPathPiece Chakras{..} = intercalate "," $ tshow <$> [blood, gen, nin, tai]

    fromPathPiece =
        hushedParse $ Chakras
            <$> Parser.decimal
            <*> (Parser.char ',' >> Parser.decimal)
            <*> (Parser.char ',' >> Parser.decimal)
            <*> (Parser.char ',' >> Parser.decimal)
            <*> return 0
            <* Parser.endOfInput

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

-- | Sum of all components.
total :: Chakras -> Int
total (Chakras b g n t r) = b + g + n + t + r

-- | Any component is negative.
lack :: Chakras -> Bool
lack (Chakras b g n t r) = b < 0 || g < 0 || n < 0 || t < 0 || r < 0

-- | Units of @Game.Model.Skill.cost@.
data Chakra
    = Blood -- ^ Bloodline
    | Gen -- ^ Genjutsu
    | Nin -- ^ Ninjutsu
    | Tai -- ^ Taijutsu
    | Rand -- ^ Random
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance AsEnumSet Chakra

instance ToMarkup Chakra where
    toMarkup Blood = HTML.div ! HTML.class_ "chakra blood" $ mempty
    toMarkup Gen   = HTML.div ! HTML.class_ "chakra gen"   $ mempty
    toMarkup Nin   = HTML.div ! HTML.class_ "chakra nin"   $ mempty
    toMarkup Tai   = HTML.div ! HTML.class_ "chakra tai"   $ mempty
    toMarkup Rand  = HTML.div ! HTML.class_ "chakra rand"  $ mempty

-- | Lower-case name of chakra for use in descriptions, etc.
chakraDesc :: Chakra -> Text
chakraDesc Blood = "bloodline"
chakraDesc Gen   = "genjutsu"
chakraDesc Nin   = "ninjutsu"
chakraDesc Tai   = "taijutsu"
chakraDesc Rand  = "random"

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
random = toEnum <$> R.random (fromEnum (minBound :: Chakra))
                             (fromEnum (maxBound :: Chakra) - 1)
