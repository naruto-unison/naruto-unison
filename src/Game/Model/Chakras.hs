module Game.Model.Chakras
  ( Chakra(..), chakraDesc
  , Chakras(..)
  , classes
  , scale
  , spend
  , checkedSpend
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON)
import           Data.Bits
import           Data.Enum.Set (AsEnumSet(..), EnumSet)
import           GHC.Exts (IsList)
import qualified GHC.Exts
import           System.Random.Stateful (Uniform(..), UniformRange(..))
import qualified System.Random.Stateful as R
import           Text.Blaze ((!), ToMarkup(..))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as HTML
import           Yesod.Core.Dispatch (PathPiece(..))

import           Class.Parse (Parse(..))
import qualified Class.Parse as Parse
import           Game.Model.Class (Class(..))
import           Util (rightToMaybe)

-- | Collection of all chakra types.
data Chakras = Chakras
    { blood :: {-# UNPACK #-} Int -- ^ Bloodline
    , gen   :: {-# UNPACK #-} Int -- ^ Genjutsu
    , nin   :: {-# UNPACK #-} Int -- ^ Ninjutsu
    , tai   :: {-# UNPACK #-} Int -- ^ Taijutsu
    , rand  :: {-# UNPACK #-} Int -- ^ Random
    } deriving (Eq, Show, Read, Generic)

naiveSubtract :: Chakras -> Chakras -> Chakras
naiveSubtract (Chakras b g n t r) (Chakras b' g' n' t' r') =
    Chakras (b - b') (g - g') (n - n') (t - t') (r - r')
{-# INLINE naiveSubtract #-}

instance IsList Chakras where
    type Item Chakras = Chakra
    toList = otoList
    {-# INLINE toList #-}
    fromList chakras = concatMap singleton chakras
    {-# INLINE fromList #-}

instance ToJSON Chakras

instance ToMarkup Chakras where
    toMarkup = concatMap toMarkup . toList

instance Semigroup Chakras where
    Chakras b g n t r <> Chakras b' g' n' t' r' =
        Chakras (b + b') (g + g') (n + n') (t + t') (r + r')
    {-# INLINE (<>) #-}

instance Monoid Chakras where
    mempty = Chakras 0 0 0 0 0
    {-# INLINE mempty #-}

type instance Element Chakras = Chakra

instance MonoFoldable Chakras where
    otoList (Chakras b g n t r) = replicate b Blood
                               ++ replicate g Gen
                               ++ replicate n Nin
                               ++ replicate t Tai
                               ++ replicate r Rand
    {-# INLINABLE otoList #-}
    oall f (Chakras b g n t r) = (b == 0 || f Blood)
                              && (g == 0 || f Gen)
                              && (n == 0 || f Nin)
                              && (t == 0 || f Tai)
                              && (r == 0 || f Rand)
    {-# INLINABLE oall #-}
    oany f (Chakras b g n t r) = (b /= 0 && f Blood)
                              || (g /= 0 && f Gen)
                              || (n /= 0 && f Nin)
                              || (t /= 0 && f Tai)
                              || (r /= 0 && f Rand)
    {-# INLINABLE oany #-}
    onull (Chakras 0 0 0 0 0) = True
    onull _                   = False
    {-# INLINE onull #-}
    olength (Chakras b g n t r) = b + g + n + t + r
    {-# INLINE olength #-}
    olength64 = toEnum . olength
    {-# INLINE olength64 #-}
    ofoldr1Ex f = ofoldr1Ex f . otoList
    {-# INLINE ofoldr1Ex #-}
    ofoldl1Ex' f = ofoldl1Ex' f . otoList
    {-# INLINE ofoldl1Ex' #-}
    ofoldMap f = ofoldMap f . otoList
    {-# INLINE ofoldMap #-}
    ofoldr f acc = ofoldr f acc . otoList
    {-# INLINE ofoldr #-}
    ofoldl' f acc = ofoldl' f acc . otoList
    {-# INLINE ofoldl' #-}

instance MonoFunctor Chakras where
    omap f (Chakras b g n t r) = replicate b (f Blood)
                              ++ replicate g (f Gen)
                              ++ replicate n (f Nin)
                              ++ replicate t (f Tai)
                              ++ replicate r (f Rand)
    {-# INLINABLE omap #-}

instance MonoPointed Chakras where
    opoint Blood = Chakras 1 0 0 0 0
    opoint Gen   = Chakras 0 1 0 0 0
    opoint Nin   = Chakras 0 0 1 0 0
    opoint Tai   = Chakras 0 0 0 1 0
    opoint Rand  = Chakras 0 0 0 0 1
    {-# INLINABLE opoint #-}

instance GrowingAppend Chakras

instance SemiSequence Chakras where
    type Index Chakras = Int
    reverse chakras = chakras
    {-# INLINE reverse #-}
    sortBy _ chakras = chakras
    {-# INLINE sortBy #-}
    cons chakra chakras = chakras ++ singleton chakra
    {-# INLINE cons #-}
    snoc chakras chakra = chakras ++ singleton chakra
    {-# INLINE snoc #-}
    intersperse chakra chakras
        | len < 2   = chakras
        | otherwise = chakras ++ replicate (len - 1) chakra
      where
        len = length chakras
    find f (Chakras b g n t r)
        | b /= 0 && f Blood = Just Blood
        | g /= 0 && f Gen   = Just Gen
        | n /= 0 && f Nin   = Just Nin
        | t /= 0 && f Tai   = Just Tai
        | r /= 0 && f Rand  = Just Rand
        | otherwise         = Nothing
    {-# INLINABLE find #-}

instance MonoTraversable Chakras where
    otraverse f chakras = fromList <$> otraverse f (toList chakras)
    {-# INLINABLE otraverse #-}
    omapM f chakras = fromList <$> omapM f (toList chakras)
    {-# INLINABLE omapM #-}

instance IsSequence Chakras where
    fromList chakras = concatMap singleton chakras
    {-# INLINE fromList #-}
    lengthIndex      = length
    {-# INLINE lengthIndex #-}

    replicate b Blood = Chakras b 0 0 0 0
    replicate g Gen   = Chakras 0 g 0 0 0
    replicate n Nin   = Chakras 0 0 n 0 0
    replicate t Tai   = Chakras 0 0 0 t 0
    replicate r Rand  = Chakras 0 0 0 0 r
    {-# INLINABLE replicate #-}

    replicateM n f = replicate n <$> f
    {-# INLINABLE replicateM #-}

    span f (Chakras b g n t r)
        | b /= 0 && f Blood = (Chakras 0 0 0 0 0, Chakras b g n t r)
        | g /= 0 && f Gen   = (Chakras b 0 0 0 0, Chakras 0 g n t r)
        | n /= 0 && f Nin   = (Chakras b g 0 0 0, Chakras 0 0 n t r)
        | t /= 0 && f Tai   = (Chakras b g n 0 0, Chakras 0 0 0 t r)
        | r /= 0 && f Rand  = (Chakras b g n t 0, Chakras 0 0 0 0 r)
        | otherwise         = (Chakras b g n t r, Chakras 0 0 0 0 0)

    break f chakras = span (not . f) chakras

    dropWhile f (Chakras b g n t r)
        | b /= 0 && not (f Blood) = Chakras b g n t r
        | g /= 0 && not (f Gen)   = Chakras 0 g n t r
        | n /= 0 && not (f Nin)   = Chakras 0 0 n t r
        | t /= 0 && not (f Tai)   = Chakras 0 0 0 t r
        | r /= 0 && not (f Rand)  = Chakras 0 0 0 0 r
        | otherwise               = Chakras 0 0 0 0 0

    takeWhile f (Chakras b g n t r)
        | b /= 0 && not (f Blood) = Chakras 0 0 0 0 0
        | g /= 0 && not (f Gen)   = Chakras b 0 0 0 0
        | n /= 0 && not (f Nin)   = Chakras b g 0 0 0
        | t /= 0 && not (f Tai)   = Chakras b g n 0 0
        | r /= 0 && not (f Rand)  = Chakras b g n t 0
        | otherwise               = Chakras b g n t r

    filter f (Chakras b g n t r) = Chakras (filt b Blood)
                                           (filt g Gen)
                                           (filt n Nin)
                                           (filt t Tai)
                                           (filt r Rand)
      where
        filt amount chakra
            | amount /= 0 && f chakra = amount
            | otherwise               = 0
    {-# INLINABLE filter #-}

    partition f chakras = (yays, chakras `naiveSubtract` yays)
      where
        yays = filter f chakras
    {-# INLINABLE partition #-}

    filterM f (Chakras b g n t r) = Chakras
        <$> filterIf Blood b
        <*> filterIf Gen   g
        <*> filterIf Nin   n
        <*> filterIf Tai   t
        <*> filterIf Rand  r
      where
        filterIf _      0      = return 0
        filterIf chakra amount = getFiltered <$> f chakra
          where
            getFiltered True  = amount
            getFiltered False = 0

    uncons (Chakras 0 0 0 0 0) = Nothing
    uncons (Chakras 0 0 0 0 r) = Just (Rand,  Chakras 0 0 0 0 (r - 1))
    uncons (Chakras 0 0 0 t r) = Just (Tai,   Chakras 0 0 0 (t - 1) r)
    uncons (Chakras 0 0 n t r) = Just (Nin,   Chakras 0 0 (n - 1) t r)
    uncons (Chakras 0 g n t r) = Just (Gen,   Chakras 0 (g - 1) n t r)
    uncons (Chakras b g n t r) = Just (Blood, Chakras (b - 1) g n t r)
    {-# INLINABLE uncons #-}

    unsnoc (Chakras 0 0 0 0 0) = Nothing
    unsnoc (Chakras b 0 0 0 0) = Just (Chakras (b - 1) 0 0 0 0, Blood)
    unsnoc (Chakras b g 0 0 0) = Just (Chakras b (g - 1) 0 0 0, Gen)
    unsnoc (Chakras b g n 0 0) = Just (Chakras b g (n - 1) 0 0, Nin)
    unsnoc (Chakras b g n t 0) = Just (Chakras b g n (t - 1) 0, Tai)
    unsnoc (Chakras b g n t r) = Just (Chakras b g n t (r - 1), Rand)
    {-# INLINABLE unsnoc #-}

instance Parse Chakras where
    parser = Chakras
        <$> Parse.parser @Int
        <*> (Parse.char ',' >> Parse.parser @Int)
        <*> (Parse.char ',' >> Parse.parser @Int)
        <*> (Parse.char ',' >> Parse.parser @Int)
        <*> return 0

instance PathPiece Chakras where
    toPathPiece (Chakras b g n t _) = intercalate "," $ tshow <$> [b, g, n, t]
    fromPathPiece piece = rightToMaybe $ Parse.parseOnly $ encodeUtf8 piece

-- | Units of @Game.Model.Skill.cost@.
data Chakra
    = Blood -- ^ Bloodline
    | Gen   -- ^ Genjutsu
    | Nin   -- ^ Ninjutsu
    | Tai   -- ^ Taijutsu
    | Rand  -- ^ Random
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance AsEnumSet Chakra

instance Uniform Chakra where
    uniformM g = fromWord <$> R.uniformWord32 g -- excludes Rand
      where
        fromWord w = toEnum $ fromEnum $ w .&. 3
    {-# INLINE uniformM #-}

instance UniformRange Chakra where
    uniformRM = R.uniformEnumRM
    {-# INLINE uniformRM #-}

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

classes :: Chakras -> EnumSet Class
classes (Chakras b g n t r) = setFromList $ fst <$> filter snd
    [ (Bloodline, b /= 0)
    , (Genjutsu,  g /= 0)
    , (Ninjutsu,  n /= 0)
    , (Taijutsu,  t /= 0)
    , (Random,    r /= 0)
    ]

mapAmounts :: (Int -> Int) -> Chakras -> Chakras
mapAmounts f (Chakras b g n t r) = Chakras (f b) (f g) (f n) (f t) (f r)
{-# INLINE mapAmounts #-}

scale :: Int -> Chakras -> Chakras
scale scalar = mapAmounts (* scalar)
{-# INLINABLE scale #-}

spend :: Chakras -> Chakras -> Chakras
spend cost chakras = mapAmounts (max 0) $ chakras `naiveSubtract` cost

checkedSpend :: Chakras -> Chakras -> Maybe Chakras
checkedSpend cost before
    | insufficient = Nothing
    | otherwise    = Just after
  where
    after@(Chakras b g n t r) = before `naiveSubtract` cost
    insufficient = b < 0 || g < 0 || n < 0 || t < 0 || r < 0
