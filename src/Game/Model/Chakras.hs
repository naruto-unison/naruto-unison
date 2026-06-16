module Game.Model.Chakras
  ( Chakra(..)
  , Chakras
  , each
  , set
  , mode
  , scale
  , toDescendingList
  , spend
  , checkedSpend
  , random
  ) where

import ClassyPrelude hiding (groupBy, groupAllOn, minimum)

import           Data.Aeson (ToJSON)
import           Data.Bits
import           Data.Enum.Set (AsEnumSet(..))
import           Data.Sequences (groupBy, groupAllOn)
import           GHC.Exts (IsList)
import qualified GHC.Exts
import           System.Random.Stateful (Uniform(..), UniformRange(..))
import qualified System.Random.Stateful as R
import           Text.Blaze ((!), ToMarkup(..))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as HTML
import           Yesod.Core.Dispatch (PathPiece(..))

import           Class.Classed (Classed(..))
import           Class.Display (Display(..), buildStrict)
import           Class.Parse (Parse)
import qualified Class.Parse as Parse
import           Class.Random (MonadRandom)
import qualified Class.Random as Random
import           Game.Model.Class (Class(..))
import           Util ((∈), rightToMaybe)

-- | Collection of all chakra types.
data Chakras = Chakras
    { blood :: {-# UNPACK #-} Int -- ^ Bloodline
    , gen   :: {-# UNPACK #-} Int -- ^ Genjutsu
    , nin   :: {-# UNPACK #-} Int -- ^ Ninjutsu
    , tai   :: {-# UNPACK #-} Int -- ^ Taijutsu
    , rand  :: {-# UNPACK #-} Int -- ^ Random
    } deriving (Eq, Show, Read, Generic)

each :: Int -> Chakras
each i = Chakras i i i i i

get :: Chakra -> Chakras -> Int
get Blood = blood
get Gen   = gen
get Nin   = nin
get Tai   = tai
get Rand  = rand
{-# INLINE get #-}

modify :: (Int -> Int) -> Chakra -> Chakras -> Chakras
modify f Blood xs = xs { blood = f $ blood xs }
modify f Gen   xs = xs { gen   = f $ gen xs }
modify f Nin   xs = xs { nin   = f $ nin xs }
modify f Tai   xs = xs { tai   = f $ tai xs }
modify f Rand  xs = xs { rand  = f $ rand xs }
{-# INLINE modify #-}

set :: Chakra -> Int -> Chakras -> Chakras
set x i = modify (const $ max 0 i) x
{-# INLINE set #-}

map1 :: (Int -> Int) -> Chakras -> Chakras
map1 f (Chakras b g n t r) = Chakras (f b) (f g) (f n) (f t) (f r)
{-# INLINE map1 #-}

map2 :: (Int -> Int -> Int) -> Chakras -> Chakras -> Chakras
map2 f (Chakras b g n t r) (Chakras b' g' n' t' r') = Chakras
    (b `f` b')
    (g `f` g')
    (n `f` n')
    (t `f` t')
    (r `f` r')
{-# INLINE map2 #-}

naiveSubtract :: Chakras -> Chakras -> Chakras
naiveSubtract = map2 (-)
{-# INLINE naiveSubtract #-}

toNormalizedList :: Chakras -> [Chakra]
toNormalizedList chakras = filter (∈ chakras) [minBound..maxBound]
{-# INLINE toNormalizedList #-}

instance IsList Chakras where
    type Item Chakras = Chakra
    toList = otoList
    {-# INLINE toList #-}

    fromList = fromList
    {-# INLINE fromList #-}

instance ToJSON Chakras

instance ToMarkup Chakras where
    toMarkup = concatMap toMarkup
    {-# INLINABLE toMarkup #-}

instance Semigroup Chakras where
    (<>) = map2 (+)
    {-# INLINE (<>) #-}

instance Monoid Chakras where
    mempty = Chakras 0 0 0 0 0
    {-# INLINE mempty #-}

type instance Element Chakras = Chakra

instance MonoFunctor Chakras where
    f `omap` (Chakras b g n t r) = modify (+ b) (f Blood)
                                 . modify (+ g) (f Gen)
                                 . modify (+ n) (f Nin)
                                 . modify (+ t) (f Tai)
                                 . modify (+ r) (f Rand)
                                 $ mempty
    {-# INLINABLE omap #-}

instance MonoFoldable Chakras where
    ofoldMap f = ofoldr (mappend . f) mempty
    {-# INLINE ofoldMap #-}

    ofoldr f acc = go
      where
        go (x:<xs) = x `f` go xs
        go _       = acc
    {-# INLINE ofoldr #-}

    ofoldl' f acc = go acc
      where
        go !acc' (x:<xs) = go (acc' `f` x) xs
        go acc'  _       = acc'
    {-# INLINE ofoldl' #-}

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

    ofoldr1Ex f = go
      where
        go (x:<Chakras 0 0 0 0 0) = x
        go (x:<xs) = f x (go xs)
        go _       = error "Data.MonoTraversable.ofoldr1Ex: empty"
    {-# INLINE ofoldr1Ex #-}

    ofoldl1Ex' f (x:<xs) = ofoldl' f x xs
    ofoldl1Ex' _ _       = error "Data.MonoTraversable.foldl1': empty"
    {-# INLINE ofoldl1Ex' #-}

    headEx (Chakras b g n t r)
      | b /= 0    = Blood
      | g /= 0    = Gen
      | n /= 0    = Nin
      | t /= 0    = Tai
      | r /= 0    = Rand
      | otherwise = error "Data.MonoTraversable.headEx': empty"
    {-# INLINE headEx #-}

    lastEx (Chakras b g n t r)
      | r /= 0    = Rand
      | t /= 0    = Tai
      | n /= 0    = Nin
      | g /= 0    = Gen
      | b /= 0    = Blood
      | otherwise = error "Data.MonoTraversable.lastEx': empty"
    {-# INLINE lastEx #-}

    maximumByEx f = maximumByEx f . toNormalizedList
    {-# INLINABLE maximumByEx #-}

    minimumByEx f = minimumByEx f . toNormalizedList
    {-# INLINABLE minimumByEx #-}

    x `oelem` xs = get x xs /= 0
    {-# INLINE oelem #-}

    x `onotElem` xs = get x xs == 0
    {-# INLINE onotElem #-}

instance MonoTraversable Chakras where
    otraverse f = foldr cons_f (pure mempty)
      where
        cons_f x ys = liftA2 cons (f x) ys
    {-# INLINE otraverse #-}

instance MonoPointed Chakras where
    opoint Blood = Chakras 1 0 0 0 0
    opoint Gen   = Chakras 0 1 0 0 0
    opoint Nin   = Chakras 0 0 1 0 0
    opoint Tai   = Chakras 0 0 0 1 0
    opoint Rand  = Chakras 0 0 0 0 1
    {-# INLINE opoint #-}

instance GrowingAppend Chakras

instance SemiSequence Chakras where
    type Index Chakras = Int

    intersperse x xs
      | len < 2   = xs
      | otherwise = modify (+ (len - 1)) x xs
      where
        len = length xs
    {-# INLINABLE intersperse #-}

    reverse = id
    {-# INLINE reverse #-}

    find f (Chakras b g n t r)
      | b /= 0 && f Blood = Just Blood
      | g /= 0 && f Gen   = Just Gen
      | n /= 0 && f Nin   = Just Nin
      | t /= 0 && f Tai   = Just Tai
      | r /= 0 && f Rand  = Just Rand
      | otherwise         = Nothing
    {-# INLINABLE find #-}

    sortBy _ xs = xs
    {-# INLINE sortBy #-}

    cons = modify (+ 1)
    {-# INLINE cons #-}

    xs `snoc` x = x `cons` xs
    {-# INLINE snoc #-}

instance IsSequence Chakras where
    lengthIndex = length
    {-# INLINE lengthIndex #-}

    break f (Chakras b g n t r)
      | b /= 0 && f Blood = (Chakras 0 0 0 0 0, Chakras b g n t r)
      | g /= 0 && f Gen   = (Chakras b 0 0 0 0, Chakras 0 g n t r)
      | n /= 0 && f Nin   = (Chakras b g 0 0 0, Chakras 0 0 n t r)
      | t /= 0 && f Tai   = (Chakras b g n 0 0, Chakras 0 0 0 t r)
      | r /= 0 && f Rand  = (Chakras b g n t 0, Chakras 0 0 0 0 r)
      | otherwise         = (Chakras b g n t r, Chakras 0 0 0 0 0)
    {-# INLINABLE break #-}

    span f = break (not . f)
    {-# INLINABLE span #-}

    dropWhile f = snd . span f
    {-# INLINABLE dropWhile #-}

    takeWhile f = fst . span f
    {-# INLINABLE takeWhile #-}

    splitAt i xs@(Chakras b g n t r) = (taken, xs `naiveSubtract` taken)
      where
        taken = Chakras (max 0 $ min b i)
                        (max 0 . min g $ i - b)
                        (max 0 . min n $ i - b - g)
                        (max 0 . min t $ i - b - g - n)
                        (max 0 . min r $ i - b - g - n - t)
    {-# INLINABLE splitAt #-}

    partition f xs = (yays, xs `naiveSubtract` yays)
      where
        yays = filter f xs
    {-# INLINABLE partition #-}

    uncons (Chakras b g n t r)
      | b /= 0    = Just (Blood, Chakras (b - 1) g n t r)
      | g /= 0    = Just (Gen,   Chakras b (g - 1) n t r)
      | n /= 0    = Just (Nin,   Chakras b g (n - 1) t r)
      | t /= 0    = Just (Tai,   Chakras b g n (t - 1) r)
      | r /= 0    = Just (Rand,  Chakras b g n t (r - 1))
      | otherwise = Nothing
    {-# INLINABLE uncons #-}

    unsnoc (Chakras b g n t r)
      | r /= 0    = Just (Chakras b g n t (r - 1), Rand)
      | t /= 0    = Just (Chakras b g n (t - 1) r, Tai)
      | n /= 0    = Just (Chakras b g (n - 1) t r, Nin)
      | g /= 0    = Just (Chakras b (g - 1) n t r, Gen)
      | b /= 0    = Just (Chakras (b - 1) g n t r, Blood)
      | otherwise = Nothing
    {-# INLINABLE unsnoc #-}

    filter f (Chakras b g n t r) = Chakras (filt b Blood)
                                           (filt g Gen)
                                           (filt n Nin)
                                           (filt t Tai)
                                           (filt r Rand)
      where
        filt amount chakra
          | amount == 0 || f chakra = amount
          | otherwise               = 0
    {-# INLINABLE filter #-}

    filterM f (Chakras b g n t r) = Chakras <$> filt b Blood
                                            <*> filt g Gen
                                            <*> filt n Nin
                                            <*> filt t Tai
                                            <*> filt r Rand
      where
        filt 0      _      = return 0
        filt amount chakra = getFiltered amount <$> f chakra
        getFiltered amount True  = amount
        getFiltered _      False = 0
    {-# INLINABLE filterM #-}

    replicate n _
      | n <= 0        = Chakras 0 0 0 0 0
    replicate b Blood = Chakras b 0 0 0 0
    replicate g Gen   = Chakras 0 g 0 0 0
    replicate n Nin   = Chakras 0 0 n 0 0
    replicate t Tai   = Chakras 0 0 0 t 0
    replicate r Rand  = Chakras 0 0 0 0 r
    {-# INLINABLE replicate #-}

    replicateM n f = replicate n <$> f
    {-# INLINABLE replicateM #-}

    groupBy _  (Chakras 0 0 0 0 0) = []
    groupBy eq xs                  = ys : groupBy eq zs
      where
        x        = unsafeHead xs
        (ys, zs) = span (eq x) xs

    groupAllOn _ (Chakras 0 0 0 0 0) = []
    groupAllOn f xs                  = ys : groupAllOn f zs
      where
        x        = unsafeHead xs
        (ys, zs) = partition ((== f x) . f) xs

    index (Chakras b g n t r) i
        | i < 0                 = Nothing
        | i                 < b = Just Blood
        | i - b             < g = Just Gen
        | i - b - g         < n = Just Nin
        | i - b - g - n     < t = Just Tai
        | i - b - g - n - t < r = Just Rand
        | otherwise             = Nothing
    {-# INLINABLE index #-}

    splitWhen f (Chakras b g n t r)
      | b /= 0 && f Blood = Chakras 0 0 0 0 0
                          : splitWhen f (Chakras (b - 1) g n t r)
      | g /= 0 && f Gen   = Chakras b 0 0 0 0
                          : splitWhen f (Chakras 0 (g - 1) n t r)
      | n /= 0 && f Nin   = Chakras b g 0 0 0
                          : splitWhen f (Chakras 0 0 (n - 1) t r)
      | t /= 0 && f Tai   = Chakras b g n 0 0
                          : splitWhen f (Chakras 0 0 0 (t - 1) r)
      | r /= 0 && f Rand  = Chakras b g n t 0
                          : splitWhen f (Chakras 0 0 0 0 (r - 1))
      | otherwise         = [Chakras b g n t r]

instance Parse Chakras where
    parser = do
        b <- Parse.parser
        Parse.char ','
        g <- Parse.parser
        Parse.char ','
        n <- Parse.parser
        Parse.char ','
        t <- Parse.parser
        return $ Chakras b g n t 0

instance PathPiece Chakras where
    toPathPiece (Chakras b g n t _) = buildStrict . intercalate ","
                                    $ display <$> [b, g, n, t]
    fromPathPiece piece = rightToMaybe $ Parse.parseToEnd piece

-- | Units of @Game.Model.Skill.cost@.
data Chakra
    = Blood -- ^ Bloodline
    | Gen   -- ^ Genjutsu
    | Nin   -- ^ Ninjutsu
    | Tai   -- ^ Taijutsu
    | Rand  -- ^ Random
    deriving (Bounded, Enum, Eq, Ord, Show)

instance AsEnumSet Chakra

instance Uniform Chakra where
    uniformM g = fromWord <$> R.uniformWord32 g -- excludes Rand
      where
        fromWord w = toEnum . fromEnum $ w .&. 3
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

instance Display Chakra where
    display Blood = "bloodline"
    display Gen   = "genjutsu"
    display Nin   = "ninjutsu"
    display Tai   = "taijutsu"
    display Rand  = "random"

instance Classed Chakras where
    getClasses chakras = setFromList $ chakraClass <$> toNormalizedList chakras
      where
        chakraClass Blood = Bloodline
        chakraClass Gen   = Genjutsu
        chakraClass Nin   = Ninjutsu
        chakraClass Tai   = Taijutsu
        chakraClass Rand  = Random

scale :: Int -> Chakras -> Chakras
scale scalar
  | scalar <= 0 = const mempty
  | otherwise   = map1 (* scalar)
{-# INLINABLE scale #-}

spend :: Chakras -> Chakras -> Chakras
spend cost chakras = map1 (max 0) $ chakras `naiveSubtract` cost

checkedSpend :: Chakras -> Chakras -> Maybe Chakras
checkedSpend cost before
    | insufficient = Nothing
    | otherwise    = Just after
  where
    after@(Chakras b g n t r) = before `naiveSubtract` cost
    insufficient = b < 0 || g < 0 || n < 0 || t < 0 || r < 0

mode :: Chakras -> Maybe Chakra
mode (Chakras 0 0 0 0 0) = Nothing
mode chakras = Just $! maximumByEx (compare `on` chakraAmount) [minBound..Tai]
  where
    chakraAmount kind = get kind chakras
{--# INLINABLE mode #-}

toDescendingList :: Chakras -> [Chakra]
toDescendingList xs = case mode xs of
    Just x  -> x : toDescendingList (modify (- 1) x xs)
    Nothing -> []

fromBits :: ∀ a. (Bits a, Enum a, Num a) => Int -> a -> Chakras
fromBits 0 _ = mempty
fromBits i n = toEnum (fromEnum $ n .&. 3) :< fromBits (i - 1) (n .>>. 2)

random :: ∀ m. MonadRandom m => Int -> m Chakras
random len
  | len <=  0 = return mempty
  | len <= 16 = fromBits @Word32 len <$> Random.random
  | len <= 32 = fromBits @Word64 len <$> Random.random
  | otherwise = (++) <$> random 32 <*> random (len - 32)
