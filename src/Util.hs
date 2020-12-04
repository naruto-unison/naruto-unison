-- | Lightweight helper functions.
module Util
  ( (!?), (!!)
  , (<$>.), (<$><$>)
  , (—)
  , (∈), (∉)
  , Lift
  , duplic
  , epoch
  , getCurrentWeek
  , hushedParse
  , intersects
  , mapFromKeyed
  , commas
  , shorten, unaccent
  ) where

import ClassyPrelude

import Control.Monad.Trans.Class (MonadTrans)
import Data.Attoparsec.Text (Parser, notInClass, parseOnly)
import Data.Kind (Type)

-- If a function doesn't seem like it should be inlined, it probably doesn't go
-- here.

-- | 'unsafeIndex'.
infixl 9 !?
(!?) :: ∀ o. IsSequence o => o -> Index o -> Maybe (Element o)
(!?) = index
{-# INLINE (!?) #-}

-- | 'unsafeIndex'.
infixl 9 !!
(!!) :: ∀ o. IsSequence o => o -> Index o -> Element o
(!!) = unsafeIndex
{-# INLINE (!!) #-}

-- | @('.') . 'fmap'@
infixr 9 <$>.
(<$>.) :: ∀ f a b c. Functor f => (a -> c) -> (b -> f a) -> b -> f c
(<$>.) = (.) . (<$>)
{-# INLINE (<$>.) #-}

-- | '-' allowing for sections.
infixl 6 —
(—) :: ∀ a. Num a => a -> a -> a
(—) = (-)
{-# INLINE (—) #-}

-- | 'elem'.
infix 4 ∈
(∈) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∈) = elem
{-# INLINE (∈) #-}

-- | 'notElem'.
infix 4 ∉
(∉) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∉) = notElem
{-# INLINE (∉) #-}

-- | @'fmap' . 'fmap'@.
infixl 4 <$><$>
(<$><$>) :: ∀ f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$><$>) = fmap . fmap
{-# INLINE (<$><$>) #-}

-- | Divides a list of @Text@s into a single, comma-separated @Text@ ended
-- with a provided conjunction.
commas :: TextBuilder -> [TextBuilder] -> TextBuilder
commas conj = go
  where
    conj'      = " " ++ conj ++ " "
    go []      = mempty
    go [x]     = x
    go [x,y]   = x ++ conj' ++ y
    go [x,y,z] = x ++ ", " ++ y ++ "," ++ conj' ++ z
    go (x:xs)  = x ++ ", " ++ go xs
{-# INLINE commas #-}

-- | True if a list contains multiple identical values.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic = go []
  where
    go _ [] = False
    go seen (x:xs)
      | x ∈ seen  = True
      | otherwise = go (x:seen) xs
{-# INLINABLE duplic #-}

-- | @UTCTime 0 0@.
epoch :: UTCTime
epoch = UTCTime (ModifiedJulianDay 0) 0

-- | This number uses 'getCurrentTime' and increments by 1 every Sunday.
getCurrentWeek :: IO Integer
getCurrentWeek = getWeek <$> getCurrentTime
  where
    getWeek (UTCTime (ModifiedJulianDay day) _) = (day + 3) `quot` 7
    {-# INLINE getWeek #-}
{-# INLINE getCurrentWeek #-}

-- | True if any elements are shared by both collections.
intersects :: ∀ a. SetContainer a => a -> a -> Bool
xs `intersects` ys = not . null $ intersection xs ys
{-# INLINE intersects #-}

-- | Creates a map from a list using a projection function.
mapFromKeyed :: ∀ map a. IsMap map
             => (a -> ContainerKey map, a -> MapValue map) -> [a] -> map
mapFromKeyed (toKey, toVal) xs = mapFromList $ (\x -> (toKey x, toVal x)) <$> xs
{-# INLINE mapFromKeyed #-}

-- | Runs a parser and silently discards errors.
hushedParse :: Parser a -> Text -> Maybe a
hushedParse x = either (const Nothing) Just . parseOnly x
{-# INLINE hushedParse #-}

-- | Removes spaces and special characters.
shorten :: Text -> Text
shorten xs = omap unaccent $ filter (notInClass "- _:()®'/?") xs
{-# INLINE shorten #-}

-- | Turns special characters into ordinary characters.
unaccent :: Char -> Char
unaccent 'ō' = 'o'
unaccent 'Ō' = 'O'
unaccent 'ū' = 'u'
unaccent 'Ū' = 'U'
unaccent 'ä' = 'a'
unaccent x   = x
{-# INLINE unaccent #-}

-- | A metaconstraint for liftable functions.
-- Useful for default signatures of MTL classes:
--
-- > default myfunc :: Lift MyMonad m => m ()
-- > myfunc = lift myfunc
--
-- This is equivalent to
--
-- > type Lift mClass tran base m = (MonadTrans tran, mClass base, m ~ tran base)
--
-- Lift is a type family rather than a type alias in order to eliminate the
-- superfluous @tran@ and @base@ parameters.

type Lift mClass m = (MonadTrans (Tran m), mClass (Base m), m ~ Tran m (Base m))
type family Tran m :: (Type -> Type) -> Type -> Type where Tran (t n) = t
type family Base (m :: Type -> Type) :: Type -> Type where Base (t n) = n
