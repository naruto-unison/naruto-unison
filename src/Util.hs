-- | Lightweight helper functions.
module Util
  ( (!?), (!!)
  , (—), (∈), (∉)
  , Lift
  , adjustWith
  , duplic
  , intersects
  , liftST
  , mapFromKeyed
  , shorten, unaccent
  , whileM
  ) where

import ClassyPrelude

import           Control.Monad.ST (ST, stToIO)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Sequence ((|>))
import qualified Data.Sequence as Seq

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

-- | 'Seq.adjust''. If the index is out of bounds, the @Seq@ is padded with
-- a provided default value up to the index.
adjustWith :: ∀ a. a -> (a -> a) -> Int -> Seq a -> Seq a
adjustWith x f i xs
  | i < length xs = Seq.adjust' f i xs
  | otherwise     = xs ++ (replicate (i - length xs) x |> f x)
{-# INLINABLE adjustWith #-}

-- | True if a list contains multiple identical values.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic = go []
  where
    go _ [] = False
    go seen (x:xs)
      | x ∈ seen  = True
      | otherwise = go (x:seen) xs
{-# INLINABLE duplic #-}

-- | True if any elements are shared by both collections.
intersects :: ∀ a. SetContainer a => a -> a -> Bool
xs `intersects` ys = not . null $ intersection xs ys
{-# INLINE intersects #-}

-- | @'liftIO' . 'stToIO'
liftST :: ∀ m a. MonadIO m => ST RealWorld a -> m a
liftST = liftIO . stToIO
{-# INLINE liftST #-}

mapFromKeyed :: ∀ map a. IsMap map
             => (a -> ContainerKey map, a -> MapValue map) -> [a] -> map
mapFromKeyed (toKey, toVal) xs = mapFromList $ (\x -> (toKey x, toVal x)) <$> xs
{-# INLINE mapFromKeyed #-}

-- | Removes spaces and special characters.
shorten :: Text -> Text
shorten = omap unaccent . filter permit
  where
    permit ' '  = False
    permit '-'  = False
    permit ':'  = False
    permit '('  = False
    permit ')'  = False
    permit '®'  = False
    permit '\'' = False
    permit '/'  = False
    permit '?'  = False
    permit _    = True
    {-# INLINE permit #-}
{-# INLINE shorten #-}

unaccent :: Char -> Char
unaccent 'ō' = 'o'
unaccent 'Ō' = 'O'
unaccent 'ū' = 'u'
unaccent 'Ū' = 'U'
unaccent 'ä' = 'a'
unaccent x   = x
{-# INLINE unaccent #-}

-- | Repeats a monadic action until it returns @False@.
whileM :: Monad m => m Bool -> m ()
whileM act = go
  where
    go = whenM act go
{-# INLINABLE whileM #-}

-- | A metaconstraint for liftable functions.
-- Useful for default signatures of MTL classes:
--
-- > default myfunc :: Lift MyMonad m => m ()
-- > myfunc = lift myfunc
--
-- This is equivalent to
--
-- > type Lift mClass tran base m = (MonadTrans tran, mClass base, m ~ tran base)

type Lift mClass m = (MonadTrans (Tran m), mClass (Base m), m ~ Tran m (Base m))
type family Tran m :: (* -> *) -> * -> * where Tran (t n) = t
type family Base (m :: * -> *) :: * -> * where Base (t n) = n
