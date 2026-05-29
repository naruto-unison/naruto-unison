-- | Lightweight helper functions.
module Util
  ( (!?), (!!)
  , (∈), (∉)
  , Lift
  , epoch
  , intersects
  , mapFromKeyed
  , tryFromJust, fromMaybeM, fromMaybeT
  , leftToMaybe, rightToMaybe
  ) where

import ClassyPrelude

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Error.Class (MonadError(..))

-- If a function doesn't seem like it should be inlined, it probably doesn't go
-- here.

-- | 'index'.
infixl 9 !?
(!?) :: ∀ o. IsSequence o => o -> Index o -> Maybe (Element o)
(!?) = index
{-# INLINE (!?) #-}

-- | 'unsafeIndex'.
infixl 9 !!
(!!) :: ∀ o. IsSequence o => o -> Index o -> Element o
(!!) = unsafeIndex
{-# INLINE (!!) #-}

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

-- | @UTCTime 0 0@.
epoch :: UTCTime
epoch = UTCTime (ModifiedJulianDay 0) 0

-- | True if any elements are shared by both collections.
intersects :: ∀ a. SetContainer a => a -> a -> Bool
xs `intersects` ys = not . null $ intersection xs ys
{-# INLINE intersects #-}

-- | Creates a map from a list using a projection function.
mapFromKeyed :: ∀ map a. IsMap map
             => (a -> ContainerKey map, a -> MapValue map) -> [a] -> map
mapFromKeyed (toKey, toVal) xs = mapFromList $ (\x -> (toKey x, toVal x)) <$> xs
{-# INLINABLE mapFromKeyed #-}

tryFromJust :: ∀ m a e. MonadError e m => e -> Maybe a -> m a
tryFromJust e = maybe (throwError e) return
{-# INLINABLE tryFromJust #-}

fromMaybeM :: ∀ m a. Monad m => m a -> m (Maybe a) -> m a
fromMaybeM e m = maybe e return =<< m
{-# INLINABLE fromMaybeM #-}

fromMaybeT :: ∀ m a. Monad m => m a -> MaybeT m a -> m a
fromMaybeT e (MaybeT m) = fromMaybeM e m
{-# INLINABLE fromMaybeT #-}

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left x)  = Just x
leftToMaybe (Right _) = Nothing
{-# INLINABLE leftToMaybe #-}

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x
{-# INLINABLE rightToMaybe #-}

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
