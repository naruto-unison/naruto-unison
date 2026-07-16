-- | Lightweight helper functions.
module Util
  ( (!?), (!!), (?), (!)
  , (∈), (∉)
  , (<.$.>), (<.$), ($.>), (<.&.>)
  , Lift
  , epoch
  , insertIf
  , intersects
  , setFromFoldable
  , lazyMapFromKeyed
  , tryFromJust, fromMaybeM, fromMaybeT
  , leftToMaybe, rightToMaybe
  ) where

import ClassyPrelude

import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Error.Class (MonadError(..))
import qualified Data.HashMap.Lazy as LHashMap

-- If a function doesn't seem like it should be inlined, it probably doesn't go
-- here.

-- | 'index'.
infixl 9 !?
(!?) :: ∀ o. IsSequence o => o -> Index o -> Maybe (Element o)
(!?) = index
{-# INLINE (!?) #-}

-- | 'unsafeIndex'.
infixl 9 !!
(!!) :: ∀ o. (HasCallStack, IsSequence o) => o -> Index o -> Element o
(!!) = unsafeIndex
{-# INLINE (!!) #-}

-- | 'index' for maps.
infixl 9 ?
(?) :: ∀ o. IsMap o => o -> ContainerKey o -> Maybe (MapValue o)
m ? k = k `lookup` m
{-# INLINE (?) #-}

-- | 'unsafeIndex' for maps.
infixl 9 !
(!) :: ∀ o. (HasCallStack, IsMap o) => o -> ContainerKey o -> MapValue o
m ! k = case m ? k of
    Just v  -> v
    Nothing -> error "(!): key not found"
{-# INLINE (!) #-}

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

-- | Monofunctor '<$>'
infixl 4 <.$.>
(<.$.>) :: ∀ o. MonoFunctor o => (Element o -> Element o) -> o -> o
(<.$.>) = omap
{-# INLINE (<.$.>) #-}

-- | Monofunctor '<$'
infixl 4 <.$
(<.$) :: ∀ o. MonoFunctor o => Element o -> o -> o
(<.$) = omap . const
{-# INLINE (<.$) #-}

-- | Monofunctor '$>'
infixl 4 $.>
($.>) :: ∀ o. MonoFunctor o => o -> Element o -> o
($.>) = flip (<.$)
{-# INLINE ($.>) #-}

-- | Monofunctor '<&>'
infixl 1 <.&.>
(<.&.>) :: ∀ o. MonoFunctor o => o -> (Element o -> Element o) -> o
(<.&.>) = flip (<.$.>)
{-# INLINE (<.&.>) #-}

-- | @UTCTime 0 0@.
epoch :: UTCTime
epoch = UTCTime (ModifiedJulianDay 0) 0

-- | Conditional 'insertSet'.
insertIf :: ∀ o. IsSet o => Bool -> Element o -> o -> o
insertIf True  = insertSet
insertIf False = const id
{-# INLINE insertIf #-}

-- | True if any elements are shared by both collections.
intersects :: ∀ a. SetContainer a => a -> a -> Bool
xs `intersects` ys = not . null $ intersection xs ys
{-# INLINE intersects #-}

setFromFoldable :: ∀ o s. (MonoFoldable o, IsSet s, Element o ~ Element s)
                => o -> s
setFromFoldable = foldl' (flip insertSet) mempty
{-# INLINE setFromFoldable #-}

-- | Creates a map from a list using a projection function.
lazyMapFromKeyed :: ∀ a k v. Hashable k
             => (a -> k, a -> v) -> [a] -> HashMap k v
lazyMapFromKeyed (toKey, toVal) = foldl' f mempty
  where
    f acc el = LHashMap.insert (toKey el) (toVal el) acc
{-# INLINABLE lazyMapFromKeyed #-}

tryFromJust :: ∀ e a m. MonadError e m => e -> Maybe a -> m a
tryFromJust e = maybe (throwError e) return
{-# INLINABLE tryFromJust #-}

fromMaybeM :: ∀ a m. Monad m => m a -> m (Maybe a) -> m a
fromMaybeM e m = maybe e return =<< m
{-# INLINABLE fromMaybeM #-}

fromMaybeT :: ∀ a m. Monad m => m a -> MaybeT m a -> m a
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
-- Lift uses type families in order to eliminate the superfluous @tran@ and
-- @base@ parameters.
type Lift mClass m = (MonadTrans (Tran m), mClass (Base m), m ~ Tran m (Base m))
-- | > Tran (MaybeT IO) ~ MaybeT
type family Tran m :: (Type -> Type) -> Type -> Type where Tran (t n) = t
-- | > Base (MaybeT IO) ~ IO
type family Base (m :: Type -> Type) :: Type -> Type where Base (t n) = n
