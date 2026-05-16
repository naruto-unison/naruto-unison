{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}
-- | A patch for Edward Kmett's [monad-st](https://hackage.haskell.org/package/monad-st) library to use [primitive](https://hackage.haskell.org/package/primitive)'s standard 'PrimState' instead of defining its own @World@ type.
module Class.ST (MonadST(..)) where

import Prelude
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Trans.Class

class PrimMonad m => MonadST m where
  liftST :: ST (PrimState m) a -> m a

instance MonadST IO where
  liftST = stToIO

instance MonadST (ST s) where
  liftST = id

instance (MonadTrans t, MonadST m, PrimMonad (t m), PrimState m ~ PrimState (t m)) => MonadST (t m) where
  liftST = lift . liftST

