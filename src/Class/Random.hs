-- | Monadic constraints for generating random data.
module Class.Random
  ( MonadRandom(..)
  , choose
  ) where

import ClassyPrelude hiding (Vector)

import Control.Monad.ST (ST)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Vector.Generic (Vector)
import System.Random.MWC (Gen)
import System.Random.MWC.Distributions (uniformShuffle)
import System.Random.Stateful (Uniform(..), UniformRange(..))
import Yesod.WebSockets (WebSocketsT)

import Util ((!?), Lift)

-- | A monad capable of nondeterministic behavior.
class Monad m => MonadRandom m where
    -- | Selects a value in a uniform range.
    random :: ∀ a. Uniform a => m a
    -- | Selects a value in an inclusive range.
    range  :: ∀ a. UniformRange a => (a, a) -> m a
    -- | Randomly shuffles elements in a list.
    shuffle :: ∀ v a. Vector v a => v a -> m (v a)

    default random :: (Lift MonadRandom m, Uniform a)
                   => m a
    random = lift random
    {-# INLINE random #-}
    default range :: (Lift MonadRandom m, UniformRange a)
                  => (a, a) -> m a
    range = lift . range
    {-# INLINE range #-}
    default shuffle :: (Lift MonadRandom m, Vector v a)
                    => v a -> m (v a)
    shuffle = lift . shuffle
    {-# INLINE shuffle #-}

instance MonadRandom (ReaderT (Gen s) (ST s)) where
    random = ask >>= lift . uniformM
    {-# INLINABLE random #-}
    range (a, b) = ask >>= lift . uniformRM (a, b)
    {-# INLINABLE range #-}
    shuffle xs = ask >>= lift . uniformShuffle xs
    {-# INLINABLE shuffle #-}

instance MonadIO m => MonadRandom (ReaderT (Gen RealWorld) m) where
    random = ask >>= liftIO . uniformM
    {-# INLINABLE random #-}
    range (a, b) = ask >>= liftIO . uniformRM (a, b)
    {-# INLINABLE range #-}
    shuffle xs = ask >>= liftIO . uniformShuffle xs
    {-# INLINABLE shuffle #-}

instance MonadRandom m => MonadRandom (ExceptT e m)
instance MonadRandom m => MonadRandom (IdentityT m)
instance MonadRandom m => MonadRandom (MaybeT m)
instance MonadRandom m => MonadRandom (SelectT r m)
instance MonadRandom m => MonadRandom (WebSocketsT m)
instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m)
instance (MonadRandom m, Monoid w) => MonadRandom (AccumT w m)

-- | Randomly selects an element from a finite list.
-- Returns @Nothing@ on an empty list.
choose :: ∀ o m. (MonadRandom m, IsSequence o, UniformRange (Index o))
       => o -> m (Maybe (Element o))
choose xs
  | null xs   = return Nothing
  | otherwise = (xs !?) <$> range (0, lengthIndex xs - 1)
