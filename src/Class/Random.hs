-- | Monadic constraints for generating random data.
module Class.Random
  ( MonadRandom(..)
  , choose
  ) where

import ClassyPrelude

import           Control.Monad.ST (ST)
import           Control.Monad.Trans.Accum (AccumT)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.Select (SelectT)
import           Control.Monad.Trans.Writer (WriterT)
import           Data.Bits ((.&.))
import qualified Data.Vector.Generic as Generic
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as Random
import           Yesod.WebSockets (WebSocketsT)

import Util (Lift)
import Game.Model.Player (Player)

-- | A monad capable of nondeterministic behavior.
--
-- Instances should satisfy the following laws:
-- * @random x y ∈ [x .. y]@
-- * @sort (shuffle xs) == sort xs@
class Monad m => MonadRandom m where
    -- | Selects an integer in an inclusive range.
    random  :: Int -> Int -> m Int
    -- | Randomly shuffles elements in a list.
    shuffle :: ∀ v a. Generic.Vector v a => v a -> m (v a)
    -- | Randomly chooses 'Player.A' or 'Player.B'. A coin toss.
    player :: m Player

    default random :: Lift MonadRandom m
                   => Int -> Int -> m Int
    random a = lift . random a
    {-# INLINE random #-}
    default shuffle :: Lift MonadRandom m
                    => Generic.Vector v a => v a -> m (v a)
    shuffle = lift . shuffle
    {-# INLINE shuffle #-}
    default player :: Lift MonadRandom m
                   => m Player
    player = lift player
    {-# INLINE player #-}

instance MonadRandom (ReaderT (Random.Gen s) (ST s)) where
    random a b = ask >>= lift . Random.uniformR (a, b)
    {-# INLINE random #-}
    shuffle xs = ask >>= lift . Random.uniformShuffle xs
    {-# INLINE shuffle #-}
    player = toEnum . (.&. 1) <$> (ask >>= lift . Random.uniform)
    {-# INLINE player #-}

instance MonadIO m => MonadRandom (ReaderT (Random.Gen RealWorld) m) where
    random a b = ask >>= liftIO . Random.uniformR (a, b)
    {-# INLINE random #-}
    shuffle xs = ask >>= liftIO . Random.uniformShuffle xs
    {-# INLINE shuffle #-}
    player = toEnum . (.&. 1) <$> (ask >>= liftIO . Random.uniform)
    {-# INLINE player #-}

instance MonadRandom m => MonadRandom (ExceptT e m)
instance MonadRandom m => MonadRandom (IdentityT m)
instance MonadRandom m => MonadRandom (MaybeT m)
instance MonadRandom m => MonadRandom (SelectT r m)
instance MonadRandom m => MonadRandom (WebSocketsT m)
instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m)
instance (MonadRandom m, Monoid w) => MonadRandom (AccumT w m)

-- | Randomly selects an element from a finite list.
-- Returns @Nothing@ on an empty list.
choose :: ∀ m a. MonadRandom m => [a] -> m (Maybe a)
choose [] = return Nothing
choose xs = index xs <$> random 0 (length xs - 1)
