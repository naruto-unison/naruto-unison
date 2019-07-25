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
import           Control.Monad.Trans.Select (SelectT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Control.Monad.Trans.Writer (WriterT)
import           Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.Vector.Generic as Generic
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as Random
import           Yesod.WebSockets (WebSocketsT)

import Core.Util ((!!), Lift)

class Monad m => MonadRandom m where
    random  :: Int -> Int -> m Int
    shuffle :: ∀ v a. Generic.Vector v a => v a -> m (v a)

    default random  :: Lift MonadRandom m => Int -> Int -> m Int
    random a = lift . random a
    default shuffle :: Lift MonadRandom m => Generic.Vector v a => v a -> m (v a)
    shuffle  = lift . shuffle

instance MonadRandom (ReaderT (Random.Gen s) (ST s)) where
    random a b = ask >>= lift . Random.uniformR (a, b)
    shuffle xs = ask >>= lift . Random.uniformShuffle xs

instance MonadIO m => MonadRandom (ReaderT (Random.Gen RealWorld) m) where
    random a b = ask >>= liftIO . Random.uniformR (a, b)
    shuffle xs = ask >>= liftIO . Random.uniformShuffle xs

instance MonadRandom m => MonadRandom (ExceptT e m)
instance MonadRandom m => MonadRandom (IdentityT m)
instance MonadRandom m => MonadRandom (MaybeT m)
instance MonadRandom m => MonadRandom (SelectT r m)
instance MonadRandom m => MonadRandom (StateT r m)
instance MonadRandom m => MonadRandom (WebSocketsT m)
instance (MonadRandom m, Monoid w) => MonadRandom (WriterT w m)
instance (MonadRandom m, Monoid w) => MonadRandom (AccumT w m)

-- | Randomly selects an element from a list.
-- Returns 'Nothing' on an empty list.
choose :: ∀ m a. MonadRandom m => [a] -> m (Maybe a)
choose [] = return Nothing
choose xs = Just . (xs !!) <$> random 0 (length xs - 1)
