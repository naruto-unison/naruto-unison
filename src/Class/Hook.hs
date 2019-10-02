module Class.Hook (MonadHook(..)) where

import ClassyPrelude

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.Maybe (MaybeT)
import Yesod.WebSockets (WebSocketsT)

import Util (Lift)
import Game.Model.Context (Context)
import Game.Model.Ninja (Ninja)
import Game.Model.Skill (Skill)

class Monad m => MonadHook m where
    action :: Skill -> [Ninja] -> m ()
    turn   :: m ()

    default action :: Lift MonadHook m => Skill -> [Ninja] -> m ()
    action sk = lift . action sk
    {-# INLINE action #-}
    default turn :: Lift MonadHook m => m ()
    turn = lift turn
    {-# INLINE turn #-}

instance MonadHook m => MonadHook (ExceptT e m)
instance MonadHook m => MonadHook (IdentityT m)
instance MonadHook m => MonadHook (MaybeT m)
instance MonadHook m => MonadHook (SelectT r m)
instance MonadHook m => MonadHook (ReaderT Context m)
instance MonadHook m => MonadHook (WebSocketsT m)
instance (MonadHook m, Monoid w) => MonadHook (WriterT w m)
instance (MonadHook m, Monoid w) => MonadHook (AccumT w m)
