module Class.Hook (MonadHook(..)) where

import ClassyPrelude

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.Writer (WriterT)
import Yesod.WebSockets (WebSocketsT)

import Game.Model.Chakra (Chakras)
import Game.Model.Context (Context)
import Game.Model.Ninja (Ninja)
import Game.Model.Player (Player)
import Game.Model.Skill (Skill)
import Game.Model.Trap (Trap)
import Util (Lift)

class Monad m => MonadHook m where
    action :: Skill -> [Ninja] -> [Ninja] -> m ()
    chakra :: Skill -> (Chakras, Chakras) -> (Chakras, Chakras) -> m ()
    trap   :: Trap -> Ninja -> m ()
    turn   :: Player -> [Ninja] -> [Ninja] -> m ()

    default action :: Lift MonadHook m
                   => Skill -> [Ninja] -> [Ninja] -> m ()
    action sk ns = lift . action sk ns
    {-# INLINE action #-}
    default chakra :: Lift MonadHook m
                   => Skill -> (Chakras, Chakras) -> (Chakras, Chakras) -> m ()
    chakra sk chaks = lift . chakra sk chaks
    {-# INLINE chakra #-}
    default trap :: Lift MonadHook m
                 => Trap -> Ninja -> m ()
    trap x = lift . trap x
    {-# INLINE trap #-}
    default turn :: Lift MonadHook m
                 => Player -> [Ninja] -> [Ninja] -> m ()
    turn p ns = lift . turn p ns
    {-# INLINE turn #-}

instance MonadHook m => MonadHook (ExceptT e m)
instance MonadHook m => MonadHook (IdentityT m)
instance MonadHook m => MonadHook (MaybeT m)
instance MonadHook m => MonadHook (SelectT r m)
instance MonadHook m => MonadHook (ReaderT Context m)
instance MonadHook m => MonadHook (WebSocketsT m)
instance (MonadHook m, Monoid w) => MonadHook (WriterT w m)
instance (MonadHook m, Monoid w) => MonadHook (AccumT w m)
