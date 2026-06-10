module Class.Hook (MonadHook(..)) where

import ClassyPrelude

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Trans.Writer (WriterT)
import Yesod.WebSockets (WebSocketsT)

import Game.Model.Chakras (Chakras)
import Game.Model.Game (Game)
import Game.Model.Internal (Context, Ninja, Skill, Trap)
import Game.Model.Player (Player)
import Game.Model.Trigger (Trigger)
import Util (Lift)

-- | Event hooks for mission progress.

class Monad m => MonadHook m where
    action    :: Skill -> Vector Ninja -> Vector Ninja -> m ()
    chakra    :: Skill -> (Chakras, Chakras) -> (Chakras, Chakras) -> m ()
    trap      :: Trap -> Ninja -> m ()
    trigger   :: Trigger -> Ninja -> m ()
    turnEnd   :: Player -> Vector Ninja -> Vector Ninja -> m ()
    turnStart :: Game -> Vector Ninja -> m ()

    default action :: Lift MonadHook m
                   => Skill -> Vector Ninja -> Vector Ninja -> m ()
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
    default trigger :: Lift MonadHook m
                    => Trigger -> Ninja -> m ()
    trigger x = lift . trigger x
    {-# INLINE trigger #-}
    default turnEnd :: Lift MonadHook m
                    => Player -> Vector Ninja -> Vector Ninja -> m ()
    turnEnd p ns = lift . turnEnd p ns
    {-# INLINE turnEnd #-}
    default turnStart :: Lift MonadHook m
                      => Game -> Vector Ninja -> m ()
    turnStart g = lift . turnStart g
    {-# INLINE turnStart #-}

instance MonadHook m => MonadHook (ExceptT e m)
instance MonadHook m => MonadHook (IdentityT m)
instance MonadHook m => MonadHook (MaybeT m)
instance MonadHook m => MonadHook (SelectT r m)
instance MonadHook m => MonadHook (ReaderT Context m)
instance MonadHook m => MonadHook (WebSocketsT m)
instance (MonadHook m, Monoid w) => MonadHook (WriterT w m)
instance (MonadHook m, Monoid w) => MonadHook (AccumT w m)
