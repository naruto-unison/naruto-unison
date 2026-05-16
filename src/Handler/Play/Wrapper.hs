-- | A container that implements user-defined monads.
-- It comes in pure, ST, and IO flavors.
-- Functions with such monadic constraints will almost always be called on MTL
-- stacks built on the foundation of one of these three types.
module Handler.Play.Wrapper
  ( STWrapper(..), fromInfo, replace, thaw
  , IOWrapper
  , Wrapper(..), freeze, unsafeFreeze, toTurn
  ) where

import ClassyPrelude

import           Control.Monad.ST (ST)
import           Control.Monad.Trans.State.Strict (StateT, gets, modify')
import           Data.Vector ((//))
import qualified Data.Vector as Vector
import           Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVector

import           Class.Hook (MonadHook(..))
import           Class.Play (MonadGame)
import qualified Class.Play as P
import qualified Class.Random
import           Class.Random (MonadRandom)
import           Class.Sockets (MonadSockets)
import           Class.ST (MonadST(..))
import           Game.Model.Game (Game)
import           Game.Model.Ninja (Ninja)
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import qualified Game.Model.Trap as Trap
import           Handler.Play.GameInfo (GameInfo)
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.Tracker (Tracker)
import qualified Handler.Play.Tracker as Tracker
import           Handler.Play.Turn (Turn)
import qualified Handler.Play.Turn as Turn
import           Mission.Progress (Progress)
import           Util ((!!))

-- | This type is the core of the entire program. It is the environment of game
-- processes and implements all of the user-defined monads.
data STWrapper s = STWrapper { tracker   :: Tracker s
                             , gameRef   :: STRef s Game
                             , ninjasRef :: STVector s Ninja
                             }

type IOWrapper = STWrapper RealWorld

askST :: ∀ m r a b. MonadST m
      => (r -> a) -> (a -> ST (PrimState m) b) -> ReaderT r m b
askST asker f = asks asker >>= liftST . f
{-# INLINE askST #-}

instance (MonadST m, s ~ PrimState m) => MonadGame (ReaderT (STWrapper s) m) where
    game       = askST gameRef readRef
    alter f    = askST gameRef $ flip modifyRef' f
    ninjas     = askST ninjasRef Vector.freeze <&> toList
    ninja i    = askST ninjasRef $ flip MVector.unsafeRead (Slot.toInt i)
    write i x  = askST ninjasRef \xs -> MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f = askST ninjasRef \xs -> MVector.unsafeModify xs f $ Slot.toInt i
    {-# INLINE game #-}
    {-# INLINE alter #-}
    {-# INLINE ninjas #-}
    {-# INLINE ninja #-}
    {-# INLINE write #-}
    {-# INLINE modify #-}

instance (MonadST m, s ~ PrimState m) => MonadHook (ReaderT (STWrapper s) m) where
    action sk ns ns' = askST tracker $
                       Tracker.trackAction (Skill.name sk) ns ns'
    chakra sk ch ch' = askST tracker $
                       Tracker.trackChakra (Skill.name sk) ch ch'
    trap tr targ     = askST tracker $
                       Tracker.trackTrap (Trap.name tr) (Trap.user tr) targ
    trigger tr targ  = askST tracker $
                       Tracker.trackTrigger tr targ
    turn p ns ns'    = askST tracker $
                       Tracker.trackTurn p ns ns'

fromInfo :: ∀ s. GameInfo -> ST s (STWrapper s)
fromInfo info =
    STWrapper <$> Tracker.fromInfo info
              <*> newRef (GameInfo.game info)
              <*> Vector.thaw (fromList $ GameInfo.ninjas info)

-- | Replaces 'gameRef' and 'ninjasRef' of the former with the latter.
-- Does not affect 'tracker'.
replace :: ∀ s. Wrapper -> STWrapper s -> ST s ()
replace Wrapper{game, ninjas} STWrapper{gameRef, ninjasRef} = do
    writeRef gameRef game
    MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

-- Wrappers are pure and immutable, so these two functions are inefficient and a
-- bit silly. Test suites can make good use of them, but Wrappers elsewhere
-- are treated merely as frozen snapshots of game state.

adjustVec :: ∀ a. (a -> a) -> Int -> Vector a -> Vector a
adjustVec f i = Vector.modify \xs -> MVector.modify xs f i

updateVec :: ∀ a. Int -> a -> Vector a -> Vector a
updateVec i x xs = xs // [(i, x)]

data Wrapper = Wrapper { progress :: [Progress]
                       , game     :: Game
                       , ninjas   :: Vector Ninja
                       }

instance MonadGame (StateT Wrapper Identity) where
    game        = gets game
    alter f     = modify' \x -> x { game = f $ game x }
    ninjas      = toList <$> gets ninjas
    ninja i     = (!! Slot.toInt i) <$> gets ninjas
    write i x   = modify' \g ->
        g { ninjas = updateVec (Slot.toInt i) x $ ninjas g }
    modify i f  = modify' \g ->
        g { ninjas = adjustVec f (Slot.toInt i) $ ninjas g }
    modifyAll f = modify' \g -> g { ninjas = f <$> ninjas g }
instance MonadRandom (StateT Wrapper Identity) where
    random x = return . const x
    shuffle  = return
    player   = return Player.A
instance MonadHook (StateT Wrapper Identity) where
    action  = const . const . const $ return ()
    chakra  = const . const . const $ return ()
    trap    = const . const $ return ()
    trigger = const . const $ return ()
    turn    = const . const . const $ return ()

freeze :: ∀ m. MonadGame m => m Wrapper
freeze = Wrapper mempty <$> P.game <*> (fromList <$> P.ninjas)

-- | The STWrapper may not be used after this operation.
unsafeFreeze :: ∀ s. STWrapper s -> ST s Wrapper
unsafeFreeze STWrapper{tracker, gameRef, ninjasRef} =
    Wrapper <$> Tracker.unsafeFreeze tracker
            <*> readRef gameRef
            <*> Vector.unsafeFreeze ninjasRef

thaw :: ∀ s. Wrapper -> ST s (STWrapper s)
thaw Wrapper{game, ninjas} =
    STWrapper Tracker.empty <$> newRef game
                            <*> Vector.thaw ninjas

--  | Encodes game state into a form suitable for sending to the client.
toTurn :: Player -> Wrapper -> Turn
toTurn player Wrapper{ninjas, game} = Turn.new player (toList ninjas) game

instance MonadRandom m => MonadRandom (ReaderT Wrapper m)
instance MonadRandom m => MonadRandom (ReaderT (STWrapper s) m)
instance MonadSockets m => MonadSockets (ReaderT Wrapper m)
instance MonadSockets m => MonadSockets (ReaderT (STWrapper s) m)
