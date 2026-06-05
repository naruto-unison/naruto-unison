-- | A container that implements user-defined monads.
-- It comes in pure, ST, and IO flavors.
-- Functions with such monadic constraints will almost always be called on MTL
-- stacks built on the foundation of one of these three types.
module Handler.Play.Wrapper
  ( STWrapper(..), fromInfo, replace, thaw
  , IOWrapper
  , Wrapper(..), freeze, unsafeFreeze, toTurn, runGame
  ) where

import ClassyPrelude

import qualified Data.Vector as Vector
import           Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVector

import           Class.Hook (MonadHook(..))
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import           Game.Model.Game (Game)
import           Game.Model.Ninja (Ninja)
import           Game.Model.Player (Player)
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill
import qualified Game.Model.Slot as Slot
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap
import           Handler.Play.GameInfo (GameInfo(GameInfo))
import qualified Handler.Play.GameInfo
import           Handler.Play.Tracker (Tracker)
import qualified Handler.Play.Tracker as Tracker
import           Handler.Play.Turn (Turn)
import qualified Handler.Play.Turn as Turn
import           Mission.Progress (Progress)

-- | This type is the core of the entire program. It is the environment of game
-- processes and implements all of the user-defined monads.
data STWrapper s = STWrapper
    { tracker   :: Tracker s
    , gameRef   :: STRef s Game
    , ninjasRef :: STVector s Ninja
    }

type IOWrapper = STWrapper RealWorld

instance (PrimMonad m, s ~ PrimState m) => MonadGame (ReaderT (STWrapper s) m) where
    game       = asks gameRef >>= readRef
    {-# INLINABLE game #-}
    alter f    = asks gameRef >>= flip modifyRef' f
    {-# INLINABLE alter #-}
    ninjas     = asks ninjasRef >>= Vector.freeze <&> toList
    {-# INLINABLE ninjas #-}
    ninja i    = asks ninjasRef >>= flip MVector.unsafeRead (Slot.toInt i)
    {-# INLINABLE ninja #-}
    write i x  = asks ninjasRef >>= \xs ->
        MVector.unsafeWrite xs (Slot.toInt i) x
    {-# INLINABLE write #-}
    modify i f = asks ninjasRef >>= \xs ->
        MVector.unsafeModify xs f (Slot.toInt i)
    {-# INLINABLE modify #-}
    modifyAll f = asks ninjasRef >>= \xs ->
        mapM_ (MVector.unsafeModify xs f . Slot.toInt) Slot.all
    {-# INLINABLE modifyAll #-}

instance (PrimMonad m, s ~ PrimState m) => MonadHook (ReaderT (STWrapper s) m) where
    action Skill{name} ns ns'  = asks tracker
        >>= Tracker.trackAction name ns ns'
    chakra Skill{name} ch ch'  = asks tracker
        >>= Tracker.trackChakra name ch ch'
    trap Trap{name, user} targ = asks tracker
        >>= Tracker.trackTrap name user targ
    trigger tr targ            = asks tracker
        >>= Tracker.trackTrigger tr targ
    turn p ns ns'              = asks tracker
        >>= Tracker.trackTurn p ns ns'

fromInfo :: ∀ m. PrimMonad m => GameInfo -> m (STWrapper (PrimState m))
fromInfo info@GameInfo{game, ninjas} = do
    tracker   <- Tracker.fromInfo info
    gameRef   <- newRef game
    ninjasRef <- Vector.thaw $ fromList ninjas
    return STWrapper { tracker, gameRef, ninjasRef }

runGame :: ∀ m. PrimMonad m
        => GameInfo -> ReaderT (STWrapper (PrimState m)) m () -> m Wrapper
runGame info f = (fromInfo info) >>= runReaderT do
    f
    unsafeFreeze =<< ask

-- | Replaces 'gameRef' and 'ninjasRef' of the former with the latter.
-- Does not affect 'tracker'.
replace :: ∀ m. PrimMonad m => Wrapper -> STWrapper (PrimState m) -> m ()
replace Wrapper{game, ninjas} STWrapper{gameRef, ninjasRef} = do
    writeRef gameRef game
    MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

data Wrapper = Wrapper
    { progress :: [Progress]
    , game     :: Game
    , ninjas   :: Vector Ninja
    }

freeze :: ∀ m. MonadGame m => m Wrapper
freeze = do
    game   <- P.game
    ninjas <- P.ninjas
    return Wrapper { progress = mempty, game, ninjas = fromList ninjas }

-- | The STWrapper may not be used after this operation.
unsafeFreeze :: ∀ m. PrimMonad m => STWrapper (PrimState m) -> m Wrapper
unsafeFreeze STWrapper{tracker, gameRef, ninjasRef} = do
    progress <- Tracker.unsafeFreeze tracker
    game     <- readRef gameRef
    ninjas   <- Vector.unsafeFreeze ninjasRef
    return Wrapper { progress, game, ninjas }

thaw :: ∀ m. PrimMonad m => Wrapper -> m (STWrapper (PrimState m))
thaw Wrapper{game, ninjas} = do
    gameRef <- newRef game
    ninjasRef <- Vector.thaw ninjas
    return STWrapper { tracker = Tracker.empty, gameRef, ninjasRef }

--  | Encodes game state into a form suitable for sending to the client.
toTurn :: Player -> Wrapper -> Turn
toTurn player Wrapper{ninjas, game} = Turn.new player (toList ninjas) game

instance MonadRandom m => MonadRandom (ReaderT Wrapper m)
instance MonadRandom m => MonadRandom (ReaderT (STWrapper s) m)
