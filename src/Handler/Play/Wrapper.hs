-- | A container that implements 'MonadGame' and 'MonadRandom'.
-- It comes in pure, ST, and IO flavors.
-- Functions with such monadic constraints will almost always be called on MTL
-- stacks built on the foundation of one of these three types.
module Handler.Play.Wrapper
  ( STWrapper(..), fromInfo, replace, thaw
  , IOWrapper
  , Wrapper(..), freeze, unsafeFreeze, toTurn
  ) where

import ClassyPrelude

import           Control.Monad.ST (RealWorld, ST)
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
import           Game.Model.Copy (Copying(..))
import           Game.Model.Game (Game)
import           Game.Model.Ninja (Ninja)
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import           Handler.Play.GameInfo (GameInfo)
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.Tracker (Tracker)
import qualified Handler.Play.Tracker as Tracker
import           Handler.Play.Turn (Turn)
import qualified Handler.Play.Turn as Turn
import           Mission.Progress (Progress)
import           Util ((!!), liftST)


data STWrapper s = STWrapper { tracker   :: Tracker s
                             , gameRef   :: STRef s Game
                             , ninjasRef :: STVector s Ninja
                             }

type IOWrapper = STWrapper RealWorld

instance MonadGame (ReaderT (STWrapper s) (ST s)) where
    game        = asks gameRef   >>= lift . readRef
    alter f     = asks gameRef   >>= lift . flip modifyRef' f
    ninjas      = asks ninjasRef >>= (toList <$>) . lift . Vector.freeze
    ninja i     = asks ninjasRef >>= lift . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks ninjasRef >>= \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks ninjasRef >>= \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

instance MonadIO m => MonadGame (ReaderT IOWrapper m) where
    game        = asks gameRef   >>= liftST . readRef
    alter f     = asks gameRef   >>= liftST . flip modifyRef' f
    ninjas      = asks ninjasRef >>= (toList <$>) . liftIO . Vector.freeze
    ninja i     = asks ninjasRef >>= liftIO . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks ninjasRef >>= liftIO . \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks ninjasRef >>= liftIO . \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

trackAction :: ∀ s. Skill -> [Ninja] -> STWrapper s -> ST s ()
trackAction skill ns x = case Skill.copying skill of
    NotCopied -> Tracker.trackAction (tracker x) (Skill.name skill) ns . toList
                  =<< Vector.freeze (ninjasRef x)
    _         -> return ()

trackTurn :: ∀ s. STWrapper s -> ST s ()
trackTurn x =
    Tracker.trackTurn (tracker x) . toList =<< Vector.freeze (ninjasRef x)

instance MonadHook (ReaderT (STWrapper s) (ST s)) where
    action skill ns = lift . trackAction skill ns =<< ask
    turn = lift . trackTurn =<< ask

instance MonadIO m => MonadHook (ReaderT IOWrapper m) where
    action skill ns = liftST . trackAction skill ns =<< ask
    turn = liftST . trackTurn =<< ask

fromInfo :: ∀ s. GameInfo -> ST s (STWrapper s)
fromInfo info = STWrapper <$> Tracker.fromInfo info
                          <*> newRef (GameInfo.game info)
                          <*> Vector.thaw (fromList $ GameInfo.ninjas info)

-- | Replaces 'gameRef' and 'ninjasRef' of the former with the latter.
-- Does not affect 'tracker'.
replace :: ∀ s. Wrapper -> STWrapper s -> ST s ()
replace Wrapper{game, ninjas} STWrapper{gameRef, ninjasRef} = do
    writeRef gameRef game
    MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

-- Wrappers are pure and immutable, so these two functions are inefficient and a
-- bit silly. That's fine, because test suites can make good use of them.
-- Elsewhere, Wrappers are merely frozen snapshots of game state.

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
    action = const . const $ return ()
    turn   = return ()

freeze :: ∀ m. MonadGame m => m Wrapper
freeze = Wrapper mempty <$> P.game <*> (fromList <$> P.ninjas)

-- | The STWrapper may not be used after this operation.
unsafeFreeze :: ∀ s. STWrapper s -> ST s Wrapper
unsafeFreeze STWrapper{tracker, gameRef, ninjasRef} =
    Wrapper
    <$> Tracker.unsafeFreeze tracker
    <*> readRef gameRef
    <*> Vector.unsafeFreeze ninjasRef

thaw :: ∀ s. Wrapper -> ST s (STWrapper s)
thaw Wrapper{game, ninjas} = STWrapper Tracker.empty
                             <$> newRef game
                             <*> Vector.thaw ninjas

toTurn :: Player -> Wrapper -> Turn
toTurn player Wrapper{..} = Turn.new player (toList ninjas) game

instance MonadRandom m => MonadRandom (ReaderT Wrapper m)
instance MonadRandom m => MonadRandom (ReaderT (STWrapper s) m)
instance MonadSockets m => MonadSockets (ReaderT Wrapper m)
instance MonadSockets m => MonadSockets (ReaderT (STWrapper s) m)
