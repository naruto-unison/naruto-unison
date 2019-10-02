-- | A container that implements 'MonadGame' and 'MonadRandom'.
-- It comes in pure, ST, and IO flavors.
-- Functions with such monadic constraints will almost always be called on MTL
-- stacks built on the foundation of one of these three types.
module Handler.Play.Wrapper
  ( STWrapper(..), fromInfo, replace, thaw
  , IOWrapper
  , Wrapper(..), freeze, unsafeFreeze, toJSON
  ) where

import ClassyPrelude

import           Control.Monad.ST (RealWorld, ST)
import           Control.Monad.Trans.State.Strict (StateT, gets, modify')
import           Data.Aeson (Value)
import           Data.STRef
import qualified Data.Vector as Vector
import           Data.Vector ((//))
import qualified Data.Vector.Mutable as MVector
import           Data.Vector.Mutable (STVector)

import           Util ((!!), liftST)
import qualified Class.Play as P
import           Class.Play (MonadGame)
import           Class.Random (MonadRandom(..))
import           Class.Hook (MonadHook(..))
import           Class.Sockets (MonadSockets)
import           Game.Model.Game (Game)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Player as Player
import           Game.Model.Player (Player)
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Slot as Slot
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.GameInfo (GameInfo)
import qualified Handler.Play.Tracker as Tracker
import           Handler.Play.Tracker (Tracker)
import           Mission.Progress (Progress)


data STWrapper s = STWrapper { tracker   :: Tracker s
                             , gameRef   :: STRef s Game
                             , ninjasRef :: STVector s Ninja
                             }

type IOWrapper = STWrapper RealWorld

instance MonadGame (ReaderT (STWrapper s) (ST s)) where
    game        = asks gameRef   >>= lift . readSTRef
    alter f     = asks gameRef   >>= lift . flip modifySTRef' f
    ninjas      = asks ninjasRef >>= (toList <$>) . lift . Vector.freeze
    ninja i     = asks ninjasRef >>= lift . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks ninjasRef >>= \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks ninjasRef >>= \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

instance MonadIO m => MonadGame (ReaderT IOWrapper m) where
    game        = asks gameRef   >>= liftST . readSTRef
    alter f     = asks gameRef   >>= liftST . flip modifySTRef' f
    ninjas      = asks ninjasRef >>= (toList <$>) . liftIO . Vector.freeze
    ninja i     = asks ninjasRef >>= liftIO . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks ninjasRef >>= liftIO . \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks ninjasRef >>= liftIO . \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

instance MonadHook (ReaderT (STWrapper s) (ST s)) where
    action skill ns = do
        STWrapper{tracker, ninjasRef} <- ask
        lift $ Tracker.trackAction tracker (Skill.name skill) ns . toList
               =<< Vector.freeze ninjasRef
    turn = do
        STWrapper{tracker, ninjasRef} <- ask
        lift $ Tracker.trackTurn tracker . toList =<< Vector.freeze ninjasRef

instance MonadIO m => MonadHook (ReaderT IOWrapper m) where
    action skill ns = do
        STWrapper{tracker, ninjasRef} <- ask
        liftST $ Tracker.trackAction tracker (Skill.name skill) ns . toList
                  =<< Vector.freeze ninjasRef
    turn = do
        STWrapper{tracker, ninjasRef} <- ask
        liftST $ Tracker.trackTurn tracker . toList =<< Vector.freeze ninjasRef

fromInfo :: ∀ s. GameInfo -> ST s (STWrapper s)
fromInfo info = STWrapper <$> Tracker.fromInfo info
                          <*> newSTRef (GameInfo.game info)
                          <*> Vector.thaw (fromList $ GameInfo.ninjas info)

-- | Replaces 'gameRef' and 'ninjasRef' of the former with the latter.
-- Does not affect 'tracker'.
replace :: ∀ s. Wrapper -> STWrapper s -> ST s ()
replace Wrapper{game, ninjas} STWrapper{gameRef, ninjasRef} = do
    writeSTRef gameRef game
    MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

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
    <*> readSTRef gameRef
    <*> Vector.unsafeFreeze ninjasRef

thaw :: ∀ s. Wrapper -> ST s (STWrapper s)
thaw Wrapper{game, ninjas} = STWrapper Tracker.empty
                             <$> newSTRef game
                             <*> Vector.thaw ninjas

toJSON :: Player -> Wrapper -> Value
toJSON p Wrapper{game, ninjas} = GameInfo.gameToJSON p (toList ninjas) game

instance MonadRandom m => MonadRandom (ReaderT Wrapper m)
instance MonadRandom m => MonadRandom (ReaderT (STWrapper s) m)
instance MonadSockets m => MonadSockets (ReaderT Wrapper m)
instance MonadSockets m => MonadSockets (ReaderT (STWrapper s) m)
