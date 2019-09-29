-- | A container that implements 'MonadGame' and 'MonadRandom'.
-- It comes in pure, ST, and IO flavors.
-- Functions with such monadic constraints will almost always be called on MTL
-- stacks built on the foundation of one of these three types.
module Handler.Play.Wrapper
  ( STWrapper(..), fromInfo, replace, thaw
  , IOWrapper
  , Wrapper(..), freeze, toJSON
  ) where

import ClassyPrelude

import           Control.Monad.ST (RealWorld, ST, stToIO)
import           Control.Monad.Trans.State.Strict (StateT, gets, modify')
import           Data.Aeson (Value)
import           Data.STRef
import qualified Data.Vector as Vector
import           Data.Vector ((//))
import qualified Data.Vector.Mutable as MVector
import           Data.Vector.Mutable (STVector)

import           Util ((!!))
import qualified Class.Play as P
import           Class.Play (MonadGame)
import           Class.Random (MonadRandom(..))
import           Class.Sockets (MonadSockets)
import           Game.Model.Game (Game)
import qualified Handler.Play.GameInfo as GameInfo
import           Handler.Play.GameInfo (GameInfo)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Player as Player
import           Game.Model.Player (Player)
import qualified Game.Model.Slot as Slot

data STWrapper s = STWrapper { gameRef   :: STRef s Game
                             , ninjasRef :: STVector s Ninja
                             }

instance MonadGame (ReaderT (STWrapper s) (ST s)) where
    game        = asks gameRef   >>= lift . readSTRef
    alter f     = asks gameRef   >>= lift . flip modifySTRef' f
    ninjas      = asks ninjasRef >>= lift . Vector.freeze
    ninja i     = asks ninjasRef >>= lift . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks ninjasRef >>= \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks ninjasRef >>= \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

type IOWrapper = STWrapper RealWorld

instance MonadIO m => MonadGame (ReaderT IOWrapper m) where
    game        = asks gameRef   >>= liftIO . stToIO . readSTRef
    alter f     = asks gameRef   >>= liftIO . stToIO . flip modifySTRef' f
    ninjas      = asks ninjasRef >>= liftIO . Vector.freeze
    ninja i     = asks ninjasRef >>= liftIO . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks ninjasRef >>= liftIO . \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks ninjasRef >>= liftIO . \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

fromInfo :: ∀ s. GameInfo -> ST s (STWrapper s)
fromInfo info = STWrapper <$> newSTRef (GameInfo.game info)
                          <*> Vector.thaw (GameInfo.ninjas info)

replace :: ∀ s. Wrapper -> STWrapper s -> ST s ()
replace Wrapper{game, ninjas} STWrapper{gameRef, ninjasRef} = do
    writeSTRef gameRef game
    MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

adjustVec :: ∀ a. (a -> a) -> Int -> Vector a -> Vector a
adjustVec f i = Vector.modify \xs -> MVector.modify xs f i

updateVec :: ∀ a. Int -> a -> Vector a -> Vector a
updateVec i x xs = xs // [(i, x)]

data Wrapper = Wrapper { game   :: Game
                       , ninjas :: Vector Ninja
                       }

instance MonadGame (StateT Wrapper Identity) where
    game        = gets game
    alter f     = modify' \x -> x { game = f $ game x }
    ninjas      = gets ninjas
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

freeze :: ∀ m. MonadGame m => m Wrapper
freeze = Wrapper <$> P.game <*> P.ninjas

thaw :: ∀ s. Wrapper -> ST s (STWrapper s)
thaw Wrapper{game, ninjas} = STWrapper <$> newSTRef game <*> Vector.thaw ninjas

toJSON :: Player -> Wrapper -> Value
toJSON p Wrapper{game, ninjas} = GameInfo.gameToJSON p ninjas game

instance MonadRandom m => MonadRandom (ReaderT Wrapper m)
instance MonadRandom m => MonadRandom (ReaderT (STWrapper s) m)
instance MonadSockets m => MonadSockets (ReaderT Wrapper m)
instance MonadSockets m => MonadSockets (ReaderT (STWrapper s) m)
