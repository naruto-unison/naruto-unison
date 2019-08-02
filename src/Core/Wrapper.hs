module Core.Wrapper
  ( IOWrapper(..), fromInfo, replaceIO, thawIO
  , STWrapper(..), replaceST
  , Wrapper(..), freeze, toJSON
  ) where

import ClassyPrelude


import           Control.Monad.ST (ST)
import           Control.Monad.Trans.State.Strict (StateT, gets, modify')
import           Data.Aeson (Value)
import           Data.STRef
import qualified Data.Vector as Vector
import           Data.Vector ((//))
import qualified Data.Vector.Mutable as MVector
import           Data.Vector.Mutable (IOVector, STVector)

import           Core.Util ((!!))
import qualified Class.Play as P
import           Class.Play (MonadGame)
import           Class.Random (MonadRandom(..))
import           Model.Game (Game)
import qualified Model.GameInfo as GameInfo
import           Model.GameInfo (GameInfo)
import           Model.Ninja (Ninja)
import           Model.Player (Player)
import qualified Model.Slot as Slot

data STWrapper s = STWrapper { stGame   :: STRef s Game
                             , stNinjas :: STVector s Ninja
                             }

instance MonadGame (ReaderT (STWrapper s) (ST s)) where
    game        = asks stGame   >>= lift . readSTRef
    alter f     = asks stGame   >>= lift . flip modifySTRef' f
    ninjas      = asks stNinjas >>= lift . Vector.freeze
    ninja i     = asks stNinjas >>= lift . flip MVector.unsafeRead (Slot.toInt i)
    write i x   = asks stNinjas >>= \xs ->
                  MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f  = asks stNinjas >>= \xs ->
                  MVector.unsafeModify xs f $ Slot.toInt i

replaceST :: ∀ s. Wrapper -> ReaderT (STWrapper s) (ST s) ()
replaceST Wrapper{..} = do
    gameRef <- asks stGame
    lift $ writeSTRef gameRef game
    ninjasRef <- asks stNinjas
    MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

data IOWrapper = IOWrapper { ioGame   :: IORef Game
                           , ioNinjas :: IOVector Ninja
                           }

instance MonadIO m => MonadGame (ReaderT IOWrapper m) where
    game       = asks ioGame   >>= liftIO . readIORef
    alter f    = asks ioGame   >>= liftIO . flip modifyIORef' f
    ninjas     = asks ioNinjas >>= liftIO . Vector.freeze
    ninja i    = asks ioNinjas >>=
                 liftIO . flip MVector.unsafeRead (Slot.toInt i)
    write i x  = asks ioNinjas >>= \xs ->
                 liftIO $ MVector.unsafeWrite xs (Slot.toInt i) x
    modify i f = asks ioNinjas >>= \xs ->
                 liftIO . MVector.modify xs f $ Slot.toInt i

fromInfo :: ∀ m. MonadIO m => GameInfo -> m IOWrapper
fromInfo info = do
    ioGame   <- newIORef $ GameInfo.game info
    ioNinjas <- liftIO . Vector.thaw $ GameInfo.ninjas info
    return IOWrapper{..}

replaceIO :: ∀ m. MonadIO m => Wrapper -> ReaderT IOWrapper m ()
replaceIO Wrapper{..} = do
    gameRef <- asks ioGame
    lift $ writeIORef gameRef game
    ninjasRef <- asks ioNinjas
    liftIO $ MVector.unsafeCopy ninjasRef =<< Vector.thaw ninjas

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
    shuffle  = return . id

freeze :: ∀ m. MonadGame m => m Wrapper
freeze = Wrapper <$> P.game <*> P.ninjas

thawIO :: MonadIO m => Wrapper -> m IOWrapper
thawIO Wrapper{..} = IOWrapper <$> newIORef game <*> liftIO (Vector.thaw ninjas)

toJSON :: Player -> Wrapper -> Value
toJSON player Wrapper{..} = GameInfo.gameToJSON player ninjas game

instance MonadRandom m => MonadRandom (ReaderT Wrapper m)
instance MonadRandom m => MonadRandom (ReaderT IOWrapper m)
instance MonadRandom m => MonadRandom (ReaderT (STWrapper s) m)
