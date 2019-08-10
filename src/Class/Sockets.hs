module Class.Sockets
  ( MonadSockets(..), SocketsT
  , sendJson
  , run
  ) where

import ClassyPrelude

import           Control.Monad.Loops (iterateUntil)
import           Control.Monad.Trans.Accum (AccumT)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Trans.Select (SelectT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Control.Monad.Trans.Writer (WriterT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Aeson (ToJSON, toEncoding)
import qualified Data.Aeson.Encoding as Encoding
import qualified Network.WebSockets as WebSockets
import           UnliftIO.Concurrent (forkIO, killThread)
import           Yesod.Core (MonadHandler(..), MonadLogger)
import           Yesod.WebSockets (WebSocketsT, webSockets)

import Core.Util (Lift)

sendJson :: ∀ m a. (MonadSockets m, ToJSON a) => a -> m ()
sendJson = send . Encoding.encodingToLazyByteString . toEncoding

run :: ∀ m. (MonadUnliftIO m, MonadHandler m) => SocketsT m () -> m ()
run = webSockets . fromYesod

fromYesod :: ∀ m a. MonadUnliftIO m => SocketsT m a -> WebSocketsT m a
fromYesod f = do
    conn <- ask
    bracket (acquire conn) (killThread . snd) $ runReaderT (runSocketsT f) . fst
  where
    acquire conn = liftIO do
        mvar <- newEmptyMVar
        proc <- forkIO . forever $ WebSockets.receiveData conn
                                   >>= putMVar mvar
        return (mvar, proc)

class Monad m => MonadSockets m where
    receive :: m Text
    clear   :: m ()
    send    :: LByteString -> m ()

    default receive :: Lift MonadSockets m => m Text
    receive = lift receive
    {-# INLINE receive #-}
    default clear :: Lift MonadSockets m => m ()
    clear = lift clear
    {-# INLINE clear #-}
    default send :: Lift MonadSockets m => LByteString -> m ()
    send = lift . send
    {-# INLINE send #-}

-- | A 'WebSocketsT' wrapper to permit asynchronous listening and timing out
-- receiveData requests without closing the socket.
newtype SocketsT m a =
    SocketsT { runSocketsT :: ReaderT (MVar Text) (WebSocketsT m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadResource)

instance MonadTrans SocketsT where
    lift = SocketsT . lift . lift
    {-# INLINE lift #-}
instance MonadUnliftIO m => MonadUnliftIO (SocketsT m) where
  askUnliftIO = SocketsT $ withUnliftIO \u ->
                return (UnliftIO (unliftIO u . runSocketsT))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = SocketsT $ withRunInIO \f -> inner (f . runSocketsT)
  {-# INLINE withRunInIO #-}

instance MonadHandler m => MonadHandler (SocketsT m) where
    type HandlerSite (SocketsT m) = HandlerSite m
    type SubHandlerSite (SocketsT m) = SubHandlerSite m
    liftHandler = lift . liftHandler
    {-# INLINE liftHandler #-}
    liftSubHandler = lift . liftSubHandler
    {-# INLINE liftSubHandler #-}

instance MonadIO m => MonadSockets (SocketsT m) where
    receive = SocketsT $ ask >>= takeMVar
    {-# INLINE receive #-}
    clear   = SocketsT $ ask >>= void . iterateUntil isNothing . tryTakeMVar
    {-# INLINE clear #-}
    send x  = SocketsT $ lift ask >>= liftIO . flip WebSockets.sendTextData x
    {-# INLINE send #-}

instance MonadSockets m => MonadSockets (ExceptT e m)
instance MonadSockets m => MonadSockets (IdentityT m)
instance MonadSockets m => MonadSockets (MaybeT m)
instance MonadSockets m => MonadSockets (SelectT r m)
instance MonadSockets m => MonadSockets (StateT r m)
instance MonadSockets m => MonadSockets (ReaderT r m)
instance (MonadSockets m, Monoid w) => MonadSockets (WriterT w m)
instance (MonadSockets m, Monoid w) => MonadSockets (AccumT w m)
