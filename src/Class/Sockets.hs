-- | Monadic constraints for websockets communication.
-- In practice, this is used to automatically lift "Yesod.WebSockets".
module Class.Sockets
  ( MonadSockets(..)
  , sendJson
  ) where

import ClassyPrelude

import           Control.Monad.Trans.Accum (AccumT)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Select (SelectT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Control.Monad.Trans.Writer (WriterT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Aeson (ToJSON, toEncoding)
import qualified Data.Aeson.Encoding as Encoding
import qualified System.Random.MWC as Random
import qualified Yesod.WebSockets as WebSockets
import           Yesod.WebSockets (WebSocketsT)

import Util (Lift)

sendJson :: ∀ m a. (MonadSockets m, ToJSON a) => a -> m ()
sendJson val = send . Encoding.encodingToLazyByteString $ toEncoding val

class Monad m => MonadSockets m where
    receive :: m Text
    send    :: LByteString -> m ()

    default receive :: Lift MonadSockets m => m Text
    receive = lift receive
    {-# INLINE receive #-}
    default send :: Lift MonadSockets m => LByteString -> m ()
    send = lift . send
    {-# INLINE send #-}

instance MonadIO m => MonadSockets (WebSocketsT m) where
    receive = WebSockets.receiveData
    send    = WebSockets.sendTextData

instance MonadSockets m => MonadSockets (ExceptT e m)
instance MonadSockets m => MonadSockets (IdentityT m)
instance MonadSockets m => MonadSockets (MaybeT m)
instance MonadSockets m => MonadSockets (SelectT r m)
instance MonadSockets m => MonadSockets (StateT r m)
instance MonadSockets m => MonadSockets (ReaderT (Random.Gen s) m)
instance (MonadSockets m, Monoid w) => MonadSockets (WriterT w m)
instance (MonadSockets m, Monoid w) => MonadSockets (AccumT w m)
