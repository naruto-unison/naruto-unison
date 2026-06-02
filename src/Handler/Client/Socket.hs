module Handler.Client.Socket
    ( Connection
    , ConnectionException(..)
    , WebSocketsData(..)
    , receiveData
    , sendJSONData
    , sendPing
    , sendTextData
    , withSocket
    ) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import           Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Network.WebSockets as WS
import           Network.WebSockets (Connection, ConnectionException(..), ConnectionOptions(..), CompressionOptions(..), WebSocketsData(..))
import           Yesod.Core (MonadHandler)
import           Yesod.WebSockets (webSocketsOptions)

withSocket :: ∀ m. (MonadUnliftIO m, MonadHandler m) => (Connection -> m ()) -> m ()
withSocket f = webSocketsOptions options $ ask >>= lift . f
  where
    options = WS.defaultConnectionOptions
        { connectionCompressionOptions = PermessageDeflateCompression
                                         WS.defaultPermessageDeflate
        }

receiveData :: ∀ m. (MonadIO m) => Connection -> m LByteString
receiveData socket = liftIO $ WS.receiveData socket

sendJSONData :: ∀ m a. (MonadIO m, ToJSON a) => Connection -> a -> m ()
sendJSONData socket x = sendTextData socket $ encodingToLazyByteString
                      $ toEncoding x

sendTextData :: ∀ m. MonadIO m => Connection -> LByteString -> m ()
sendTextData socket message = liftIO $ WS.sendTextData socket message

sendPing :: ∀ m. MonadIO m => Connection -> LByteString -> m ()
sendPing socket message = liftIO $ WS.sendPing socket message
