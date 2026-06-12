module Handler.Client.Socket
    ( Connection
    , ConnectionException(..)
    , WebSocketsData(..)
    , receiveData
    , sendJSONData
    , sendTextData
    , sendPing
    , withSocket
    ) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import           Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Network.WebSockets as WS
import           Network.WebSockets (Connection, ConnectionException(..), ConnectionOptions(..), CompressionOptions(..), WebSocketsData(..))
import           Yesod.Core (MonadHandler)
import           Yesod.WebSockets (webSocketsOptions)

withSocket :: ∀ m. (MonadUnliftIO m, MonadHandler m)
           => (Connection -> m ()) -> m ()
withSocket f = webSocketsOptions options $ ask >>= lift . f
  where
    options = WS.defaultConnectionOptions
        { connectionCompressionOptions = PermessageDeflateCompression
                                         WS.defaultPermessageDeflate
        }

receiveData :: ∀ m. (MonadIO m)
            => Connection -> m (Either SomeException LByteString)
receiveData socket = liftIO $ tryAny $ WS.receiveData socket

sendTextData :: ∀ m. MonadIO m
             => Connection -> LByteString -> m (Either SomeException ())
sendTextData socket message = liftIO $ tryAny $ WS.sendTextData socket message

sendJSONData :: ∀ a m. (MonadIO m, ToJSON a)
             => Connection -> a -> m (Either SomeException ())
sendJSONData socket x = sendTextData socket $ encodingToLazyByteString
                      $ toEncoding x

sendPing :: ∀ m. MonadIO m
         => Connection -> LByteString -> m (Either SomeException ())
sendPing socket message = liftIO $ tryAny $ WS.sendPing socket message
