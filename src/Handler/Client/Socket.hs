module Handler.Client.Socket
    ( Connection
    , ConnectionException(..)
    , WebSocketsData(..)
    , receiveData
    , connectionOptions
    , sendJSONData
    , sendPing
    , sendTextData
    ) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))
import qualified Network.WebSockets as WS
import           Network.WebSockets (Connection, ConnectionException(..), ConnectionOptions(..), WebSocketsData(..))
import Data.Aeson.Encoding (encodingToLazyByteString)

connectionOptions :: ConnectionOptions
connectionOptions = WS.defaultConnectionOptions

receiveData :: ∀ m. (MonadIO m) => Connection -> m LByteString
receiveData socket = liftIO $ WS.receiveData socket

sendJSONData :: ∀ m a. (MonadIO m, ToJSON a) => Connection -> a -> m ()
sendJSONData socket x = sendTextData socket $ encodingToLazyByteString
                      $ toEncoding x

sendTextData :: ∀ m. MonadIO m => Connection -> LByteString -> m ()
sendTextData socket message = liftIO $ WS.sendTextData socket message

sendPing :: ∀ m. MonadIO m => Connection -> LByteString -> m ()
sendPing socket message = liftIO $ WS.sendPing socket message
