-- | Handles API routes and WebSockets related to gameplay.
module Handler.Client.Message
    ( Failure(..)
    , Message(..)
    , send
    , ping
    ) where

import ClassyPrelude

import           Data.Aeson (ToJSON, toEncoding)
import qualified Data.Aeson.Encoding as Encoding

import           Class.Sockets (MonadSockets)
import qualified Class.Sockets as Sockets
import           Handler.Client.Reward (Reward)
import           Handler.Play.GameInfo (GameInfo)
import           Handler.Play.Turn (Turn)
import Control.Monad.Error.Class (MonadError(..))

-- | Error messages sent to the client.
data Failure
    = AlreadyQueued
    | Canceled
    | InvalidTeam String
    | Locked [Text]
    | NotFound
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Failure

-- | A message sent through the websocket to the client.
-- This definition is exported so that @elm-bridge@ sends it over to the client.
data Message
    = Fail Failure
    | Info GameInfo
    | Ping
    | Play Turn
    | Rewards [Reward]
    deriving (Generic)

instance ToJSON Message

send :: ∀ m. MonadSockets m => Message -> m ()
send x = Sockets.send . Encoding.encodingToLazyByteString $ toEncoding x

ping :: ∀ m. (MonadError Failure m, MonadSockets m) => m ()
ping = do
    send Ping
    pong <- Sockets.receive {-! BLOCKS !-}
    when (pong == "cancel")
        $ throwError Canceled
