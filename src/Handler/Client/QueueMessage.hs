-- | Handles API routes and WebSockets related to queueing.
module Handler.Client.QueueMessage
    ( QueueMessage(..)
    , QueueFailure(..)
    ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Handler.Play.GameInfo (GameInfo)

-- | Error messages sent to the client.
data QueueFailure
    = AlreadyQueued
    | Canceled
    | InvalidTeam String
    | Locked [Text]
    | NotFound
    | SocketError String
    deriving (Eq, Ord, Show, Generic)

instance ToJSON QueueFailure

-- | A message sent through the websocket to the client.
-- This definition is exported so that @elm-bridge@ sends it over to the client.
data QueueMessage
    = Fail QueueFailure
    | Info GameInfo
    | Ping
    deriving (Generic)

instance ToJSON QueueMessage
