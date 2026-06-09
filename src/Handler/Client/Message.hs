-- | Handles API routes and WebSockets related to gameplay.
module Handler.Client.Message
    ( Failure(..)
    , Message(..)
    ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Handler.Client.Reward (Reward)
import Handler.Play.GameInfo (GameInfo)
import Handler.Play.Turn (Turn)

-- | Error messages sent to the client.
data Failure
    = AlreadyQueued
    | Canceled
    | InvalidTeam String
    | Locked [Text]
    | NotFound
    | SocketError String
    deriving (Eq, Ord, Show, Generic)

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
