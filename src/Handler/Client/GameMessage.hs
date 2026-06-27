-- | Handles API routes and WebSockets related to queueing.
module Handler.Client.GameMessage (GameMessage(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Handler.Client.Reward (Reward)
import Handler.Play.Turn (Turn)

-- | A message sent through the websocket to the client.
-- This definition is exported so that @elm-bridge@ sends it over to the client.
data GameMessage
    = Play Turn
    | Rewards [Reward]
    deriving (Generic)

instance ToJSON GameMessage
