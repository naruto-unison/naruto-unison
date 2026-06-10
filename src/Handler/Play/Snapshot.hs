module Handler.Play.Snapshot (Snapshot(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Game.Model.Chakras (Chakras)
import Game.Model.Ninja (Ninja)

data Snapshot = Snapshot
    { chakra :: Chakras
    , ninjas :: [Ninja]
    } deriving (Generic)

instance ToJSON Snapshot
