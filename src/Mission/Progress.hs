module Mission.Progress
    ( Progress(..)
    , resetToZero
    , Store
    ) where

import ClassyPrelude

import Game.Model.Slot (SlotSet)

-- | Represents a user's progress on a mission goal during a game.
-- At the end of the game, @Progress@es are collected and sent to "Mission" to
-- be inserted into the database.
data Progress = Progress
    { character :: Text
    , objective :: Int
    , amount    :: Int
    } deriving (Eq, Ord, Show)

-- | Add this to mission progress in order to reset it to 0.
resetToZero :: Int
resetToZero = minBound

-- | Some mission objectives require a persistent object for tracking progress.
type Store = SlotSet
