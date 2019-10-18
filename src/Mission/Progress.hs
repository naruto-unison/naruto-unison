module Mission.Progress (Progress(..)) where

import ClassyPrelude

-- | Represents a user's progress on a mission goal during a game.
-- At the end of the game, @Progress@es are collected and sent to "Mission" to
-- be inserted into the database.
data Progress = Progress { character :: Text
                         , objective :: Int
                         , amount    :: Int
                         } deriving (Eq, Ord, Show, Read)
