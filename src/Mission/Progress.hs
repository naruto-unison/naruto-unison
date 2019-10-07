module Mission.Progress (Progress(..)) where

import ClassyPrelude

data Progress = Progress { character :: Text
                         , objective :: Int
                         , amount    :: Int
                         } deriving (Eq, Show, Read)
