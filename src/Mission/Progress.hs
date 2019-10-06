module Mission.Progress (Progress(..)) where

import Prelude (Int)

import Data.Text (Text)

data Progress = Progress { character :: Text
                         , objective :: Int
                         , amount :: Int
                         }
