module Mission.Progress (Progress(..)) where

import Prelude (Int)

import Data.Text (Text)

data Progress = Progress { name :: Text, objective :: Int, amount :: Int }
