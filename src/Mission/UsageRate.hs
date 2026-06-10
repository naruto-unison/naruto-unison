module Mission.UsageRate
  ( UsageRate(..)
  , new
  ) where

import ClassyPrelude

import           Application.Model.Usage (Usage(Usage))
import qualified Application.Model.Usage
import           Game.Model.Character (Character)

data UsageRate = UsageRate
    { pickRate  :: Float
    , winRate   :: Float
    , character :: Character
} deriving (Eq, Ord, Show)


new :: Character -> Usage -> UsageRate
new character Usage{wins, losses, picked, unpicked} =
    UsageRate
    { character
    , winRate  = toRate wins losses
    , pickRate = toRate picked unpicked
    }
  where
    toRate x y = 100 * fromIntegral x / (fromIntegral x + fromIntegral y)
