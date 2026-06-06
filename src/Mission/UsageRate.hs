module Mission.UsageRate
  ( UsageRate(..)
  , new
  ) where

import ClassyPrelude

import           Application.Model (Usage(Usage))
import qualified Application.Model
import           Game.Model.Character (Character)
import           OrphanInstances.Character ()

data UsageRate = UsageRate
    { pickRate  :: Float
    , winRate   :: Float
    , character :: Character
} deriving (Eq, Ord, Show, Read)


new :: Character -> Usage -> UsageRate
new character Usage{usageWins, usageLosses, usagePicked, usageUnpicked} =
    UsageRate
    { character
    , winRate  = toRate usageWins usageLosses
    , pickRate = toRate usagePicked usageUnpicked
    }
  where
    toRate x y = 100 * fromIntegral x / (fromIntegral x + fromIntegral y)
