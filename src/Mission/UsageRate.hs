module Mission.UsageRate
  ( UsageRate(..)
  , new
  ) where

import ClassyPrelude

import           Application.Model (Usage(..))
import           Game.Model.Character (Character)

data UsageRate = UsageRate
    { pickRate  :: Float
    , winRate   :: Float
    , character :: Character
} deriving (Eq, Ord)


new :: Character -> Usage -> UsageRate
new character usage = UsageRate
    { character
    , winRate  = toRate (usageWins usage)   (usageLosses usage)
    , pickRate = toRate (usagePicked usage) (usageUnpicked usage)
    }
  where
    toRate x y = 100 * fromIntegral x / (fromIntegral x + fromIntegral y)
