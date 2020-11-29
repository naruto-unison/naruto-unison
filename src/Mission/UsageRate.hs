module Mission.UsageRate
  ( UsageRate(..)
  , new
  ) where

import ClassyPrelude

import           Application.Model (Usage(..))
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character

data UsageRate = UsageRate { character :: Character
                           , winRate   :: Float
                           , pickRate  :: Float
                           } deriving (Eq)
instance Ord UsageRate where
    x `compare` y = mconcat [ winCompare
                            , pickRate y `compare` pickRate x
                            , (compare `on` Character.category . character) x y
                            , (compare `on` Character.name . character) x y
                            ]
      where
        xNaN = isNaN $ winRate x
        yNaN = isNaN $ winRate y
        winCompare
          | xNaN && yNaN       = EQ
          | not (xNaN) && yNaN = LT
          | xNaN && not (yNaN) = GT
          | otherwise          = winRate y `compare` winRate x

new :: Character -> Usage -> UsageRate
new character usage =
    UsageRate { character
              , winRate  = toRate (usageWins usage)   (usageLosses usage)
              , pickRate = toRate (usagePicked usage) (usageUnpicked usage)
              }
  where
    toRate x y = 100 * fromIntegral x / (fromIntegral x + fromIntegral y)
