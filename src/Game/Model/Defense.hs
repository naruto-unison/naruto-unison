module Game.Model.Defense (Defense(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import           Game.Model.Duration (Duration)
import           Game.Model.Slot (Slot)

-- | Destructible defense.
data Defense = Defense
    { amount :: Int
    , user   :: Slot
    , name   :: Text
    , dur    :: Duration
    } deriving (Eq, Show, Read, Generic)

instance ToJSON Defense
