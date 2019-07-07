{-# LANGUAGE DeriveAnyClass #-}
module Model.Defense  (Defense(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import qualified Class.Labeled
import           Class.Labeled (Labeled)
import           Class.TurnBased (TurnBased(..))
import           Model.Slot (Slot)

-- | Destructible defense.
data Defense = Defense { amount :: Int
                       , user   :: Slot
                       , name   :: Text
                       , dur    :: Int
                       } deriving (Eq, Ord, Show, Read, Generic, ToJSON)
instance TurnBased Defense where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Defense where
    name = name
    user = user
