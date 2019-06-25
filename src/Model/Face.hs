{-# LANGUAGE DeriveAnyClass #-}
module Model.Face (Face(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Class.TurnBased (TurnBased(..))
import Model.Slot (Slot)

-- | Changes the character icon of a 'Model.Ninja.Ninja'.
data Face = Face { icon :: Text
                 , user :: Slot
                 , dur  :: Int
                 } deriving (Eq, Generic, ToJSON)
instance TurnBased Face where
    getDur     = dur
    setDur d x = x { dur = d }
