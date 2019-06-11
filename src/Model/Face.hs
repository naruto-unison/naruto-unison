{-# LANGUAGE DeriveAnyClass #-}
module Model.Face
  ( Face(..)
  ) where

import ClassyPrelude.Yesod

import Class.TurnBased (TurnBased(..))
import Model.Slot (Slot)

-- | Changes the character icon of a 'Ninja'.
data Face = Face { icon   :: Text
                 , source :: Slot
                 , dur    :: Int
                 } deriving (Eq, Generic, ToJSON)
instance TurnBased Face where
    getDur     = dur
    setDur d x = x { dur = d }
