{-# LANGUAGE DeriveAnyClass #-}

module Game.Model.Face (Face(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Class.TurnBased (TurnBased(..))
import Game.Model.Slot (Slot)
import Game.Model.Variant (Varying)

-- | Changes the character icon of a 'Model.Ninja.Ninja'.
data Face = Face { icon :: Text
                 , user :: Slot
                 , dur  :: Varying
                 } deriving (Eq, Ord, Show, Read, Generic, ToJSON)
instance TurnBased Face where
    getDur        = getDur . dur
    setDur x vari = vari { dur = setDur x $ dur vari }

