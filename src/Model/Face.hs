{-# LANGUAGE DeriveAnyClass #-}

module Model.Face (Face(..)) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Class.TurnBased (TurnBased(..))
import Model.Slot (Slot)
import Model.Variant (Varying)

-- | Changes the character icon of a 'Model.Ninja.Ninja'.
data Face = Face { icon :: Text
                 , user :: Slot
                 , dur  :: Varying
                 } deriving (Eq, Ord, Show, Read, Generic, ToJSON)
instance TurnBased Face where
    getDur        = getDur . dur
    setDur x vari = vari { dur = setDur x $ dur vari }

