{-# LANGUAGE DeriveAnyClass #-}
module Model.Player
  ( Player(..)
  , opponent
  , from
  ) where

import ClassyPrelude hiding (even)

import Data.Aeson (ToJSON)

import qualified Class.Parity as Parity
import           Class.Parity (Parity)

data Player
    = A | B
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

instance Parity Player where
    even A = True
    even B = False

from :: ∀ a. Parity a => a -> Player
from (Parity.even -> True) = A
from _                     = B

opponent :: Player -> Player
opponent A = B
opponent B = A
