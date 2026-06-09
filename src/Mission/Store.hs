{-# OPTIONS_HADDOCK prune #-}

module Mission.Store
  ( Store
  , resetToZero
  ) where

import ClassyPrelude

import Game.Model.Slot (SlotSet)

-- | Some mission objectives require a persistent object for tracking progress.
type Store = SlotSet

-- | Add this to mission progress in order to reset it to 0.
resetToZero :: Int
resetToZero = minBound
