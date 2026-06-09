{-# OPTIONS_HADDOCK prune #-}

module Mission.Store
  ( Store
  , resetToZero
  ) where

import ClassyPrelude

-- | Some mission objectives require a persistent object for tracking progress.
type Store = IntSet

-- | Add this to mission progress in order to reset it to 0.
resetToZero :: Int
resetToZero = minBound
