{-# OPTIONS_GHC -fno-warn-orphans #-}

module OrphanInstances where

import ClassyPrelude

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

import Game.Model.Effect (Amount)
import Game.Model.Attack (Attack)

shrinkBoundedEnum :: ∀ a. (Bounded a, Enum a, Eq a) => a -> [a]
shrinkBoundedEnum x
  | x == minBound = []
  | otherwise     = [minBound..pred x]

instance Arbitrary Amount where
    arbitrary = arbitraryBoundedEnum
    shrink    = shrinkBoundedEnum

instance Arbitrary Attack where
    arbitrary = arbitraryBoundedEnum
    shrink    = shrinkBoundedEnum
