module Model.Variant
  ( Variant(..)
  , cooldown
  , none
  ) where

import ClassyPrelude.Yesod
import Model.Internal (Variant(..))

cooldown :: Variant -> Int
cooldown v
  | ownCd v   = variant v
  | otherwise = 0

none :: Variant
none = Variant 0 False "" False 0

