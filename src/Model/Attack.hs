module Model.Attack
  ( Attack(..)
  ) where

import ClassyPrelude

data Attack
    = Afflict
    | Pierce
    | Damage
    | Demolish
    deriving (Eq)
