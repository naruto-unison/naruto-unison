module Game.Model.Attack (Attack(..)) where

import ClassyPrelude

data Attack
    = Afflict
    | Pierce
    | Damage
    | Demolish
    deriving (Bounded, Enum, Eq, Ord, Show, Read)
