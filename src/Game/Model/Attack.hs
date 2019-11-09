module Game.Model.Attack (Attack(..)) where

import ClassyPrelude

-- | Damage type.
data Attack
    = Demolish -- ^ Only damages destructible barrier/defense.
    | Afflict -- ^ Ignores damage reduction and destructible defense.
    | Pierce -- ^ Ignores damage reduction.
    | Damage -- ^ Deals damage normally.
    deriving (Bounded, Enum, Eq, Ord, Show, Read)
