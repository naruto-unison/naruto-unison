module Game.Model.Attack (Attack(..)) where

import ClassyPrelude

-- | Damage type.
data Attack
    = Demolish -- ^ Only damages destructible barrier/defense.
    | Damage -- ^ Deals damage normally.
    | Pierce -- ^ Ignores damage reduction.
    | Afflict -- ^ Ignores damage reduction and destructible defense.
    deriving (Bounded, Enum, Eq, Ord, Show, Read)
