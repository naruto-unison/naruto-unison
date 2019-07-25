{-# LANGUAGE DeriveAnyClass        #-}
module Model.Class (Class(..), ClassSet) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Core.EnumSet (EnumSet)

-- | Qualifiers of 'Model.Skill.Skill's and 'Model.Status.Status'es.
data Class
    = Invisible
    | InvisibleTraps
    | Soulbound
    -- Tags
    | Bane
    | Summon
    -- Distance
    | Melee
    | Ranged
    -- Type
    | Chakra
    | Physical
    | Mental
    -- Limits
    | Nonstacking
    | Single
    | Extending
    -- Prevention
    | Bypassing
    | Uncounterable
    | Unreflectable
    | Unremovable
    | Necromancy
    -- Fake (Hidden)
    | All
    | Harmful
    | Healing
    | Hidden
    | Affliction
    | NonAffliction
    | NonMental
    | Resource -- ^ Display stacks separately
    | Direct
    -- Chakra (Hidden)
    | Bloodline
    | Genjutsu
    | Ninjutsu
    | Taijutsu
    | Random
    deriving (Bounded, Enum, Eq, Ord, Generic, ToJSON)

type ClassSet = EnumSet Word64 Class

instance Show Class where
    show Invisible      = "Invisible"
    show InvisibleTraps = "Invisible"
    show Soulbound      = "Soulbound"
    show Bane           = "Bane"
    show Summon         = "Summon"
    show Melee          = "Melee"
    show Ranged         = "Ranged"
    show Chakra         = "Chakra"
    show Physical       = "Physical"
    show Mental         = "Mental"
    show Nonstacking    = "Non-stacking"
    show Single         = "Single"
    show Extending      = "Extending"
    show Bypassing      = "Bypassing"
    show Uncounterable  = "Uncounterable"
    show Unreflectable  = "Unreflectable"
    show Unremovable    = "Unremovable"
    show Necromancy     = "Necromancy"
    show All            = "All"
    show Harmful        = "Harmful"
    show Healing        = "Healing"
    show Hidden         = "Hidden"
    show Affliction     = "Affliction"
    show NonAffliction  = "Non-affliction"
    show NonMental      = "Non-mental"
    show Resource       = "Resource"
    show Direct         = "Direct"
    show Bloodline      = "Bloodline"
    show Genjutsu       = "Genjutsu"
    show Ninjutsu       = "Ninjutsu"
    show Taijutsu       = "Taijutsu"
    show Random         = "Random"
