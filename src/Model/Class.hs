{-# LANGUAGE DeriveAnyClass #-}
module Model.Class (Class(..), lower, ClassSet) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import qualified Data.Enum.Memo as Enum
import           Data.Enum.Set (EnumSet)

import Class.Display (Display(..))

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
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

type ClassSet = EnumSet Word64 Class

instance ToJSON Class where
    toJSON = Enum.memoize $ toJSON . name

instance Display Class where
    display = Enum.memoize $ display . name

name :: Class -> Text
name InvisibleTraps = name Invisible
name Nonstacking    = "Non-stacking"
name NonAffliction  = "Non-affliction"
name NonMental      = "Non-mental"
name x              = tshow x

lower :: Class -> TextBuilder
lower = Enum.memoize $ display . toLower . name
