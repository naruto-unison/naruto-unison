{-# LANGUAGE DeriveAnyClass #-}
module Model.Class (Class(..), lower, ClassSet) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import           Data.EnumSet (EnumSet)
import qualified Data.Vector as Vector

import Class.Display (Display(..))
import Core.Util ((!!))

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
    toJSON = toJSON . (textCache !!) . fromEnum

instance Display Class where
    display = (displayCache !!) . fromEnum

textCache :: Vector Text
textCache = Vector.generate (1 + fromEnum (maxBound :: Class)) $ f . toEnum
  where
    f InvisibleTraps = f Invisible
    f Nonstacking    = "Non-stacking"
    f NonAffliction  = "Non-affliction"
    f NonMental      = "Non-mental"
    f x              = tshow x

displayCache :: Vector TextBuilder
displayCache = toBuilder <$> textCache

lowerCache :: Vector TextBuilder
lowerCache = toBuilder . toLower <$> textCache

lower :: Class -> TextBuilder
lower = (lowerCache !!) . fromEnum
