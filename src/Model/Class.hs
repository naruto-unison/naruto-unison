module Model.Class (Class(..), lower) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import qualified Data.Enum.Memo as Enum
import           Data.Enum.Set.Class (AsEnumSet(..))

import Class.Display (Display(..))

-- | Qualifiers of 'Model.Skill.Skill's and 'Model.Status.Status'es.
data Class
    = Bypassing
    | Invisible
    | InvisibleTraps
    | Soulbound
    -- Tags
    | Bane
    | Necromancy
    -- Distance
    | Melee
    | Ranged
    -- Type
    | Chakra
    | Mental
    | Physical
    | Summon
    -- Limits
    | Nonstacking
    | Extending
    -- Prevention
    | Uncounterable
    | Unreflectable
    | Unremovable
    -- Fake (Hidden)
    | All
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

instance AsEnumSet Class where
    type EnumSetRep Class = Word64

instance ToJSON Class where
    toJSON = Enum.memoize $ toJSON . name

instance Hashable Class where
    hashWithSalt salt = hashWithSalt salt . fromEnum

instance Display Class where
    display = Enum.memoize $ display . name

name :: Class -> Text
name InvisibleTraps = "Invisible"
name Nonstacking    = "Non-stacking"
name NonAffliction  = "Non-affliction"
name NonMental      = "Non-mental"
name x              = tshow x

lower :: Class -> TextBuilder
lower = Enum.memoize $ display . toLower . name
