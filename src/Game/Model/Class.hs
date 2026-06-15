module Game.Model.Class
  ( Class(..)
  , name, lower
  , visible
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Enum.Memo as Enum
import           Data.Enum.Set (AsEnumSet(..))
import           Text.Blaze (ToMarkup(..))

import Class.Display (Display(..))

-- | Qualifiers of 'Model.Skill.Skill's and 'Model.Status.Status'es.
data Class
    -- Kind
    = Chakra
    | Mental
    | Physical
    | Summon
    -- Distance
    | Melee
    | Ranged
    -- Effects
    | Bypassing
    | Invisible
    | Soulbound
    | Controlled
    -- Tags
    | Bane
    | Necromancy
    -- Prevention
    | Uncounterable
    | Unreflectable
    | Unremovable
    -- Hidden
    | Hidden
    | Resource -- ^ Display stacks separately
    | Nonstacking
    | Extending
    | Atemporal -- ^ Unaffected by Izanagi
    -- Fake (don't put these in Skill.classes manually)
    | All
    | Affliction
    | NonAffliction
    | NonBane
    | NonMental
    | NonRanged
    | Continues
    | Bloodline
    | Genjutsu
    | Ninjutsu
    | Taijutsu
    | Random
    deriving (Bounded, Enum, Eq, Ord, Show)

instance AsEnumSet Class where
    type EnumSetRep Class = Word64

instance ToJSON Class where
    toJSON = A.String . nameMemo

instance ToMarkup Class where
    toMarkup = toMarkup . nameMemo

instance Hashable Class where
    hashWithSalt salt = hashWithSalt salt . fromEnum

instance Display Class where
    display = Enum.memoize $ display . name
    {-# NOINLINE display #-}

visible :: Class -> Bool
visible = (< Hidden)

name :: Class -> Text
name Nonstacking    = "Non-stacking"
name NonAffliction  = "Non-affliction"
name NonBane        = "Non-bane"
name NonMental      = "Non-mental"
name NonRanged      = "Non-ranged"
name x              = tshow x

nameMemo :: Class -> Text
nameMemo = Enum.memoize name
{-# NOINLINE nameMemo #-}

lower :: Class -> TextBuilder
lower = Enum.memoize $ display . toLower . name
{-# NOINLINE lower #-}
