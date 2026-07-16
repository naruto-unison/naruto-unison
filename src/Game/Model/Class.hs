module Game.Model.Class
  ( Class(..)
  , name, lower
  , inherited
  , visible
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Enum.Memo as Enum
import           Data.Enum.Set (AsEnumSet(..), EnumSet)
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
    | Reanimation
    -- Prevention
    | Uncounterable
    | Unreflectable
    | Unremovable
    -- Hidden
    | Hidden
    | Resource -- ^ Display stacks separately
    | Nonstacking
    | Atemporal -- ^ Unaffected by Izanagi
    -- Fake (don't put these in Skill.classes manually)
    | All
    | Affliction
    | NonAffliction
    | NonBane
    | NonMental
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

inherited :: EnumSet Class
inherited = setFromList
    [ Invisible
    , Soulbound
    , Controlled
    , Bane
    , Necromancy
    , Reanimation
    , Unremovable
    , Hidden
    , Resource
    , Nonstacking
    , Atemporal
    , All
    , NonBane
    , Continues
    ]

name :: Class -> Text
name Nonstacking    = "Non-stacking"
name NonAffliction  = "Non-affliction"
name NonBane        = "Non-bane"
name NonMental      = "Non-mental"
name x              = tshow x

nameMemo :: Class -> Text
nameMemo = Enum.memoize name
{-# NOINLINE nameMemo #-}

lower :: Class -> TextBuilder
lower = Enum.memoize $ display . toLower . name
{-# NOINLINE lower #-}
