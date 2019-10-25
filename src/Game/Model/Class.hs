module Game.Model.Class
  ( Class(..)
  , name, lower
  , visible, visiblesList, visiblesMap
  ) where

import ClassyPrelude

import           Data.Aeson (ToJSON(..), Value)
import qualified Data.Enum.Memo as Enum
import           Data.Enum.Set.Class (AsEnumSet(..))
import           Text.Blaze (ToMarkup(..))

import Class.Display (Display(..))
import Util (mapFromKeyed)

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
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance AsEnumSet Class where
    type EnumSetRep Class = Word64

instance ToJSON Class where
    toJSON = toJSON . name
    {-# INLINE toJSON #-}

instance ToMarkup Class where
    toMarkup = toMarkup . name

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

lower :: Class -> TextBuilder
lower = Enum.memoize $ display . toLower . name
{-# NOINLINE lower #-}

visiblesList :: Value
visiblesList = toJSON $ filter visible [minBound..maxBound]
{-# NOINLINE visiblesList #-}

visiblesMap :: Value
visiblesMap = (toJSON :: Map Text Bool -> Value) .
              mapFromKeyed (name, const True) $
              filter visible [minBound..maxBound]
{-# NOINLINE visiblesMap #-}
