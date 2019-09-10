module Class.Labeled
  ( Labeled(..)
  , eq
  , match
  ) where

import ClassyPrelude

import Model.Slot (Slot)

-- | Types with names and 'Model.Ninja.Ninja' sources.
-- This is important because two different 'Model.Ninja.Ninja's might have
-- 'Model.Skill.Skill's with the same name, so both name and origin must match
-- in order for a structure to count as theirs.
class Labeled a where
    -- | Label
    name :: a -> Text
    -- | 'Model.Ninja.Ninja' user
    user :: a -> Slot

-- Equality.
eq :: ∀ a. Labeled a => a -> a -> Bool
eq x y = name x == name y && user x == user y

-- Matching.
match :: ∀ a. Labeled a => Text -> Slot -> a -> Bool
match n usr a = name a == n && user a == usr
