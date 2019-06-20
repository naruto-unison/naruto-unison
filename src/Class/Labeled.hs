module Class.Labeled
  ( Labeled(..)
  , eq
  , match
  ) where

import ClassyPrelude.Yesod

import Model.Slot (Slot)

-- Class for types with names and 'Ninja' sources.
-- This is important because two different 'Ninja's might have 'Skill's
-- with the same name, so both name and origin must match in order for a
-- structure to count as theirs.
class Labeled a where
    -- | Label
    name  :: a -> Text
    -- | 'Ninja' user
    user :: a -> Slot

-- Equality.
eq :: ∀ a. Labeled a => a -> a -> Bool
eq x y = name x == name y && user x == user y

-- Matching.
match :: ∀ a. Labeled a => Text -> Slot -> a -> Bool
match n src a = name a == n && user a == src
