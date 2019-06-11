module Class.Labeled
  ( Labeled(..)
  , eq
  , match
  ) where

import ClassyPrelude.Yesod

import Model.Slot (Slot)

-- Typeclass for data structures that have a name
-- and originate from a specific 'Ninja'.
-- This is important because two different 'Ninja's might have 'Skill's with
-- the same name, so both name and origin must match in order for a structure
-- to count as theirs.
class Labeled a where
    -- Label
    name  :: a -> Text
    -- 'Ninja' user
    source :: a -> Slot

-- Equality.
eq :: ∀ a. Labeled a => a -> a -> Bool
eq x y = name x == name y && source x == source y

-- Matching.
match :: ∀ a. Labeled a => Text -> Slot -> a -> Bool
match n src a = name a == n && source a == src
