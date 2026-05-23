module Class.Labeled
  ( Labeled(..)
  , eq
  , match
  , mapFirst
  ) where

import ClassyPrelude

import           Game.Model.Internal (Destructible, Copy, Skill, Status, Trap)
import qualified Game.Model.Internal.Copy as Copy
import qualified Game.Model.Internal.Destructible as Destructible
import qualified Game.Model.Internal.Skill as Skill
import qualified Game.Model.Internal.Status as Status
import qualified Game.Model.Internal.Trap as Trap
import           Game.Model.Slot (Slot)

-- | Types with names and 'Model.Ninja.Ninja' sources.
-- This is important because two different 'Model.Ninja.Ninja's might have
-- 'Model.Skill.Skill's with the same name, so both name and origin must match
-- in order for a structure to count as theirs.
class Labeled a where
    -- | Label
    name :: a -> Text
    -- | 'Model.Ninja.Ninja' user
    user :: a -> Slot

-- Equality by both fields.
eq :: ∀ a. Labeled a => a -> a -> Bool
eq x y = name x == name y && user x == user y
{-# INLINE eq #-}

-- Matching by both fields.
match :: ∀ a. Labeled a => Text -> Slot -> a -> Bool
match name' user' x = name x == name' && user x == user'
{-# INLINE match #-}

mapFirst :: ∀ a. Labeled a => (a -> a) -> Text -> Slot -> [a] -> [a]
mapFirst _ _ _ [] = []
mapFirst f name' user' (x:xs)
  | match name' user' x = f x : xs
  | otherwise           = x : mapFirst f name' user' xs

instance Labeled Destructible where
    name = name . Destructible.skill
    user = Destructible.user

instance Labeled Copy where
    name = name . Copy.skill
    user = user . Copy.skill

instance Labeled Skill where
    name = Skill.name
    user = Skill.owner

instance Labeled Status where
    name = Status.name
    user = Status.user

instance Labeled Trap where
    name = Trap.name
    user = Trap.user
