module Class.Labeled
  ( Labeled(..)
  , eq
  , match
  , mapFirst
  ) where

import ClassyPrelude

import Game.Model.Slot (Slot)
import           Game.Model.Barrier (Barrier)
import qualified Game.Model.Barrier as Barrier
import           Game.Model.Defense (Defense)
import qualified Game.Model.Defense as Defense
import           Game.Model.Status (Status)
import qualified Game.Model.Status as Status
import           Game.Model.Trap (Trap)
import qualified Game.Model.Trap as Trap

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

instance Labeled Barrier where
    name = Barrier.name
    user = Barrier.user

instance Labeled Defense where
    name = Defense.name
    user = Defense.user

instance Labeled Status where
    name = Status.name
    user = Status.user

instance Labeled Trap where
    name = Trap.name
    user = Trap.user
