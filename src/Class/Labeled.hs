module Class.Labeled
  ( Labeled(..)
  , eq
  , group
  , match, named
  , mapFirst
  ) where

import ClassyPrelude hiding (group)

import Game.Model.Slot (Slot)

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
eq :: Labeled a => a -> a -> Bool
eq x y = name x == name y && user x == user y
{-# INLINABLE eq #-}

group :: ∀ o. (IsSequence o, Labeled (Element o))
      => o -> [NonNull [] (Element o)]
group xs = groupBy eq . toList $ sortBy cmp xs
  where
    x `cmp` y = (name x `compare` name y) <> (user x `compare` user y)
{-# INLINABLE group #-}

-- Matching by both fields.
match :: ∀ a. Labeled a => Text -> Slot -> a -> Bool
match name' user' x = name x == name' && user x == user'
{-# INLINABLE match #-}

-- Matching by name.
named :: ∀ a. Labeled a => Text -> a -> Bool
named name' x = name x == name'
{-# INLINABLE named #-}

mapFirst :: ∀ a. Labeled a => (a -> a) -> Text -> Slot -> [a] -> [a]
mapFirst _ _ _ [] = []
mapFirst f name' user' (x:xs)
  | match name' user' x = f x : xs
  | otherwise           = x : mapFirst f name' user' xs
