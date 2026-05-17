-- 'N.cooldowns' processing.
module Game.Engine.Cooldown
  ( spendCharge, update
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import qualified Game.Engine.Effects as Effects
import           Game.Model.Duration (sync)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)

-- | Adds to an element in 'N.cooldowns'.
alter :: Text -> Int -> Slot -> Ninja -> Ninja
alter skill cd owner n =
    n { N.cooldowns = insertWith (+) key cd $ N.cooldowns n }
  where
    key = Skill.Key skill owner

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Ninja -> Ninja
update skill n =
    n { N.cooldowns = insertMap (Skill.key skill) cd $ N.cooldowns n }
  where
    cd = max 0 $ sync (Skill.cooldown skill) + 2 + 2 * Effects.snare n

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
spendCharge :: Skill -> Ninja -> Ninja
spendCharge skill n =
    n { N.charges = insertWith (+) (Skill.key skill) 1 $ N.charges n }

-- | Sets an element in 'N.coooldowns' to 0 by name.
reset :: Text -> Slot -> Ninja -> Ninja
reset skill owner n =
    n { N.cooldowns = insertMap key 0 $ N.cooldowns n }
  where
    key = Skill.Key skill owner

-- | Sets all 'N.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n = n { N.cooldowns = mempty }
