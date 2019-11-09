-- 'Ninja.cooldowns' processing.
module Game.Engine.Cooldown
  ( spendCharge, update
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import qualified Game.Engine.Effects as Effects
import           Game.Model.Duration (sync)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)

-- | Adds to an element in 'Ninja.cooldowns'.
alter :: Text -> Int -> Slot -> Ninja -> Ninja
alter skill cd owner n =
    n { Ninja.cooldowns = insertWith (+) key cd $ Ninja.cooldowns n }
  where
    key = Skill.Key skill owner

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Ninja -> Ninja
update skill n =
    n { Ninja.cooldowns = insertMap (Skill.key skill) cd $ Ninja.cooldowns n }
  where
    cd = max 0 $ sync (Skill.cooldown skill) + 2 + 2 * Effects.snare n

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
spendCharge :: Skill -> Ninja -> Ninja
spendCharge skill n =
    n { Ninja.charges = insertWith (+) (Skill.key skill) 1 $ Ninja.charges n }

-- | Sets an element in 'Ninja.coooldowns' to 0 by name.
reset :: Text -> Slot -> Ninja -> Ninja
reset skill owner n =
    n { Ninja.cooldowns = insertMap key 0 $ Ninja.cooldowns n }
  where
    key = Skill.Key skill owner

-- | Sets all 'Ninja.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n = n { Ninja.cooldowns = mempty }
