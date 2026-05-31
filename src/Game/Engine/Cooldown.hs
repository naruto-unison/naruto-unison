-- 'N.cooldowns' processing.
module Game.Engine.Cooldown
  ( spendCharge, update
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import qualified Game.Engine.Effects as Effects
import           Game.Model.Channel (Channeling(..))
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character
import           Game.Model.Duration (sync)
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((∉))

-- | Adds to an element in 'N.cooldowns'.
alter :: Text -> Int -> Slot -> Ninja -> Ninja
alter skill cd owner n =
    n { N.cooldowns = insertWith (+) key cd $ N.cooldowns n }
  where
    key = Skill.Key skill owner

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Ninja -> Ninja
update skill@Skill{cooldown, dur} n =
    n { N.cooldowns = insertMap (Skill.key skill) cd $ N.cooldowns n }
  where
    minim
      | dur == Instant || dur == Passive = 0
      | otherwise = sync cooldown + 2
    cd = max minim $ sync cooldown + 2 + 2 * Effects.snare n

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

-- | Sets all Instant 'N.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n@Ninja{character = Character{skills}} =
    n { N.cooldowns = filterWithKey isInstantCooldown $ N.cooldowns n }
  where
    isNonInstant Skill{dur = Instant} = False
    isNonInstant Skill{dur = Passive} = False
    isNonInstant _                    = True
    nonInstantSkills :: HashSet Skill.Key
    nonInstantSkills = setFromList $ Skill.key
                       <$> (filter isNonInstant . toList =<< toList skills)
    isInstantCooldown key _ = key ∉ nonInstantSkills
