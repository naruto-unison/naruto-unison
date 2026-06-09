-- 'N.cooldowns' processing.
module Game.Engine.Cooldown
  ( update
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

modifyCooldowns :: (HashMap Skill.Key Int -> HashMap Skill.Key Int)
                -> Ninja
                -> Ninja
modifyCooldowns f n = n { N.cooldowns = f $ N.cooldowns n }

-- | Adds to an element in 'N.cooldowns'.
alter :: Text -> Int -> Slot -> Ninja -> Ninja
alter skill cd owner = modifyCooldowns $ insertWith (+) key cd
  where
    key = Skill.Key skill owner

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Ninja -> Ninja
update skill@Skill{cooldown, dur} n =
    modifyCooldowns (insertMap (Skill.key skill) cd) n
  where
    minim
      | dur == Instant || dur == Passive = 0
      | otherwise = sync cooldown + 2
    cd = max minim $ sync cooldown + 2 + 2 * Effects.snare n

-- | Sets an element in 'N.coooldowns' to 0 by name.
reset :: Text -> Slot -> Ninja -> Ninja
reset skill owner = modifyCooldowns $ insertMap key 0
  where
    key = Skill.Key skill owner

-- | Sets all Instant 'N.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n@Ninja{character = Character{skills}} =
    modifyCooldowns (filterWithKey isInstantCooldown) n
  where
    isNonInstant Skill{dur = Instant} = False
    isNonInstant Skill{dur = Passive} = False
    isNonInstant _                    = True
    nonInstantSkills :: HashSet Skill.Key
    nonInstantSkills = setFromList [ Skill.key sk | skill <- toList skills,
                                                    sk    <- toList skill,
                                                    isNonInstant sk ]
    isInstantCooldown key _ = key ∉ nonInstantSkills
