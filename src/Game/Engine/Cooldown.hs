-- 'N.cooldowns' processing.
module Game.Engine.Cooldown
  ( update
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import qualified Game.Engine.Effects as Effects
import           Game.Model.Channel (Channeling(..))
import qualified Game.Model.Character
import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Util ((∉))

idKey :: ID -> Skill.Key
idKey ID{name, owner} = Skill.Key name owner

modifyCooldowns :: (HashMap Skill.Key Int -> HashMap Skill.Key Int)
                -> Ninja
                -> Ninja
modifyCooldowns f n = n { N.cooldowns = f n.cooldowns }

-- | Adds to an element in 'N.cooldowns'.
alter :: Int -> ID -> Ninja -> Ninja
alter cd skillID = modifyCooldowns $ insertWith (+) (idKey skillID) cd

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Ninja -> Ninja
update skill@Skill{cooldown} n
  | cd == 0   = n
  | otherwise = modifyCooldowns (insertMap (Skill.key skill) $ 1 + cd) n
  where
    cd = max 0 $ cooldown + Effects.snare n

-- | Sets an element in 'N.coooldowns' to 0 by name.
reset :: ID-> Ninja -> Ninja
reset skillID = modifyCooldowns $ insertMap (idKey skillID) 0

-- | Sets all Instant 'N.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n = modifyCooldowns (filterWithKey isInstantCooldown) n
  where
    isNonInstant Skill{dur = Instant} = False
    isNonInstant Skill{dur = Passive} = False
    isNonInstant _                    = True
    nonInstantSkills :: HashSet Skill.Key
    nonInstantSkills = setFromList
        [ Skill.key sk | skill <- toList n.character.skills,
                         sk    <- toList skill,
                         isNonInstant sk ]
    isInstantCooldown key _ = key ∉ nonInstantSkills
