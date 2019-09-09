{-# OPTIONS_HADDOCK hide #-}
module Characters.Base
  ( module Import
  , invuln
  , user, target, userHas, targetHas
  , userStacks, targetStacks, userDefense
  , channeling, immune
  , self, allies, enemies, everyone
  , baseVariant
  , bonusIf, numAffected, numDeadAllies
  ) where

import ClassyPrelude as Import hiding (swap)

import Model.Character as Import (Character(..), Category)
import Model.Chakra as Import (Chakra(..), Chakras)
import Model.Channel as Import (Channeling(..))
import Model.Copy as Import (Copying(..))
import Model.Class as Import (Class(..))
import Model.Effect as Import (Amount(..), Constructor(..), Effect(..))
import Model.Ninja as Import (Ninja(health, slot), alive, hasDefense, hasOwn, isChanneling, numActive, numHarmfulStacks, numHelpful)
import Model.Requirement as Import (Requirement(..))
import Model.Runnable as Import (Runnable(To))
import Model.Skill as Import (Target(..))
import Model.Status as Import (Bomb(..))
import Model.Trap as Import (Trigger(..))
import Action.Chakra as Import
import Action.Combat as Import
import Action.Channel as Import
import Action.Skill as Import
import Action.Status as Import
import Action.Trap as Import
import Engine.Skills as Import
import Engine.Ninjas as Import (addOwnStacks, addOwnDefense)

import Data.Enum.Set.Class (EnumSet)

import qualified Class.Play as P
import           Class.Play (MonadPlay)
import qualified Model.Context as Context
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import qualified Model.Slot as Slot
import qualified Engine.Effects as Effects

invuln :: Text -> Text -> EnumSet Class -> Skill
invuln skillName userName classes = Skill.new
    { Skill.name      = skillName
    , Skill.desc      = userName ++ " becomes invulnerable for 1 turn."
    , Skill.classes   = classes
    , Skill.cooldown  = 4
    , Skill.effects   = [To Self $ apply 1 [Invulnerable All]]
    }

self :: ∀ m a. MonadPlay m => m a -> m a
self = P.with Context.reflect

-- | Directly applies an effect to all other Ninjas, both living and dead,
-- ignoring invulnerabilities and traps.
everyone :: ∀ m. MonadPlay m => m () -> m ()
everyone f = flip P.withTargets f . (`delete` Slot.all) =<< P.user

-- | Directly applies an effect to all allies, both living and dead,
-- ignoring invulnerabilities and traps.
allies :: ∀ m. MonadPlay m => m () -> m ()
allies f = flip P.withTargets f . Slot.allies =<< P.user

-- | Directly applies an effect to all enemies, both living and dead,
-- ignoring invulnerabilities and traps.
enemies :: ∀ m. MonadPlay m => m () -> m ()
enemies f = flip P.withTargets f . Slot.enemies =<< P.user

baseVariant :: Text
baseVariant = ""

bonusIf :: ∀ m. MonadPlay m => Int -> m Bool -> m Int
bonusIf amount condition = do
    succeed <- condition
    return if succeed then amount else 0

user :: ∀ m a. MonadPlay m => (Ninja -> a) -> m a
user f = f <$> P.nUser

target :: ∀ m a. MonadPlay m => (Ninja -> a) -> m a
target f = f <$> P.nTarget

userHas :: ∀ m. MonadPlay m => Text -> m Bool
userHas name = Ninja.hasOwn name <$> P.nUser

targetHas :: ∀ m. MonadPlay m => Text -> m Bool
targetHas name = Ninja.has name <$> P.user <*> P.nTarget

userStacks :: ∀ m. MonadPlay m => Text -> m Int
userStacks name = Ninja.numStacks name <$> P.user <*> P.nUser

targetStacks :: ∀ m. MonadPlay m => Text -> m Int
targetStacks name = Ninja.numStacks name <$> P.user <*> P.nTarget

userDefense :: ∀ m. MonadPlay m => Text -> m Int
userDefense name = defense <$> P.nUser
  where
    defense n = Ninja.defenseAmount name (slot n) n

channeling :: ∀ m. MonadPlay m => Text -> m Bool
channeling name = Ninja.isChanneling name <$> P.nUser

immune :: Ninja -> Bool
immune = not . null . Effects.immune

filterOthers :: ∀ m. MonadPlay m => (Ninja -> Bool) -> m Int
filterOthers match = do
    notUser <- (/=) <$> P.user
    length . filter (\t -> notUser (Ninja.slot t) && match t) <$> P.ninjas

numAffected :: ∀ m. MonadPlay m => Text -> m Int
numAffected name = filterOthers =<< Ninja.has name <$> P.user

numDeadAllies :: ∀ m. MonadPlay m => m Int
numDeadAllies = filterOthers $ not . alive
