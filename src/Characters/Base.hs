{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_HADDOCK hide        #-}
module Characters.Base
  ( module Import
  , p, k
  , invuln
  , user, target, userHas, targetHas
  , userStacks, targetStacks, userDefense
  , channeling, immune
  , self, allies, enemies, everyone
  , baseVariant
  , bonusIf, numAffected, numDeadAllies
  ) where

import ClassyPrelude as Import hiding (swap)
import Class.Play as Import (Play(..))
import Model.Character as Import (Character(..), Category)
import Model.Chakra as Import (Chakra(..), Chakras)
import Model.Channel as Import (Channeling(..))
import Model.Copy as Import (Copying(..))
import Model.Class as Import (Class(..))
import Model.Effect as Import (Amount(..), Constructor(..), Effect(..))
import Model.Ninja as Import (Ninja(health, slot), addOwnStacks, addOwnDefense, alive, hasDefense, hasOwn, isChanneling, numActive, numHelpful)
import Model.Requirement as Import (Requirement(..))
import Model.Skill as Import (Target(..))
import Model.Status as Import (Bomb(..))
import Model.Trap as Import (Trigger(..))
import Action.Chakra as Import
import Action.Combat as Import
import Action.Channel as Import
import Action.Skill as Import
import Action.Status as Import
import Action.Trap as Import
import Engine.SkillTransform as Import

import qualified Data.List as List

import qualified Class.Play as P
import           Class.Play (PlayConstraint, MonadPlay)
import qualified Model.Chakra as Chakra
import qualified Model.Context as Context
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import qualified Model.Slot as Slot
import qualified Engine.Effects as Effects

p :: ∀ a b. a -> PlayConstraint b -> (a, Play b)
p tar f = (tar, Play f)

k :: [Chakra] -> Chakras
k = Chakra.collect

invuln :: Text -> Text -> [Class] -> Skill
invuln skillName userName classes = Skill.new
    { Skill.name      = skillName
    , Skill.desc      = userName ++ " becomes invulnerable for 1 turn."
    , Skill.classes   = classes
    , Skill.cooldown  = 4
    , Skill.effects   = [(Self, Play $ apply 1 [Invulnerable All])]
    }

self :: ∀ m a. MonadPlay m => m a -> m a
self = P.with Context.reflect

-- | Directly applies an effect to all other Ninjas, both living and dead,
-- ignoring invulnerabilities and traps.
everyone :: ∀ m. MonadPlay m => m () -> m ()
everyone f = P.user >>= flip P.withTargets f . (`List.delete` Slot.all)

-- | Directly applies an effect to all allies, both living and dead,
-- ignoring invulnerabilities and traps.
allies :: ∀ m. MonadPlay m => m () -> m ()
allies f = P.user >>= flip P.withTargets f . Slot.allies

-- | Directly applies an effect to all enemies, both living and dead,
-- ignoring invulnerabilities and traps.
enemies :: ∀ m. MonadPlay m => m () -> m ()
enemies f = P.user >>= flip P.withTargets f . Slot.enemies

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
    notUser   <- (/=) <$> P.user
    let filt t = notUser (Ninja.slot t) && match t
    length . filter filt . Game.ninjas <$> P.game

numAffected :: ∀ m. MonadPlay m => Text -> m Int
numAffected name = filterOthers =<< Ninja.has name <$> P.user

numDeadAllies :: ∀ m. MonadPlay m => m Int
numDeadAllies = filterOthers $ not . alive
