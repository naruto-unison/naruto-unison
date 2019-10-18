{-# OPTIONS_HADDOCK prune #-}

module Game.Characters.Base
  ( module Import
  , invuln
  , user, target, userHas, targetHas
  , userStacks, targetStacks, userDefense
  , channeling, invulnerable
  , self, allies, enemies, everyone
  , bonusIf, numAffected, numDeadAllies
  ) where

import ClassyPrelude as Import hiding (swap)

import Class.Play as Import (withTarget)
import Game.Action.Chakra as Import
import Game.Action.Channel as Import
import Game.Action.Combat as Import
import Game.Action.Skill as Import
import Game.Action.Status as Import
import Game.Action.Trap as Import
import Game.Engine.Effects as Import (stunned)
import Game.Engine.Ninjas as Import (addOwnStacks, addOwnDefense)
import Game.Engine.Skills as Import
import Game.Model.Chakra as Import (Chakra(..), Chakras)
import Game.Model.Channel as Import (Channeling(..))
import Game.Model.Character as Import (Character(..), Category)
import Game.Model.Class as Import (Class(..))
import Game.Model.Effect as Import (Amount(..), Constructor(..), Effect(..))
import Game.Model.Ninja as Import (Ninja(health, slot), alive, hasDefense, hasOwn, isChanneling, numActive, numAnyStacks, numHelpful)
import Game.Model.Requirement as Import (Requirement(..))
import Game.Model.Runnable as Import (RunConstraint, Runnable(To))
import Game.Model.Skill as Import (Target(..))
import Game.Model.Status as Import (Bomb(..))
import Game.Model.Trap as Import (Direction(..))
import Game.Model.Trigger as Import (Trigger(..))

import Data.Enum.Set.Class (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Context as Context
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot

-- | Baseline fourth skill that makes the character invulnerable for one turn.
invuln :: Text -- ^ 'Skill.name'.
       -> Text -- ^ Character name/nickname; first phrase in 'Skill.desc'.
       -> EnumSet Class -- ^ 'Skill.classes'.
       -> Skill
invuln skillName userName classes = Skill.new
    { Skill.name      = skillName
    , Skill.desc      = userName ++ " becomes invulnerable for 1 turn."
    , Skill.classes   = classes
    , Skill.cooldown  = 4
    , Skill.effects   = [To Self $ apply 1 [Invulnerable All]]
    }

-- | Applies an effect to the user, rather than the target.
self :: ∀ m a. MonadPlay m => m a -> m a
self = P.with Context.reflect

targetWithUser :: ∀ m. MonadPlay m => (Slot -> [Slot]) -> m () -> m ()
targetWithUser targeter f = do
    targets <- targeter <$> P.user
    P.withTargets targets f

-- | Directly applies an effect to all allies, both living and dead,
-- ignoring invulnerabilities and traps.
allies :: ∀ m. MonadPlay m => m () -> m ()
allies = targetWithUser Slot.allies

-- | Directly applies an effect to all enemies, both living and dead,
-- ignoring invulnerabilities and traps.
enemies :: ∀ m. MonadPlay m => m () -> m ()
enemies = targetWithUser Slot.enemies

-- | Directly applies an effect to all other Ninjas, both living and dead,
-- ignoring invulnerabilities and traps.
everyone :: ∀ m. MonadPlay m => m () -> m ()
everyone = targetWithUser (`delete` Slot.all)

-- | Returns the bonus if the monadic condition succeeds, otherwise returns 0.
bonusIf :: ∀ m. MonadPlay m => Int -> m Bool -> m Int
bonusIf amount condition = do
    succeed <- condition
    return if succeed then amount else 0

-- | Applies a pure function to 'P.nUser'.
user :: ∀ m a. MonadPlay m => (Ninja -> a) -> m a
user f = f <$> P.nUser

-- | Applies a pure function to 'P.nTarget'.
target :: ∀ m a. MonadPlay m => (Ninja -> a) -> m a
target f = f <$> P.nTarget

-- | True if user 'Ninja.hasOwn'.
userHas :: ∀ m. MonadPlay m => Text -> m Bool
userHas name = Ninja.hasOwn name <$> P.nUser

-- | True if target 'Ninja.has'.
targetHas :: ∀ m. MonadPlay m => Text -> m Bool
targetHas name = Ninja.has name <$> P.user <*> P.nTarget

-- | 'Ninja.numStacks' of the user, from the user.
userStacks :: ∀ m. MonadPlay m => Text -> m Int
userStacks name = Ninja.numStacks name <$> P.user <*> P.nUser

-- | 'Ninja.numStacks' of the target, from the user.
targetStacks :: ∀ m. MonadPlay m => Text -> m Int
targetStacks name = Ninja.numStacks name <$> P.user <*> P.nTarget

-- | Returns 'Ninja.defense' of the user's own defense.
userDefense :: ∀ m. MonadPlay m => Text -> m Int
userDefense name = defense <$> P.nUser
  where
    defense n = Ninja.defenseAmount name (slot n) n

-- | True if user 'Ninja.isChanneling'.
channeling :: ∀ m. MonadPlay m => Text -> m Bool
channeling name = Ninja.isChanneling name <$> P.nUser

-- | True if the subject is 'Invulnerable' to any 'Model.Game.Class.Class'.
invulnerable :: Ninja -> Bool
invulnerable n = not . null $ Effects.invulnerable n

filterOthers :: ∀ m. MonadPlay m => (Ninja -> Bool) -> m Int
filterOthers match = length . filter match <$> (P.allies =<< P.user)

-- | Number of user's allies who are affected by a 'Model.Game.Status.Status'.
numAffected :: ∀ m. MonadPlay m => Text -> m Int
numAffected name = filterOthers =<< Ninja.has name <$> P.user

-- | Number of user's allies who are dead.
numDeadAllies :: ∀ m. MonadPlay m => m Int
numDeadAllies = filterOthers $ not . alive
