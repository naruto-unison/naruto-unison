{-# OPTIONS_HADDOCK prune #-}

module Game.Characters.Import
  ( module Import
  , invuln
  , user, target, userHas, targetHas, userHas', targetHas'
  , userStacks, targetStacks, userDefense
  , channeling, invulnerable, inGroup
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
import Game.Model.Chakras as Import (Chakra(..), Chakras, chakraDesc)
import Game.Model.Channel as Import (Channeling(..))
import Game.Model.Character as Import (Character(..), Category(..))
import Game.Model.Class as Import (Class(..))
import Game.Model.Duration as Import (Duration(..))
import Game.Model.Effect as Import (Amount(..), Constructor(..), Effect(..))
import Game.Model.Group as Import (Group(..))
import Game.Model.Ninja as Import (Ninja(barrier, defense, health, slot, statuses, traps), alive, hasBarrier, hasDefense, hasOwnDefense, hasOwn, is, isChanneling, numActive, numHelpful, numSkills)
import Game.Model.Requirement as Import (Requirement(..))
import Game.Model.Runnable as Import (RunConstraint, Runnable(To))
import Game.Model.Skill as Import (Target(..))
import Game.Model.Slot as Import (toInt, teamSize)
import Game.Model.Status as Import (Bomb(..))
import Game.Model.Trap as Import (Direction(..))
import Game.Model.Trigger as Import (Trigger(..))

import Data.Enum.Set (EnumSet)

import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Character as Character
import qualified Game.Model.Context as Context
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Util ((∈))

userSlot :: ∀ m. MonadPlay m => m Slot
userSlot = Context.user <$> P.context

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
    slot <- userSlot
    P.withTargets (targeter slot) f

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
everyone = P.withTargets Slot.all

-- | Returns the bonus if the monadic condition succeeds, otherwise returns 0.
bonusIf :: ∀ m a. (MonadPlay m, Num a) => a -> m Bool -> m a
bonusIf amount condition = getBonus <$> condition
  where
    getBonus True  = amount
    getBonus False = 0

-- | Applies a pure function to 'P.nUser'.
user :: ∀ m a. MonadPlay m => (Ninja -> a) -> m a
user f = f <$> P.nUser

-- | Applies a pure function to 'P.nTarget'.
target :: ∀ m a. MonadPlay m => (Ninja -> a) -> m a
target f = f <$> P.nTarget

has' :: ∀ m a. (MonadPlay m, Labeled a)
     => m Ninja -> (Ninja -> [a]) -> Text -> m Bool
has' subjectGetter fieldGetter name = getHas <$> userSlot <*> subjectGetter
  where
    getHas :: Slot -> Ninja -> Bool
    getHas from to = any (Labeled.match name from) $ fieldGetter to

-- | Generic 'userHas'.
userHas' :: ∀ m a. (MonadPlay m, Labeled a)
         => (Ninja -> [a]) -> Text -> m Bool
userHas' = has' P.nUser

-- | Generic 'targetHas'.
targetHas' :: ∀ m a. (MonadPlay m, Labeled a)
           => (Ninja -> [a]) -> Text -> m Bool
targetHas' = has' P.nTarget

-- | True if user 'N.hasOwn'.
userHas :: ∀ m. MonadPlay m => Text -> m Bool
userHas = userHas' N.statuses

-- | True if target 'N.has'.
targetHas :: ∀ m. MonadPlay m => Text -> m Bool
targetHas = targetHas' N.statuses

-- | 'N.numStacks' of the user, from the user.
userStacks :: ∀ m. MonadPlay m => Text -> m Int
userStacks name = N.numStacks name <$> userSlot <*> P.nUser

-- | 'N.numStacks' of the target, from the user.
targetStacks :: ∀ m. MonadPlay m => Text -> m Int
targetStacks name = N.numStacks name <$> userSlot <*> P.nTarget

-- | Returns 'N.defense' of the user's own defense.
userDefense :: ∀ m. MonadPlay m => Text -> m Int
userDefense name = getUserDefense <$> P.nUser
  where
    getUserDefense :: Ninja -> Int
    getUserDefense n = N.defenseAmount name (slot n) n

-- | True if user 'N.isChanneling'.
channeling :: ∀ m. MonadPlay m => Text -> m Bool
channeling name = N.isChanneling name <$> P.nUser

-- | True if the subject is 'Invulnerable' to any 'Model.Game.Class.Class'.
invulnerable :: Ninja -> Bool
invulnerable n = not . null $ Effects.invulnerable n

-- | True if 'N.character' has a 'Group'.
inGroup :: Group -> Ninja -> Bool
inGroup x n = x ∈ Character.groups (N.character n)

-- | Number of users affected by a 'Model.Game.Status.Status'.
numAffected :: ∀ m. MonadPlay m => Text -> m Int
numAffected name = getNumAffected <$> userSlot <*> P.ninjas
  where
    getNumAffected :: Slot -> [Ninja] -> Int
    getNumAffected slot ninjas = length $ filter (N.has name slot) ninjas

-- | Number of user's allies who are dead.
numDeadAllies :: ∀ m. MonadPlay m => m Int
numDeadAllies = do
    slot <- userSlot
    length . filter (not.alive) <$> P.allies slot
