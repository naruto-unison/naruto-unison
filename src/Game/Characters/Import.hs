{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Game.Characters.Import
  ( module Import
  , SkillEffect
  , invuln
  , user, target
  , channeling, inGroup
  , targeting
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
import Game.Engine.Skills as Import
import Game.Model.Chakras as Import (Chakra(..), Chakras, chakraDesc)
import Game.Model.Channel as Import (Channeling(..))
import Game.Model.Character as Import (Character(..), Category(..))
import Game.Model.Class as Import (Class(..))
import Game.Model.Duration as Import (Duration(..))
import Game.Model.Effect as Import (Amount(..), Constructor(..), Effect(..))
import Game.Model.Group as Import (Group(..))
import Game.Model.Ninja as Import (Ninja(barrier, defense, health, slot, statuses, traps), alive, barrierAmount, defenseAmount, has, has', is, lastChakraSpent, numHelpful, numSkills, numStacks)
import Game.Model.Requirement as Import (Requirement(..))
import Game.Model.Runnable as Import (IntRunConstraint, RunConstraint, Runnable(To))
import Game.Model.Skill as Import (Skill, Target(..))
import Game.Model.Slot as Import (Slot, toInt, teamSize)
import Game.Model.Status as Import (Bomb(..))
import Game.Model.Trap as Import (Direction(..))
import Game.Model.Trigger as Import (Trigger(..))

import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Action as Action
import qualified Game.Model.Character as Character
import qualified Game.Model.Context as Context
import qualified Game.Model.Ninja as N
import qualified Game.Model.Skill as Skill
import           Util ((∈))
import Class.Random (MonadRandom)
import Data.Kind (Type)

type SkillEffect = RunConstraint ()

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

targeting :: ∀ m. (MonadPlay m, MonadRandom m) => Target -> m () -> m ()
targeting t f = do
    targets <- Action.chooseTargets t
    P.withTargets targets f

-- | Returns the bonus if the monadic condition succeeds, otherwise returns 0.
bonusIf :: ∀ m a. (MonadPlay m, Num a) => a -> m Bool -> m a
bonusIf amount condition = getBonus <$> condition
  where
    getBonus True  = amount
    getBonus False = 0

-- | True if user 'N.isChanneling'.
channeling :: ∀ m. MonadPlay m => Text -> m Bool
channeling name = N.isChanneling name <$> P.nUser

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

class NinjaGetter (m :: Type -> Type) a where
    type Getter (m :: Type -> Type) a
    target :: a -> Getter m a
    user   :: a -> Getter m a

instance MonadPlay m => NinjaGetter m () where
    type Getter m () = m Ninja
    target () = P.nTarget
    user   () = P.nUser

instance MonadPlay m => NinjaGetter m (Ninja -> a) where
    type Getter m (Ninja -> a) = m a
    target f = f <$> P.nTarget
    user   f = f <$> P.nUser

instance MonadPlay m => NinjaGetter m (Text -> Slot -> Ninja -> a) where
    type Getter m (Text -> Slot -> Ninja -> a) = Text -> m a
    target f name = f name <$> userSlot <*> P.nTarget
    user   f name = f name <$> userSlot <*> P.nUser

instance MonadPlay m => NinjaGetter m ((Ninja -> [b]) -> Text -> Slot -> Ninja -> a) where
    type Getter m ((Ninja -> [b]) -> Text -> Slot -> Ninja -> a) = (Ninja -> [b]) -> Text -> m a
    target f getter name = f getter name <$> userSlot <*> P.nTarget
    user   f getter name = f getter name <$> userSlot <*> P.nUser
