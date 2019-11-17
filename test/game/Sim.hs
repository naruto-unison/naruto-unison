module Sim
  ( targetSlot
  , describeCategory
  , act
  , turns
  , as, at, use
  , targetIsExposed
  , statusDur
  , hasSkill
  , withClass, withClasses
  , get
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)
import Test.Hspec hiding (context)

import           Class.Hook (MonadHook)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Action as Action
import           Game.Action.Status
import qualified Game.Characters as Characters
import qualified Game.Engine as Engine
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import           Game.Model.Act (Act(Act))
import qualified Game.Model.Act
import           Game.Model.Character (Category(..), Character)
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..), Turns, sync, unsync)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Runnable (Runnable(..), RunConstraint)
import           Game.Model.Skill (Target(..))
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import qualified Game.Model.Status as Status
import           Util ((!!))

describeCategory :: HasCallStack
                 => Category -> Text -> (SpecWith Character) -> SpecWith ()
describeCategory category name specs =
    describe (unpack name) case find matchChar Characters.list of
        Nothing   -> it "exists in the database" False
        Just char -> before (return char) $ parallel specs
  where
    matchChar x = Character.name x == name && Character.category x == category

use :: ∀ m. (HasCallStack, MonadHook m, MonadPlay m, MonadRandom m)
    => Text -> m ()
use skillName =
    actWith . fromMaybe notFoundError . find ((== skillName) . Skill.name) .
    Ninjas.skills . unsafeHead =<< P.ninjas
  where
    notFoundError = error $ "invalid skill: " ++ unpack skillName

at :: ∀ m a. MonadPlay m => Target -> m a -> m a
at target = P.withTarget $ targetSlot target

targetSlot :: Target -> Slot
targetSlot Self = Slot.all !! 0
targetSlot Ally = Slot.all !! 1
targetSlot Allies = Slot.all !! 1
targetSlot RAlly = Slot.all !! 1
targetSlot RXAlly = Slot.all !! 2
targetSlot XAlly = Slot.all !! 2
targetSlot XAllies = Slot.all !! 2
targetSlot Enemy = Slot.all !! 3
targetSlot Enemies = Slot.all !! 3
targetSlot REnemy = Slot.all !! 3
targetSlot XEnemies = Slot.all !! 4
targetSlot Everyone = Slot.all !! 0

get :: ∀ m. MonadGame m => Target -> m Ninja
get target = P.ninja $ targetSlot target

act :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => m ()
act = actWith =<< Skills.change <$> P.nUser <*> P.skill

actWith :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m) => Skill -> m ()
actWith actSkill = do
    user   <- P.user
    target <- P.target
    player <- P.player
    unless (Parity.allied user player) $ Engine.processTurn $ return ()
    Engine.processTurn $ Action.act True Act { user, target, skill }
  where
    skill = Right $ actSkill { Skill.cost     = 0
                             , Skill.cooldown = 0
                             }

turns :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Turns -> m ()
turns (Duration -> i) = do
    player <- P.player
    replicateM_ (sync i + 1 - fromEnum player) . Engine.processTurn $ return ()

as :: ∀ m. (MonadPlay m, MonadHook m, MonadRandom m)
   => Target -> RunConstraint () -> m ()
as t f = P.with ctx $ actWith =<< P.skill
  where
    user        = targetSlot t
    ctx context = Context {user, target, skill, new = True, continues = False}
      where
        skill = Skill.new { Skill.effects = effects, Skill.classes }
        ctxTarget = Context.target context
        target
          | ctxTarget == user = Context.user context
          | otherwise         = ctxTarget
        effects
          | Context.user context == user && ctxTarget == user = [To Self f]
          | otherwise = [To XAlly f, To Enemy f]
        classes = Skill.classes (Context.skill context) `difference` setFromList
                  [Bypassing, Uncounterable, Unreflectable, Unremovable]

targetIsExposed :: ∀ m. MonadPlay m => m Bool
targetIsExposed = do
    target <- P.target
    P.with (\context -> context { Context.user = target }) $
        apply 0 [Invulnerable All]
    null . Effects.invulnerable <$> P.nTarget

hasSkill :: Text -> Ninja -> Bool
hasSkill name n = any ((== name) . Skill.name) $ Ninjas.skills n

withClass :: ∀ m. MonadPlay m => Class -> m () -> m ()
withClass cla = withClasses $ singletonSet cla

withClasses :: ∀ m. MonadPlay m => EnumSet Class -> m () -> m ()
withClasses classes = P.with ctx
  where
    ctx context  = context { Context.skill = withSkill $ Context.skill context }
    withSkill sk = sk { Skill.classes = insertSet All classes }

statusDur :: Text -> Ninja -> Duration
statusDur name n = maybe 0 (unsync . Status.dur) .
                   find ((== name) . Status.name) $ Ninja.statuses n
