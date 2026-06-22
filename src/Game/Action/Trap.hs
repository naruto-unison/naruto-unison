-- | Actions that characters can use to affect @Trap@s.
module Game.Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , controlTrap, controlTrapFrom
  , onBreak, onBreakFrom
  , removeTrap
  ) where
import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Engine.Traps as Traps
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Channel (Channeling(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration(..))
import           Game.Model.Runnable (IntRunConstraint, RunConstraint)
import qualified Game.Model.ID as ID
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))

-- | Adds a @Trap@ to 'N.traps' that targets the person it was used on.
trap :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trap = trapConst Trap.Toward mempty
-- | 'Hidden' 'trap'.
trap' :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trap' = trapConst Trap.Toward $ setFromList [Hidden]

-- | Adds a @Trap@ to 'N.traps' that targets the person who triggers it.
trapFrom :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trapFrom = trapConst Trap.From mempty
-- | 'Hidden' 'trapFrom'.
trapFrom' :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trapFrom' = trapConst Trap.From $ setFromList [Hidden]

-- | Adds a @Trap@ to 'N.traps' with additional 'Class'es.
trapWith :: ∀ m. MonadPlay m
         => EnumSet Class -> Duration -> Trigger -> RunConstraint () -> m ()
trapWith = trapConst Trap.Toward

-- | Adds a @Trap@ to 'N.traps' with an effect that depends on a number
-- accumulated while the trap is in play and tracked with its 'Trap.tracker'.
trapPer  :: ∀ m. MonadPlay m
         => Duration -> Trigger -> IntRunConstraint () -> m ()
trapPer  = Traps.apply Trap.Per mempty
-- | 'Hidden' 'trapPer'.
trapPer' :: ∀ m. MonadPlay m
         => Duration -> Trigger -> IntRunConstraint () -> m ()
trapPer' = Traps.apply Trap.Per $ setFromList [Bypassing, Hidden]

controlTrapConst :: ∀ m. MonadPlay m
                 => Trap.Direction -> EnumSet Class -> Trigger
                 -> RunConstraint () -> m ()
controlTrapConst trapType clas tr f = do
    Context{skill = Skill{dur}} <- P.context
    case dur of
        Control i -> trapConst trapType clas i tr f
        _         -> return ()

controlTrap :: ∀ m. MonadPlay m => Trigger -> RunConstraint () -> m ()
controlTrap = controlTrapConst Trap.Toward $ singleton Controlled

controlTrapFrom :: ∀ m. MonadPlay m => Trigger -> RunConstraint () -> m ()
controlTrapFrom = controlTrapConst Trap.From $ singleton Controlled

-- | Adds an 'OnBreak' @Trap@ for the used 'Skill.Skill' to 'N.traps'.
-- @OnBreak@ traps are triggered when a @Destructible@ in 'N.defense' with the
-- same 'Destructible.name' is broken.
onBreak :: ∀ m. MonadPlay m => RunConstraint () -> m ()
onBreak f = do
    triggerID <- ID.from <$> P.context
    trap' Permanent (OnBreak triggerID) f

-- | Adds an 'OnBreak' @Trap@ for the used 'Skill.Skill' to 'N.traps'.
-- @OnBreak@ traps are triggered when a @Destructible@ in 'N.defense' with the
-- same 'Destructible.name' is broken.
onBreakFrom :: ∀ m. MonadPlay m => RunConstraint () -> m ()
onBreakFrom f = do
    triggerID <- ID.from <$> P.context
    trapFrom' Permanent (OnBreak triggerID) f

-- | Adds a @Trap@ to 'N.traps'.
trapConst :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> RunConstraint () -> m ()
trapConst trapType clas dur tr f = Traps.apply trapType clas dur tr $ const f

-- | Removes 'N.traps' with matching 'Trap.name'.
-- Uses 'Ninjas.clearTrap' internally.
removeTrap :: ∀ m. MonadPlay m => Text -> m ()
removeTrap name = P.toTargetFromUser Ninjas.clearTrap name
