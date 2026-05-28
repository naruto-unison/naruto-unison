-- | Actions that characters can use to affect @Trap@s.
module Game.Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , onBreak
  , removeTrap, removeTrap'
  ) where
import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import qualified Game.Action as Action
import qualified Game.Engine.Traps as Traps
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import           Game.Model.Runnable (IntRunConstraint, RunConstraint)
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))

-- | Adds a @Trap@ to 'N.traps' that targets the person it was used on.
trap :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trap = trapConst Trap.Toward mempty
-- | 'Hidden' 'trap'.
trap' :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trap' = trapConst Trap.Toward $ setFromList [Bypassing, Hidden]

-- | Adds a @Trap@ to 'N.traps' that targets the person who triggers it.
trapFrom :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trapFrom = trapConst Trap.From mempty
-- | 'Hidden' 'trapFrom'.
trapFrom' :: ∀ m. MonadPlay m => Duration -> Trigger -> RunConstraint () -> m ()
trapFrom' = trapConst Trap.From $ setFromList [Bypassing, Hidden]

-- | Adds a @Trap@ to 'N.traps' with additional 'Class'es.
trapWith :: ∀ m. MonadPlay m
         => EnumSet Class -> Duration -> Trigger -> RunConstraint () -> m ()
trapWith = trapConst Trap.Toward

-- | Adds a @Trap@ to 'N.traps' with an effect that depends on a number
-- accumulated while the trap is in play and tracked with its 'Trap.tracker'.
trapPer  :: ∀ m. MonadPlay m
         => Duration -> Trigger -> IntRunConstraint () -> m ()
trapPer  = trapFull Trap.Per mempty
-- | 'Hidden' 'trapPer'.
trapPer' :: ∀ m. MonadPlay m
         => Duration -> Trigger -> IntRunConstraint () -> m ()
trapPer' = trapFull Trap.Per $ setFromList [Bypassing, Hidden]

-- | Adds an 'OnBreak' @Trap@ for the used 'Skill.Skill' to 'N.traps'.
-- @OnBreak@ traps are triggered when a @Destructible@ in 'N.defense' with the
-- same 'Destructible.name' is broken.
onBreak :: ∀ m. MonadPlay m => RunConstraint () -> m ()
onBreak f = do
    Context{skill = Skill{name}} <- P.context
    trap' Permanent (OnBreak name) f

-- | Adds a @Trap@ to 'N.traps'.
trapConst :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> RunConstraint () -> m ()
trapConst trapType clas dur tr f = trapFull trapType clas dur tr \_ -> f

-- | Trap engine.
trapFull :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> IntRunConstraint () -> m ()
trapFull direction classes unthrottled trigger f =
    Traps.apply direction classes unthrottled trigger $ Action.wrap . f

-- | Removes 'N.traps' with matching 'Trap.name'.
-- Uses 'Ninjas.clearTrap' internally.
removeTrap :: ∀ m. MonadPlay m => m ()
removeTrap = do
    Context{target, user, skill = Skill{name}} <- P.context
    P.modify target $ Ninjas.clearTrap name user

-- | Removes 'N.traps' with matching 'Trap.name'.
-- Uses 'Ninjas.clearTrap' internally.
removeTrap' :: ∀ m. MonadPlay m => Text -> m ()
removeTrap' name = do
    Context{target, user} <- P.context
    P.modify target $ Ninjas.clearTrap name user
