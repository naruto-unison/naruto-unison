-- | Processing of 'Effect's that change an action as it occurs.
module Game.Engine.Counter
  ( filterCounters
  , targetCounters, targetUncounter
  , userCounters, userUncounter
  ) where

import ClassyPrelude hiding (swap)

import Data.Enum.Set (EnumSet)

import           Class.Hook (MonadHook)
import           Class.Play (MonadPlay)
import           Class.Random (MonadRandom)
import qualified Game.Engine.Traps as Traps
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (Runnable)
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Slot (Slot, SlotSet)
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉))

-- | Filters a list of targets to those capable of countering a skill.
filterCounters :: ∀ o. (IsSequence o, Ninja ~ Element o)
               => [[Runnable Slot]] -- ^ Effects of the skill to be countered.
               -> o -> o
filterCounters slots = filter $ (∈ targetSet) . N.slot
  where
    targetSet :: SlotSet
    targetSet = setFromList $ Runnable.target <$> join slots

getCounters :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
           => (Trigger -> Maybe Class) -> Slot -> EnumSet Class -> Ninja -> [m ()]
getCounters f from classes Ninja{traps} = Traps.run from <$> filter include traps
  where
    include Trap{trigger} = case f trigger of
        Just cla | cla ∈ classes -> True
        _                        -> False

-- | 'Countered' and 'Nullified' traps.
userCounters :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
             => Bool -- ^ Enemies were targeted
             -> Slot -> EnumSet Class -> Ninja -> [m ()]
userCounters harmed = getCounters f
  where
    f Nullified                = Just All
    f (Countered cla) | harmed = Just cla
    f _                        = Nothing

-- | Removes 'Countered' traps matching the specified @Class@es.
userUncounter :: EnumSet Class -> Ninja -> Ninja
userUncounter classes n = n { N.traps = filter (keep . Trap.trigger) n.traps }
  where
    keep (Countered cla) = cla ∉ classes
    keep _               = True

-- | 'CounterAll' and 'Counter' traps.
targetCounters :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
               => Slot -> EnumSet Class -> Ninja -> [m ()]
targetCounters from classes n
  | n `is` Uncounter = mempty
  | otherwise        = getCounters f from classes n
  where
    f (CounterAll cla) = Just cla
    f (Counter    cla) = Just cla
    f _                = Nothing

-- | Removes 'Counter' traps matching the specified @Class@es.
targetUncounter :: EnumSet Class -> Ninja -> Ninja
targetUncounter classes n
  | n `is` Uncounter = n
  | otherwise        = n { N.traps = filter (keep . Trap.trigger) n.traps }
  where
    keep (Counter cla) = cla ∉ classes
    keep _             = True
