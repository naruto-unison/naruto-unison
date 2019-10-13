-- | Processing of 'Effect's that change an action as it occurs.
module Game.Engine.Trigger
  ( absorb
  , redirect

  , targetCounters, targetUncounter
  , userCounters, userUncounter
  ) where

import ClassyPrelude hiding (swap)

import Data.Enum.Set.Class (EnumSet)

import           Class.Hook (MonadHook)
import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Engine.Traps as Traps
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Trap (Trap)
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉))

-- | Trigger an 'Absorb'.
absorb :: ∀ m. MonadPlay m => Ninja -> m ()
absorb n
  | n `is` Absorb = do
      cost <- Skill.cost <$> P.skill
      P.alter $ Game.adjustChakra n (+ cost)
  | otherwise = return ()

-- | Trigger a 'Redirect'.
redirect :: Ninja -> Maybe Slot
redirect n = listToMaybe [slot | Redirect slot <- Ninja.effects n]

getCounters :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
           => (Trap -> Maybe Class) -> Slot -> EnumSet Class -> Ninja -> [m ()]
getCounters f from classes n = mapMaybe g $ Ninja.traps n
  where
    g tr = case f tr of
        Just cla | cla ∈ classes -> Just $ Traps.run from tr
        _                        -> Nothing

userCounters :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
             => Bool -- ^ Enemies were targeted
             -> Slot -> EnumSet Class -> Ninja -> [m ()]
userCounters harmed = getCounters f
  where
    f tr = case Trap.trigger tr of
        Nullified              -> Just All
        Countered cla | harmed -> Just cla
        _                      -> Nothing

userUncounter :: EnumSet Class -> Ninja -> Ninja
userUncounter classes n =
    n { Ninja.traps = filter (keep . Trap.trigger) $ Ninja.traps n }
  where
    keep (Countered cla) = cla ∉ classes
    keep _               = True

targetCounters :: ∀ m. (MonadHook m, MonadPlay m, MonadRandom m)
               => Slot -> EnumSet Class -> Ninja -> [m ()]
targetCounters from classes n
  | n `is` Uncounter = []
  | otherwise        = getCounters f from classes n
  where
    f tr = case Trap.trigger tr of
        CounterAll cla -> Just cla
        Counter cla    -> Just cla
        _              -> Nothing

targetUncounter :: EnumSet Class -> Ninja -> Ninja
targetUncounter classes n
  | n `is` Uncounter = n
  | otherwise        =
    n { Ninja.traps = filter (keep . Trap.trigger) $ Ninja.traps n }
  where
    keep (Counter cla) = cla ∉ classes
    keep _             = True
