-- 'Trap.Trap' processing.
module Engine.Traps
  ( track
    -- Performing 'Trap.Trap's
  , runTurn
    -- Collecting 'Trap.Trap's
  , get, getOf, getTracked, getPer
  , broken
  ) where

import ClassyPrelude hiding ((\\), toList)

import Data.List ((\\), nub)

import           Core.Util ((∈), (∉))
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame)
import           Class.Random (MonadRandom)
import qualified Model.Character as Character
import qualified Model.Context as Context
import           Model.Context (Context)
import qualified Model.Defense as Defense
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Runnable as Runnable
import           Model.Runnable (Runnable)
import           Model.Player (Player)
import           Model.Slot (Slot)
import qualified Model.Trap as Trap
import           Model.Trap (Trap, Trigger(..))

savedPlay :: Slot -> Trap -> Runnable Context
savedPlay user trap
  | Trap.direction trap == Trap.From = Runnable.retarget withTarget play
  | otherwise                        = play
  where
      withTarget ctx = ctx { Context.target = user }
      play           = Trap.effect trap $ Trap.tracker trap

getOf :: (MonoFoldable o, Trigger ~ Element o) => Slot -> o -> Ninja
      -> Seq (Runnable Context)
getOf user triggers n =
    savedPlay user <$> filter (match . Trap.trigger) (Ninja.traps n)
  where
    match trigger = trigger ∈ Ninja.triggers n && trigger ∈ triggers

get :: Slot -> Ninja -> Seq (Runnable Context)
get user n =
    savedPlay user
    <$> filter ((∈ Ninja.triggers n) . Trap.trigger) (Ninja.traps n)

-- | Adds a value to 'Trap.tracker' of 'Ninja.traps' with a certain 'Trigger'.
track :: Trigger -> Int -> Ninja -> Ninja
track trigger amount n = n { Ninja.traps = tracked <$> Ninja.traps n }
  where
    tracked trap
      | Trap.trigger trap == trigger =
          trap { Trap.tracker = amount + Trap.tracker trap }
      | otherwise = trap

-- | 'OnBreak' effects of 'Ninja.defense' removed during a turn.
broken :: Ninja -- ^ Old.
       -> Ninja -- ^ New.
       -> Ninja
broken n n' =
    n' { Ninja.traps    = filter ((∉ triggers) . Trap.trigger) $ Ninja.traps n'
       , Ninja.triggers = foldl' (flip insertSet) (Ninja.triggers n') triggers
       }
  where
    triggers = OnBreak <$> nub (Defense.name <$> Ninja.defense n)
                        \\ nub (Defense.name <$> Ninja.defense n')

-- | Conditionally returns 'Trap.Trap's that accept a numeric value.
getPer :: Bool -- ^ If 'False', returns 'mempty' instead.
       -> Trigger -- ^ Filter.
       -> Int -- ^ Value to pass to 'Trap.effect'.
       -> Ninja -- 'Ninja.traps' owner.
       -> Seq (Runnable Context)
getPer False _  _   _ = mempty
getPer True  tr amt n = [Trap.effect trap amt | trap <- Ninja.traps n
                                              , Trap.trigger trap == tr]

-- | Conditionally returns 'Character.hooks'.
getHooks :: Bool -- ^ If 'False', returns 'mempty' instead.
         -> Trigger -- ^ Filter.
         -> Int -- ^ Value to pass to 'Character.hooks' effects.
         -> Ninja -- ^ 'Character.hooks' owner.
         -> Seq (Slot, Ninja -> Ninja)
getHooks False _  _   _ = mempty
getHooks True  tr amt n = [(Ninja.slot n, f amt)
                              | (p, f) <- Character.hooks $ Ninja.character n
                              , tr == p]

-- | Tallies 'PerHealed' and 'PerDamaged' hooks.
getTurnHooks :: Player -- ^ Player during the current turn.
             -> Ninja -- ^ Old.
             -> Ninja -- ^ New.
             -> Seq (Slot, Ninja -> Ninja)
getTurnHooks player n n'
  | hp < 0 && Parity.allied player user       = getHooks True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getHooks True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

-- | Conditionally returns 'Trap.tracker' 'Trap.Trap's.
getTracked :: Bool -- ^ If 'False', returns 'mempty' instead.
           -> Trigger -- ^ Filter.
           -> Ninja -- ^ 'Ninja.traps' owner.
           -> Seq (Runnable Context)
getTracked False _ _ = mempty
getTracked True tr n =
    [Trap.effect trap $ Trap.tracker trap | trap <- Ninja.traps n
                                          , Trap.trigger trap == tr
                                          , Trap.dur trap <= 2
                                          , Trap.tracker trap > 0]

-- | Tallies 'PerHealed' and 'PerDamaged' traps.
getTurnPer :: Player -- ^ Player during the current turn.
           -> Ninja -- ^ Old.
           -> Ninja -- ^ New.
           -> Seq (Runnable Context)
getTurnPer player n n'
  | hp < 0 && Parity.allied player user       = getPer True PerHealed (-hp) n'
  | hp > 0 && not (Parity.allied player user) = getPer True PerDamaged hp n'
  | otherwise                                 = mempty
  where
    user = Ninja.slot n'
    hp   = Ninja.health n - Ninja.health n'

-- | Returns 'OnNoAction' 'Trap.Trap's.
getTurnNot :: Player -- ^ Player during the current turn.
           -> Ninja -- ^ 'Ninja.flags' owner.
           -> Seq (Runnable Context)
getTurnNot player n
  | Parity.allied player user = getOf user [OnNoAction] n
  | otherwise                 = mempty
  where
    user = Ninja.slot n

-- | Processes and runs all 'Trap.Trap's at the end of a turn.
runTurn :: ∀ m. (MonadGame m, MonadRandom m) => Vector Ninja -> m ()
runTurn ninjas = do
    player <- P.player
    ninjas' <- P.ninjas
    traverses (uncurry P.modify) $ zipWith (getTurnHooks player) ninjas ninjas'
    traverses P.launch $ zipWith (getTurnPer player) ninjas ninjas'
    traverses P.launch $ getTurnNot player <$> ninjas'
  where
    traverses f = traverse_ $ traverse_ f
