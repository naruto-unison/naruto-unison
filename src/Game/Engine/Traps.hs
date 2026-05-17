-- 'Trap.Trap' processing.
module Game.Engine.Traps
  ( run
  , track
    -- Performing 'Trap.Trap's
  , runTurn
    -- Collecting 'Trap.Trap's
  , get, getOf
  , broken
  ) where

import ClassyPrelude hiding ((\\), toList)

import Data.List ((\\), nub)

import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import           Game.Model.Context (Context)
import qualified Game.Model.Context as Context
import qualified Game.Model.Defense as Defense
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import           Game.Model.Runnable (Runnable)
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Slot (Slot)
import           Game.Model.Trap (Trap)
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger(Trigger(..))
import           Util ((∈))

launch :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
       => Trap -> Runnable Context -> m ()
launch trap runner = do
    P.launch runner
    nTarget <- P.ninja . Context.target $ Runnable.target runner
    when (Trap.uncopied trap)
        $ Hook.trap trap nTarget

run :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Trap -> m ()
run user trap = launch trap case Trap.direction trap of
    Trap.From -> Runnable.retarget ctx play
    _         -> play
  where
    play        = Trap.effect trap $ Trap.tracker trap
    ctx context = context { Context.target = user }

getOf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
      => Slot -> Trigger -> Ninja -> [m ()]
getOf user trigger n = run user
    <$> filter ((== trigger) . Trap.trigger) (N.traps n)

get :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> [m ()]
get user n
  | N.alive n = hooks : (run user <$> traps)
  | otherwise     = []
  where
      hooks = traverse_ (`Hook.trigger` n) $ N.triggers n
      traps = filter ((∈ N.triggers n) . Trap.trigger) $ N.traps n

-- | Adds a value to 'Trap.tracker' of 'N.traps' with a certain @Trigger@.
track :: Trigger -> Int -> Ninja -> Ninja
track trigger amount n = n { N.traps = tracked <$> N.traps n }
  where
    tracked trap
      | Trap.trigger trap == trigger =
          trap { Trap.tracker = amount + Trap.tracker trap }
      | otherwise = trap

-- | 'OnBreak' effects of 'N.defense' removed during a turn.
broken :: Ninja -- ^ Old.
       -> Ninja -- ^ New.
       -> Ninja
broken n n' =
    n' { N.triggers = foldl' (flip insertSet) (N.triggers n') triggers }
  where
    triggers = OnBreak
        <$> nub (Defense.name <$> N.defense n)
            \\ nub (Defense.name <$> N.defense n')

-- | Conditionally returns 'Trap.Trap's that accept a numeric value.
getPer :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
       => Bool -- ^ If False, returns @mempty@ instead.
       -> Trigger -- ^ Filter.
       -> Int -- ^ Value to pass to 'Trap.effect'.
       -> Ninja -- 'N.traps' owner.
       -> [m ()]
getPer False _  _   _ = mempty
getPer True  tr amt n =
    [launch trap $ Trap.effect trap amt | trap <- N.traps n
                                        , Trap.trigger trap == tr]

-- | Tallies 'PerDamaged' traps.
getTurnPer :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
           => Player -- ^ Player during the current turn.
           -> Ninja -- ^ Old.
           -> Ninja -- ^ New.
           -> [m ()]
getTurnPer player n n'
  | not $ N.alive n'     = mempty
  | hp > 0 && not allied = getPer True PerDamaged hp n'
  | otherwise            = mempty
  where
    allied = Parity.allied player n'
    hp   = N.health n - N.health n'

-- | Returns 'OnNoAction' 'Trap.Trap's.
getTurnNot :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
           => Ninja -- ^ 'N.flags' owner.
           -> [m ()]
getTurnNot n
  | N.acted n = mempty
  | N.alive n = getOf (N.slot n) OnNoAction n
  | otherwise = mempty

-- | Processes and runs all 'Trap.Trap's at the end of a turn.
runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => [Ninja] -> m ()
runTurn ninjas = do
    player  <- P.player
    ninjas' <- P.ninjas
    traverse_ sequence_ $ zipWith (getTurnPer player) ninjas ninjas'
    traverse_ sequence_ $ getTurnNot <$> Parity.half player ninjas'
