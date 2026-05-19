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
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import           Game.Model.Runnable (Runnable)
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Slot (Slot)
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger(Trigger(..))
import           Util ((∈))

launch :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
       => Trap -> Runnable Context -> m ()
launch trap runner
  | not $ Trap.uncopied trap = P.launch runner
  | otherwise                = do
    nTarget <- P.ninja . Context.target $ Runnable.target runner
    P.launch runner
    Hook.trap trap nTarget

run :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Trap -> m ()
run user trap@Trap{direction = Trap.From, effect, tracker} =
    launch trap $ Runnable.retarget ctx $ effect tracker
  where
    ctx context = context { Context.target = user }

run _ trap@Trap{effect, tracker} = launch trap $ effect tracker

getOf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
      => Slot -> Trigger -> Ninja -> [m ()]
getOf user trigger Ninja{traps} = run user
    <$> filter ((== trigger) . Trap.trigger) traps

get :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> [m ()]
get user n@Ninja{traps, triggers}
  | N.alive n = hooks : (run user <$> traps')
  | otherwise = []
  where
      hooks = traverse_ (`Hook.trigger` n) triggers
      traps' = filter ((∈ triggers) . Trap.trigger) traps

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
getPer True  tr amt Ninja{traps} =
    [launch trap $ effect amt | trap@Trap{effect, trigger} <- traps
                               , trigger == tr]

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
getTurnNot n@Ninja{acted, slot}
  | acted     = mempty
  | N.alive n = getOf slot OnNoAction n
  | otherwise = mempty

-- | Processes and runs all 'Trap.Trap's at the end of a turn.
runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => [Ninja] -> m ()
runTurn ninjas = do
    Game{playing = player} <- P.game
    ninjas' <- P.ninjas
    traverse_ sequence_ $ zipWith (getTurnPer player) ninjas ninjas'
    traverse_ sequence_ $ getTurnNot <$> Parity.half player ninjas'
