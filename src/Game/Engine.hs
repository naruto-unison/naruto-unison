-- | Turn execution. The surface of the game engine.
module Game.Engine (runTurn, processTurn) where

import ClassyPrelude

import           Control.Monad (zipWithM_)
import           Data.List (deleteFirstsBy)

import           Util ((—))
import qualified Class.Hook as Hook
import           Class.Hook (MonadHook)
import qualified Class.Labeled as  Labeled
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame)
import           Class.Random (MonadRandom)
import qualified Class.TurnBased as TurnBased
import qualified Game.Model.Act as Act
import           Game.Model.Act (Act)
import qualified Game.Model.Barrier as Barrier
import qualified Game.Model.Context as Context
import qualified Game.Model.Delay as Delay
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Player as Player
import qualified Game.Model.Runnable as Runnable
import qualified Game.Model.Status as Status
import           Game.Model.Status (Bomb(..), Status)
import qualified Game.Model.Slot as Slot
import           Game.Model.Slot (Slot)
import qualified Game.Engine.Chakras as Chakras
import qualified Game.Engine.Effects as Effects
import qualified Game.Action as Action
import           Game.Action (Affected(..))
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import qualified Game.Engine.Trigger as Trigger

-- | The game engine's main function.
-- Performs 'Act's and 'Model.Channel.Channel's;
-- applies effects from 'Bomb's, 'Barrier.Barrier's, 'Delay.Delay's, and
-- 'Model.Trap.Trap's;
-- decrements all 'TurnBased.TurnBased' data;
-- and resolves 'Model.Chakra.Chakras' for the next turn.
-- Uses 'process' internally.
runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => [Act] -> m ()
runTurn acts = do
    processTurn $ traverse_ Action.act acts
    Chakras.gain

-- | The underlying mechanism of 'run'.
-- Performs posteffects such as 'Model.Channel.Channel's and 'Model.Trap.Trap's.
-- Using 'run' is generally preferable to invoking this function directly.
processTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m () -> m ()
processTurn runner = do
    initial     <- P.ninjas
    player      <- Game.playing <$> P.game
    let opponent = Player.opponent player
    runner
    channels <- concatMap getChannels . filter Ninja.alive <$> P.allies player
    traverse_ Action.act channels
    Traps.runTurn initial
    doBombs Remove initial
    doBarriers
    doDelays
    doDeaths
    expired <- P.ninjas
    P.modifyAll Ninjas.decr
    doBombs Expire expired
    doBombs Done initial
    doHpsOverTime
    P.alter \game -> game { Game.playing = opponent }
    doDeaths
    P.yieldVictor
    Hook.turn
  where
    getChannels n = map (Act.fromChannel n) .
                    filter ((1 /=) . TurnBased.getDur) $
                    Ninja.channels n

-- | Runs 'Game.delays'.
doDelays :: ∀ m. (MonadGame m, MonadRandom m) => m ()
doDelays = traverse_ delay . filter Ninja.alive =<< P.ninjas
  where
    delay n = traverse_ (P.launch . Delay.effect) .
              filter ((<= 1) . Delay.dur) $ Ninja.delays n

-- | Executes 'Status.bombs' of a @Status@.
doBomb :: ∀ m. (MonadGame m, MonadRandom m) => Bomb -> Slot -> Status -> m ()
doBomb bomb target st = traverse_ detonate $ Status.bombs st
  where
    ctx = (Context.fromStatus st) { Context.target = target }
    detonate x
      | bomb == Runnable.target x = P.withContext ctx .
                                    Action.wrap (singletonSet Trapped) $
                                    Runnable.run x
      | otherwise                 = return ()

-- | Executes 'Status.bombs' of all 'Status'es that were removed.
doBombs :: ∀ m. (MonadGame m, MonadRandom m) => Bomb -> [Ninja] -> m ()
doBombs bomb ninjas = zipWithM_ comp ninjas =<< P.ninjas
  where
    comp n n' = sequence $
                doBomb bomb (Ninja.slot n) <$> deleteFirstsBy Labeled.eq
                (Ninja.statuses n) (Ninja.statuses n')

-- | Executes 'Barrier.while' and 'Barrier.finish' effects.
doBarriers :: ∀ m. (MonadGame m, MonadRandom m) => m ()
doBarriers = do
    player <- P.player
    ninjas <- P.ninjas
    traverse_ (doBarrier player) $ concatMap ((head <$>) . collect) ninjas
  where
    collect n = groupBy Labeled.eq . sortWith Barrier.name $ Ninja.barrier n
    doBarrier p b
      | Barrier.dur b == 1 = P.launch . Barrier.finish b $ Barrier.amount b
      | Parity.allied p $ Barrier.user b = P.launch $ Barrier.while b
      | otherwise = return ()

-- | Executes 'Trigger.death'.
doDeaths :: ∀ m. (MonadGame m, MonadRandom m) => m ()
doDeaths = traverse_ Trigger.death Slot.all

-- | Executes 'Model.Effect.Afflict' and 'Model.Effect.Heal'
-- 'Model.Effect.Effect's.
doHpOverTime :: ∀ m. MonadGame m => Slot -> m ()
doHpOverTime slot = do
    player <- P.player
    n      <- P.ninja slot
    hp     <- Effects.hp player n <$> P.ninjas
    when (Ninja.alive n) . P.modify slot $ Ninjas.adjustHealth (— hp)

doHpsOverTime :: ∀ m. MonadGame m => m ()
doHpsOverTime = traverse_ doHpOverTime Slot.all
