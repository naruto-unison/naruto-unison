-- | Turn execution. The surface of the game engine.
module Game.Engine
  ( runTurn
  , processTurn
  , unSoulbound
  , skipTurn
  , forfeit
  , resetInactive
  ) where

import ClassyPrelude

import Control.Monad (zipWithM_)
import Data.List (deleteFirstsBy)

import           Class.Classed (Classed)
import qualified Class.Classed as Classed
import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
import           Class.Play (MonadGame)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.TurnBased as TurnBased
import qualified Game.Action as Action
import qualified Game.Engine.Chakra as Chakra
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import qualified Game.Engine.Traps as Traps
import           Game.Model.Barrier (Barrier(Barrier))
import qualified Game.Model.Barrier as Barrier
import           Game.Model.Channel (Channel(Channel))
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import qualified Game.Model.Delay as Delay
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Runnable (Runnable(To))
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Status (Bomb(..), Status(Status))
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉))

-- | The game engine's main function.
-- Performs 'Act's and 'Model.Channel.Channel's;
-- applies effects from 'Bomb's, 'Barrier.Barrier's, 'Delay.Delay's, and
-- 'Model.Trap.Trap's;
-- decrements all 'TurnBased.TurnBased' data;
-- and resolves 'Model.Chakra.Chakras' for the next turn.
-- Uses 'processTurn' internally.
runTurn :: ∀ m o. ( MonadGame m, MonadHook m, MonadRandom m
                  , MonoTraversable o, Context ~ Element o
                  ) => o -> m ()
runTurn acts = do
    processTurn $ traverse_ Action.act acts
    Chakra.gain

-- | The underlying mechanism of 'runTurn'.
-- Performs posteffects such as 'Model.Channel.Channel's and 'Model.Trap.Trap's.
-- Using 'runTurn' is generally preferable to invoking this function directly.
processTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m () -> m ()
processTurn runner = do
    initial <- P.ninjas
    Game{playing = player} <- P.game
    let opponent = Player.opponent player
    runner
    channels <- concatMap getChannels . filter N.alive <$> P.allies player
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
    P.alter \g -> g { Game.playing = opponent }
    doDeaths
    yieldVictor
    Hook.turn player initial =<< P.ninjas
  where
    getChannels n = fromChannel n
        <$> filter ((/= -1) . TurnBased.getDur) (N.channels n)
    fromChannel n (Channel skill target _) = Context
        { new       = False
        , user      = N.slot n
        , skill     = Skills.change n skill
        , continues = False
        , target
        }

-- | Runs 'Game.delays'.
doDelays :: ∀ m. (MonadGame m, MonadRandom m) => m ()
doDelays = traverse_ delay . filter N.alive =<< P.ninjas
  where
    delay Ninja{delays} = traverse_ (P.launch . Delay.effect)
        $ filter ((<= -1) . Delay.dur) delays

-- | Executes 'Status.bombs' of a @Status@.
doBomb :: ∀ m. (MonadGame m, MonadRandom m) => Bomb -> Slot -> Status -> m ()
doBomb bomb target st@Status{bombs} = traverse_ detonate bombs
  where
    context = (Context.fromStatus st) { Context.target = target }
    detonate (To targ run)
      | bomb /= targ = return ()
      | otherwise    = P.withContext context $ Action.wrap run

-- | Executes 'Status.bombs' of all 'Status'es that were removed.
doBombs :: ∀ m. (MonadGame m, MonadRandom m) => Bomb -> [Ninja] -> m ()
doBombs bomb ninjas = zipWithM_ comp ninjas =<< P.ninjas
  where
    comp n n' = sequence
              $ doBomb bomb (N.slot n)
              <$> deleteFirstsBy Labeled.eq (stats n) (stats n')
      where
        stats
          | N.alive n' = N.statuses
          | otherwise  = filter ((Necromancy ∈) . Status.classes)
                       . N.statuses

-- | Executes 'Barrier.while' and 'Barrier.finish' effects.
doBarriers :: ∀ m. (MonadGame m, MonadRandom m) => m ()
doBarriers = do
    Game{playing = player} <- P.game
    ninjas <- P.ninjas
    traverse_ (doBarrier player) $ concatMap ((head <$>) . collect) ninjas
  where
    collect Ninja{barrier} = groupBy Labeled.eq $ sortWith Barrier.name barrier
    doBarrier p Barrier{amount, dur, finish, user, while}
      | dur == -1            = P.launch $ finish amount
      | Parity.allied p user = P.launch while
      | otherwise            = return ()

-- | Executes 'Trigger.death'.
doDeaths :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
doDeaths = traverse_ doDeath Slot.all

-- | If the 'N.health' of a 'Ninja' reaches 0,
-- they are either resurrected by triggering 'OnRes'
-- or they die and trigger 'OnDeath'.
-- If they die, their 'Soulbound' effects are canceled.
doDeath :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Slot -> m ()
doDeath slot = do
    n@Ninja{health, statuses} <- P.ninja slot
    let res
          | n `is` Plague = mempty
          | otherwise     = Traps.getOf slot OnRes n

    if health > 0 then
        return ()

    else if null res then do
        P.modify slot $ Ninjas.clearTraps OnDeath
        sequence_ $ Traps.getOf slot OnDeath n
        traverse_ (doBomb Done slot)
            $ filter ((Necromancy ∉) . Status.classes) statuses
        P.modifyAll $ unSoulbound slot

    else do
        P.modify slot $ Ninjas.setHealth 1 . Ninjas.clearTraps OnRes
        sequence_ res

-- | Removes 'Soulbound' effects. Applied when a Ninja dies or is factory-reset.
unSoulbound :: Slot -> Ninja -> Ninja
unSoulbound user n@Ninja{copies, statuses, traps} = Ninjas.modifyStatuses
    (const $ filter notSoulbound statuses)
    $ n { N.traps  = filter notSoulbound traps
        , N.copies = filter (maybe True notSoulbound) copies
        }
  where
    notSoulbound :: ∀ a. (Classed a, Labeled a) => a -> Bool
    notSoulbound x = Soulbound ∉ Classed.classes x || Labeled.user x /= user

-- | Executes 'Model.Effect.Afflict' and 'Model.Effect.Heal'
-- 'Model.Effect.Effect's.
doHpsOverTime :: ∀ m. MonadGame m => m ()
doHpsOverTime = traverse_ doHpOverTime Slot.all

doHpOverTime :: ∀ m. MonadGame m => Slot -> m ()
doHpOverTime slot = do
    Game{playing = player} <- P.game
    n  <- P.ninja slot
    hp <- Effects.hp player n <$> P.ninjas
    when (N.alive n)
        . P.modify slot $ Ninjas.adjustHealth (- hp)

-- | Updates 'Game.victor'.
yieldVictor :: ∀ m. MonadGame m => m ()
yieldVictor = whenM (Game.inProgress <$> P.game) do
    ninjas <- P.ninjas
    let splitNs = splitAt (length ninjas `quot` 2) ninjas
    P.alter \g ->
        g { Game.victor = filter (victor splitNs) [Player.A, Player.B] }
  where
    victor (_, ninjas) Player.A = not $ any N.alive ninjas
    victor (ninjas, _) Player.B = not $ any N.alive ninjas

forfeit :: ∀ m. MonadGame m => Player -> m ()
forfeit player = whenM (Game.inProgress <$> P.game) do
    P.modifyAll suicide
    P.alter \g -> g { Game.victor  = [Player.opponent player]
                    , Game.forfeit = True
                    }
  where
    suicide n
      | Parity.allied player n = n { N.health = 0 }
      | otherwise              = n

-- | Adds to 'Game.inactive', and forfeits if a threshold is reached.
skipTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
         => Int -> Player -> m ()
skipTurn threshold player = do
    P.alter \g ->
        g { Game.inactive = Parity.modifyOf player (+ 1) $ Game.inactive g }
    Game{inactive} <- P.game
    if Parity.getOf player inactive >= threshold then
        forfeit player
    else
        runTurn []

-- | Resets 'Game.inactive'.
resetInactive :: ∀ m. MonadGame m => Player -> m ()
resetInactive player = P.alter \g ->
    g { Game.inactive = Parity.setOf player 0 $ Game.inactive g }
