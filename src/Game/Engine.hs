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

import Data.List (deleteFirstsBy)
import Data.Vector (zipWithM_)

import           Class.Classed (Classed)
import qualified Class.Classed as Classed
import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
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
import           Game.Model.Channel (Channel(Channel))
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import           Game.Model.ID (HasID)
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Game.Model.Status (Bomb(..), Status(Status))
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉))

-- | The game engine's main function.
-- Performs 'Act's and 'Model.Channel.Channel's;
-- applies effects from 'Bomb's, 'Destructible.Destructible's, 'Delay.Delay's, and
-- 'Model.Trap.Trap's;
-- decrements all 'TurnBased.TurnBased' data;
-- and resolves 'Model.Chakra.Chakras' for the next turn.
-- Uses 'processTurn' internally.
runTurn :: ∀ o m. ( MonadGame m, MonadHook m, MonadRandom m
                  , MonoTraversable o, Context ~ Element o
                  ) => o -> m ()
runTurn acts = do
    processTurn $ mapM_ Action.act acts
    Chakra.gainPerAlive

-- | The underlying mechanism of 'runTurn'.
-- Performs posteffects such as 'Model.Channel.Channel's and 'Model.Trap.Trap's.
-- Using 'runTurn' is generally preferable to invoking this function directly.
processTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m () -> m ()
processTurn runner = do
    initial <- P.ninjas
    game@Game{playing = player} <- P.game
    Hook.turnStart game initial
    runner
    channels <- concatMap getChannels . filter N.alive <$> P.allies player
    mapM_ Action.act channels
    Traps.runTurn initial
    doSkillEnds
    doDeaths
    Traps.runExpirations
    expired <- P.ninjas
    P.modifyAll Ninjas.decrement
    doExpiredBombs expired
    doDoneBombs initial
    doHpsOverTime
    P.alterGame Game.swapPlaying
    doDeaths
    yieldVictor
    Hook.turnEnd player initial =<< P.ninjas
  where
    getChannels n = mapMaybe (fromChannel n) $ N.channels n
    fromChannel n (Channel skill target new dur)
      | new || TurnBased.expiring dur = Nothing
      | otherwise = Just Context { new       = False
                                 , user      = N.slot n
                                 , skill     = Skills.change n skill
                                 , continues = False
                                 , target
                                 }

-- | Executes 'Status.bombs' of a @Status@.
doBomb :: ∀ m. (MonadGame m, MonadRandom m) => Bomb -> Slot -> Status -> m ()
doBomb bomb target st@Status{bombs, skill} = mapM_ doEach bombs
  where
    st'
      | bomb == Done = st { Status.skill = Skill.addClass Necromancy skill }
      | otherwise    = st
    context = (Context.fromStatus st') { Context.target = target }
    doEach (To targ run)
      | bomb /= targ = return ()
      | otherwise    = P.withContext context $ Action.wrap run

-- | Executes 'Status.bombs' of all 'Status'es that were removed.
doDoneBombs :: ∀ m. (MonadGame m, MonadRandom m) => Vector Ninja -> m ()
doDoneBombs ninjas = zipWithM_ doEach ninjas =<< P.ninjas
  where
    doEach n n'
      | null stats = return ()
      | otherwise  = sequence_ $ doBomb Done (N.slot n)
                             <$> deleteFirstsBy ((==) `on` ID.from) stats stats'
      where
        stats  = getStatuses n
        stats' = getStatuses n'
        includeStatus Status{bombs = []} = False
        includeStatus Status{bombs, classes} =
            (N.alive n' || Necromancy ∈ classes)
            && any ((== Done) . Runnable.target) bombs
        getStatuses Ninja{statuses} = filter includeStatus statuses

-- | Executes 'Trigger.death'.
doDeaths :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
doDeaths = mapM_ doEach Slot.all
  where
    doEach slot = do
        n@Ninja{statuses} <- P.ninja slot
        let res
              | n `is` Plague = []
              | otherwise     = Traps.getOf slot OnRes n

        if N.alive n then
            return ()

        else if null res then do
            P.modify slot $ Ninjas.clearTraps OnDeath
            sequence_ $ Traps.getOf slot OnDeath n
            mapM_ (doBomb Done slot)
                $ filter ((Necromancy ∉) . Status.classes) statuses
            P.modifyAll $ unSoulbound slot

        else do
            P.modify slot $ Ninjas.setHealth 1 . Ninjas.clearTraps OnRes
            sequence_ res

-- | Removes 'Soulbound' effects. Applied when a Ninja dies or is factory-reset.
unSoulbound :: Slot -> Ninja -> Ninja
unSoulbound user n = Ninjas.modifyAll (filter notSoulbound)
    n { N.copies = filter (maybe True notSoulbound) $ N.copies n }
  where
    notSoulbound :: ∀ a. (Classed a, HasID a) => a -> Bool
    notSoulbound x = Soulbound ∉ Classed.classes x || (ID.user $ ID.from x) /= user

doExpiredBombs :: ∀ m. (MonadGame m, MonadRandom m) => Vector Ninja -> m ()
doExpiredBombs ninjas = mapM_ doEach ninjas
  where
    doEach Ninja{slot, statuses} = mapM_ (doBomb Expire slot)
                                 $ filter TurnBased.expiring statuses

doSkillEnds :: ∀ m. (MonadGame m, MonadRandom m) => m ()
doSkillEnds = mapM_ doEach =<< P.ninjas
  where
    doEach n@Ninja{channels} = mapM_ (runSkillEnd n)
                             $ filter TurnBased.expiring channels

runSkillEnd :: ∀ m. (MonadGame m, MonadRandom m) => Ninja -> Channel -> m ()
runSkillEnd Ninja{slot} (Channel skill@Skill{end} target _ _) = P.launch
    $ To context $ Action.runTargeted end
  where
    context = Context { skill
                      , user      = slot
                      , target
                      , new       = False
                      , continues = False
                      }

-- | Executes 'Model.Effect.Afflict' and 'Model.Effect.Heal'
-- 'Model.Effect.Effect's.
doHpsOverTime :: ∀ m. MonadGame m => m ()
doHpsOverTime = do
    Game{playing = player} <- P.game
    mapM_ (doEach player) =<< P.ninjas
  where
    doEach player n@Ninja{slot} = do
        hp <- Effects.hp player n <$> P.ninjas
        when (N.alive n)
            . P.modify slot $ Ninjas.adjustHealth (- hp)

-- | Updates 'Game.victor'.
yieldVictor :: ∀ m. MonadGame m => m ()
yieldVictor = whenM (Game.inProgress <$> P.game) do
    ninjas <- P.ninjas
    let splitNs = splitAt (length ninjas `quot` 2) ninjas
    P.alterGame $ Game.setVictorBy $ isVictor splitNs
  where
    isVictor (_, ninjas) Player.A = not $ any N.alive ninjas
    isVictor (ninjas, _) Player.B = not $ any N.alive ninjas

forfeit :: ∀ m. MonadGame m => Player -> m ()
forfeit player = whenM (Game.inProgress <$> P.game) do
    P.modifyAll suicide
    P.alterGame $ Game.forfeit player
  where
    suicide n
      | Parity.allied player n = n { N.health = 0 }
      | otherwise              = n

-- | Adds to 'Game.inactive', and forfeits if a threshold is reached.
skipTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
         => Int -> Player -> m ()
skipTurn threshold player = do
    P.alterGame $ Game.incrementInactive player
    Game{inactive} <- P.game
    if Parity.getOf player inactive >= threshold then
        forfeit player
    else
        runTurn []

-- | Resets 'Game.inactive'.
resetInactive :: ∀ m. MonadGame m => Player -> m ()
resetInactive player = P.alterGame $ Game.resetInactive player
