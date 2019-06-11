module Engine.Turn
  ( run
  ) where

import ClassyPrelude.Yesod hiding (Status, (\\), groupBy, drop, head)
import           Data.List ((\\))
import           Data.List.NonEmpty (groupBy, head)

import           Core.Util ((—))
import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (GameT)
import           Class.Random (RandomT)
import qualified Class.TurnBased as TurnBased
import qualified Model.Act as Act
import           Model.Act (Act)
import qualified Model.Barrier as Barrier
import qualified Model.Context as Context
import qualified Model.Delay as Delay
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import qualified Model.Player as Player
import qualified Model.Status as Status
import           Model.Status (Bomb(..), Status)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Engine.Chakras as Chakras
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))
import qualified Engine.Traps as Traps
import qualified Engine.Trigger as Trigger

doDelays :: ∀ m. (GameT m, RandomT m) => m ()
doDelays = filter ((<= 1) . Delay.dur) . Game.delays <$> P.game >>=
           traverse_ (P.launch . Delay.effect)

doBomb :: ∀ m. (GameT m, RandomT m) => Bomb -> Slot -> Status -> m ()
doBomb bomb target st = traverse_ detonate $ Status.bombs st
  where
    ctx = (Context.fromStatus st) { Context.target = target }
    detonate (bomb', f)
      | bomb == bomb' = P.withContext ctx . Execute.wrap [Trapped] $ P.play f
      | otherwise     = return ()

doBombs :: ∀ m. (GameT m, RandomT m) => Bomb -> Game -> m ()
doBombs bomb game = do
    ninjas <- Game.ninjas <$> P.game
    sequence_ . concat $ zipWith comp (Game.ninjas game) ninjas
  where
    comp n n' = doBomb bomb (Ninja.slot n) <$>
                Ninja.statuses n \\ Ninja.statuses n'

doBarriers :: ∀ m. (GameT m, RandomT m) => m ()
doBarriers = do
    player <- P.player
    ninjas <- Game.ninjas <$> P.game
    traverse_ (doBarrier player) $ concatMap collect ninjas
  where
    collect = (head <$>) . groupBy Labeled.eq . sort . Ninja.barrier
    doBarrier p b
      | Barrier.dur b == 1 = P.launch . Barrier.finish b $ Barrier.amount b
      | Parity.allied p $ Barrier.source b = P.launch $ Barrier.while b
      | otherwise = return ()

doTraps :: ∀ m. (GameT m, RandomT m) => m ()
doTraps = do
    traps <- Game.traps <$> P.game
    P.modify \game -> game { Game.traps = mempty }
    traverse_ P.launch traps

doDeaths :: ∀ m. (GameT m, RandomT m) => m ()
doDeaths = traverse_ Trigger.death Slot.all

doHpOverTime :: ∀ m. GameT m => Slot -> m ()
doHpOverTime slot = do
    player <- P.player
    n      <- Game.ninja slot <$> P.game
    hp     <- Effects.hp player n <$> P.game
    when (Ninja.alive n) . P.modify . Game.adjust slot $
        Ninja.adjustHealth (— hp)

doHpsOverTime :: ∀ m. GameT m => m ()
doHpsOverTime = traverse_ doHpOverTime Slot.all

-- | The game engine's main function. Performs 'Act's and 'Channel's;
-- applies effects from 'Bomb's, 'Barrier's, 'Delay's, and 'Trap's;
-- decrements all 'TurnBased' data; adds 'ChannelTag's;
-- and resolves 'Chakras' for the next turn.
run :: ∀ m. (GameT m, RandomT m) => [Act] -> m ()
run actions = do
    initial     <- P.game
    player      <- Game.playing <$> P.game
    let opponent = Player.opponent player
    traverse_ (Execute.act []) actions
    channels <- concatMap getChannels . filter (Ninja.playing player) .
                Game.ninjas <$> P.game
    traverse_ (Execute.act [Channeled]) channels
    P.modify $ Traps.runTurn player
    doBombs Remove initial
    P.modify $ Game.alter (Ninja.decrStats <$>)
    doBarriers
    Traps.runPer initial
    doTraps
    doDelays
    doDeaths
    P.modify \game -> game { Game.delays = decrDelays $ Game.delays game }
    expired <- P.game
    P.modify $ Game.alter (Ninja.decr <$>)
    doBombs Expire expired
    doBombs Done initial
    doHpsOverTime
    P.modify \game -> game { Game.playing = opponent }
    doDeaths
    Chakras.gain
  where
    getChannels n = map (Act.fromChannel n) .
                    filter ((1 /=) . TurnBased.getDur) $
                    Ninja.channels n
    decrDelays = filter ((0 /=) . Delay.dur) . mapMaybe TurnBased.decr
