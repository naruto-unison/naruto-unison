-- 'Trap.Trap' processing.
module Game.Engine.Traps
  ( run
  , track
    -- Performing 'Trap.Trap's
  , runTriggers, runDeaths, runTurn, runExpirations
  , apply
  ) where

import ClassyPrelude hiding ((\\))

import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)
import Data.Enum.Set (EnumSet)

import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Duration (Duration(..))
import qualified Game.Model.Duration as Duration
import           Game.Model.Effect (Constructor(..), Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import           Game.Model.Runnable (Runnable(To), IntRunConstraint)
import qualified Game.Model.Runnable as Runnable
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger(Trigger(..))
import qualified Game.Model.Trigger as Trigger
import           Util ((∈), intersects)

launch :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
       => Trap -> Runnable Context -> m ()
launch trap (To context@Context{target} f)
  | not $ Trap.uncopied trap = P.withContext context f
  | otherwise                = do
    nTarget <- P.ninja target
    P.withContext context f
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
getOf user trigger Ninja{traps}
    = run user <$> filter ((== trigger) . Trap.trigger) traps

runTriggers :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> m ()
runTriggers user = do
    mapM_ (runTriggersOf user) =<< P.ninjas
    P.modifyAll clearTriggers
  where
    clearTriggers n = Ninjas.clearAnyTraps singleUses n { N.triggers = mempty }
      where
        singleUses = filterSet Trigger.isSingleUse n.triggers

runTriggersOf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> m ()
runTriggersOf user n@Ninja{traps, triggers}
  | null triggers = return ()
  | otherwise     = do
    mapM_ (`Hook.trigger` n) triggers
    mapM_ (run user) $ filter ((∈ triggers) . Trap.trigger) traps

runDeaths :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Maybe Slot -> m ()
runDeaths muser = void $ iterateWhile (any id) $ mapM doEach =<< P.ninjas
  where
    doEach n@Ninja{slot} = runDeathTriggersOf (fromMaybe slot muser) n

runDeathTriggersOf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> m Bool
runDeathTriggersOf user n@Ninja{slot, traps}
  | N.alive n = return False
  | not $ null resurrectTraps = do
        Hook.trigger Resurrect n
        P.modify slot \n' -> Ninjas.clearTraps Resurrect n' { N.health = 1 }
        mapM_ (run user) resurrectTraps
        return True
  | not $ null onDeathTraps = do
        Hook.trigger OnDeath n
        P.modify slot $ Ninjas.clearTraps OnDeath
        mapM_ (run user) onDeathTraps
        return True
  | otherwise = do
        Hook.trigger OnDeath n
        return False
  where
    trapsOf trigger = filter ((== trigger) . Trap.trigger) traps
    resurrectTraps
      | N.alive n || n `is` Plague = mempty
      | otherwise = trapsOf Resurrect
    onDeathTraps = trapsOf OnDeath

-- | Adds a value to 'Trap.tracker' of 'N.traps' with a certain @Trigger@.
track :: Trigger -> Int -> Ninja -> Ninja
track trigger amount n = n { N.traps = tracked <$> n.traps }
  where
    tracked trap
      | trap.trigger == trigger = trap { Trap.tracker = amount + trap.tracker }
      | otherwise               = trap

-- | Conditionally returns 'Trap.Trap's that accept a numeric value.
getPer :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
       => Bool -- ^ If False, returns @mempty@ instead.
       -> Trigger -- ^ Filter.
       -> Int -- ^ Value to pass to 'Trap.effect'.
       -> Ninja -- 'N.traps' owner.
       -> [m ()]
getPer False _       _   _ = mempty
getPer True  trigger amt Ninja{traps} =
    [ launch trap $ trap.effect amt | trap <- traps
                                    , trap.trigger == trigger ]

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
    hp   = n.health - n'.health

-- | Returns 'OnNoAction' 'Trap.Trap's.
getTurnNot :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
           => Ninja -- ^ 'N.flags' owner.
           -> [m ()]
getTurnNot n
  | n.acted   = mempty
  | N.alive n = getOf n.slot OnNoAction n
  | otherwise = mempty

-- | Processes and runs all 'Trap.Trap's at the end of a turn.
runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
        => Vector Ninja -> m ()
runTurn ninjas = do
    Game{playing = player} <- P.game
    ninjas' <- P.ninjas
    mapM_ sequence_ $ zipWith (getTurnPer player) ninjas ninjas'
    mapM_ sequence_ $ getTurnNot <$> Parity.half player ninjas'

runExpirations :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
runExpirations = mapM_ expire =<< P.ninjas
  where
    expire n
      | null yays = return ()
      | otherwise = do
            P.modify n.slot \n' -> n' { N.traps = nays }
            mapM_ (run n.slot) yays
      where
        (yays, nays) = partition (Trap.isExpiring n) n.traps

-- | Trap engine.
apply :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> IntRunConstraint () -> m ()
apply direction classes unthrottled trigger f = void $ runMaybeT do
    context@Context{new, target} <- P.context
    nUser <- P.nUser
    guard . not $ isCounter && nUser `is` Disable Counters
    dur   <- if not new || isChanneled then return unthrottled else
                hoistMaybe $ throttle nUser
    let trap = makeTrap context direction classes dur trigger f
    P.modify target $ Ninjas.addTrap trap
  where
    isChanneled = setFromList [Continues, Controlled] `intersects` classes
    isCounter = Trigger.isCounter trigger
    throttle n
      | isCounter = Duration.throttle (Effects.throttleCounters n) unthrottled
      | otherwise = Just unthrottled

makeTrap :: Context -> Trap.Direction -> EnumSet Class -> Duration
         -> Trigger -> IntRunConstraint () -> Trap
makeTrap ctx@Context { continues
                     , new
                     , skill
                     , user
                     } direction classes dur trigger f = Trap
    { trigger
    , direction
    , skill   = skill'
    , user
    , name    = skill'.name
    , effect  = \i -> To context $ f i
    , classes = classes'
    , tracker = 0
    , dur     = succ dur
    }
  where
    setContinues
      | continues && dur <= 1 = insertSet Continues
      | continues || new      = deleteSet Continues
      | otherwise             = deleteSet Continues . deleteSet Invisible
    setNecromancy
      | Trigger.affectsDead trigger = insertSet Necromancy
      | otherwise                   = id
    classes' = insertSet Nonstacking . setContinues . setNecromancy
             $ classes ++ skill.classes
    skill'   = skill { Skill.classes = classes'
                     , Skill.require = mempty
                     }
    context  = ctx { Context.skill     = skill'
                   , Context.continues = False
                   , Context.new       = False
                   }
