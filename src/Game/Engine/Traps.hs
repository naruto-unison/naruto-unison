-- 'Trap.Trap' processing.
module Game.Engine.Traps
  ( run
  , track
    -- Performing 'Trap.Trap's
  , runTurn, runExpirations
    -- Collecting 'Trap.Trap's
  , runTriggers, getOf
  , apply
  ) where

import ClassyPrelude hiding ((\\), toList)

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
import           Game.Model.Requirement (Requirement(..))
import           Game.Model.Runnable (Runnable(To), IntRunConstraint)
import qualified Game.Model.Runnable as Runnable
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger(Trigger(..))
import qualified Game.Model.Trigger as Trigger
import           Util ((∈), (∉))

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

runTriggers :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> m ()
runTriggers user n@Ninja{traps, triggers}
  | not $ N.alive n = return ()
  | otherwise       = do
        mapM_ (`Hook.trigger` n) triggers
        mapM_ (run user) traps'
  where
      traps' = filter ((∈ triggers) . Trap.trigger) traps

-- | Adds a value to 'Trap.tracker' of 'N.traps' with a certain @Trigger@.
track :: Trigger -> Int -> Ninja -> Ninja
track trigger amount n = n { N.traps = tracked <$> N.traps n }
  where
    tracked trap
      | Trap.trigger trap == trigger =
          trap { Trap.tracker = amount + Trap.tracker trap }
      | otherwise = trap

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
    mapM_ sequence_ $ zipWith (getTurnPer player) ninjas ninjas'
    mapM_ sequence_ $ getTurnNot <$> Parity.half player ninjas'

runExpirations :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
runExpirations = mapM_ expire =<< P.ninjas
  where
    expire n@Ninja{slot, traps}
      | null yays = return ()
      | otherwise = do
            P.modify slot \n' -> n' { N.traps = nays }
            mapM_ (run slot) yays
      where
        (yays, nays) = partition (Trap.isExpiring n) traps

-- | Trap engine.
apply :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> IntRunConstraint () -> m ()
apply direction classes unthrottled trigger f = void $ runMaybeT do
    context@Context{new, target} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget
    dur     <- if not new then return unthrottled else
               hoistMaybe $ throttle nUser
    let tr = makeTrap context direction classes dur trigger f
    guard $ tr ∉ N.traps nTarget
    guard . not $ isCounter && nUser `is` Disable Counters
    P.modify target $ Ninjas.addTrap tr
  where
    isCounter = Trigger.isCounter trigger
    throttle n
      | isCounter = Duration.throttle (Effects.throttleCounters n) unthrottled
      | otherwise = Just unthrottled

makeTrap :: Context -> Trap.Direction -> EnumSet Class -> Duration
         -> Trigger -> IntRunConstraint () -> Trap
makeTrap ctx direction classes dur trigger f = Trap
    { trigger
    , direction
    , skill   = skill'
    , user
    , name    = Skill.name skill'
    , effect  = \i -> To { target = context, run = f i }
    , classes = classes'
    , tracker = 0
    , dur     = succ dur
    }
  where
    Context{continues, new, skill, user} = ctx
    setContinues
      | continues && dur <= 1 = insertSet Continues
      | continues || new      = deleteSet Continues
      | otherwise             = deleteSet Continues . deleteSet Invisible
    setNecromancy
      | Trigger.affectsDead trigger = insertSet Necromancy
      | otherwise                   = id
    classes' = insertSet Nonstacking . setContinues . setNecromancy
             $ classes ++ Skill.classes skill
    skill'   = skill { Skill.classes = classes'
                     , Skill.require = Usable
                     }
    context  = ctx { Context.skill     = skill'
                   , Context.continues = False
                   , Context.new       = False
                   }
