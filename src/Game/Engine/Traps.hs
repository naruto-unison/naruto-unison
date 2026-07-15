-- 'Trap.Trap' processing.
module Game.Engine.Traps
  ( run
    -- Performing 'Trap.Trap's
  , runTriggers, runDeaths, runTurn, runExpirations
  , apply
  ) where

import ClassyPrelude hiding (deleteBy)

import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.Maybe (MaybeT(..), hoistMaybe)
import Data.Enum.Set (EnumSet)
import Data.List (deleteBy)

import           Class.Classed (Classed(..))
import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Class as Class
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
import           Util ((∈), (∉), intersects, insertIf)

launch :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
       => Trap -> Runnable Context -> m ()
launch trap (To context@Context{target} f)
  | not $ Trap.uncopied trap = P.withContext context f
  | otherwise                = do
    nTarget <- P.ninja target
    P.withContext context f
    Hook.trap trap nTarget

isHpTrap :: Ninja -> Trap -> Bool
isHpTrap Ninja{health} Trap{trigger = OnHealthMax hp} = health <= hp
isHpTrap _ _ = False

run :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Trap -> m ()
run user trap@Trap{direction = Trap.From, effect} =
    launch trap $ Runnable.retarget ctx $ effect 0
  where
    ctx context = context { Context.target = user }

run _ trap@Trap{effect} = launch trap $ effect 0

runAndRemoveIf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
                 => (Trap -> Bool) -> Slot -> Slot -> m Bool
runAndRemoveIf predicate user slot = do
    n@Ninja{traps} <- P.ninja slot
    let (yays, nays) = partition predicate traps
    if null yays then
        return False
    else do
        P.write slot n { N.traps = nays }
        mapM_ (run user) yays
        return True

runTriggers :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> m ()
runTriggers user = do
    mapM_ (runTriggersOf user) =<< P.ninjas
    P.modifyAll clearTriggers
  where
    clearTriggers n
      | null singleUses = n { N.triggers = mempty }
      | otherwise       = n { N.triggers = mempty
                            , N.traps    = filter notSingleUse n.traps
                            }
      where
        singleUses   = filterSet Trigger.isSingleUse n.triggers
        notSingleUse = (∉ singleUses) . Trap.trigger

runTriggersOf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> m ()
runTriggersOf user n@Ninja{slot, traps, triggers}
  | null triggers = return ()
  | otherwise     = do
    mapM_ (`Hook.trigger` n) triggers
    void $ runAndRemoveIf (isHpTrap n) user slot
    mapM_ (run user) $ filter ((∈ triggers) . Trap.trigger) traps

runDeaths :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Maybe Slot -> m ()
runDeaths muser = void $ iterateWhile (any id)
                $ mapM doEach =<< P.ninjas
  where
    doEach n@Ninja{slot} = runDeathTriggersOf (fromMaybe slot muser) n

runDeathTriggersOf :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
    => Slot -> Ninja -> m Bool
runDeathTriggersOf user n@Ninja{slot, traps}
  | alive     = runAndRemoveIf (isHpTrap n) user slot
  | otherwise = case resurrectTrap of
        Just res -> do
            Hook.trigger Resurrect n
            P.modify slot \n' -> n'
                { N.health = 1
                , N.traps = deleteBy ((==) `on` Trap.trigger) res n'.traps
                }
            run user res
            void $ runAndRemoveIf ((== OnResurrected) . Trap.trigger) user slot
            return True
        Nothing -> do
            Hook.trigger OnDeath n
            runAndRemoveIf (isOnDeath . Trap.trigger) user slot
  where
    resurrectTrap
      | n `is` Plague = Nothing
      | otherwise     = find ((== Resurrect) . Trap.trigger) traps
    alive = N.alive n
    isOnDeath OnDeath   = True
    isOnDeath OnBreak{} = True
    isOnDeath _ = False

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

turnNegatives :: ∀ a b. (Parity a, Parity b) => a -> b -> EnumSet Trigger.Negative
turnNegatives player n
  | Parity.allied player n = sameTurn
  | otherwise              = allNegatives \\ sameTurn
  where
    allNegatives = setFromList [minBound..maxBound]
    sameTurn     = filterSet Trigger.duringSameTurn allNegatives

-- | Returns 'OnNoAction' 'Trap.Trap's.
getTurnNot :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
           => Player
           -> Ninja -- ^ 'N.flags' owner.
           -> [m ()]
getTurnNot player n
  | null negatives = []
  | otherwise      = run n.slot <$> filter ((∈ triggers) . Trap.trigger) n.traps
  where
    negatives = turnNegatives player n \\ n.negatives
    triggers  = Trigger.fromNegative <$> toList negatives

-- | Processes and runs all 'Trap.Trap's at the end of a turn.
runTurn :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m)
        => Vector Ninja -> m ()
runTurn ninjas = do
    Game{playing = player} <- P.game
    ninjas' <- P.ninjas
    mapM_ sequence_ $ zipWith (getTurnPer player) ninjas ninjas'
    mapM_ sequence_ $ getTurnNot player <$> ninjas'
    P.modifyAll $ clearNegatives player
  where
    clearNegatives player n =
        n { N.negatives = n.negatives `difference` turnNegatives player n }

runExpirations :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => m ()
runExpirations = mapM_ expire =<< P.ninjas
  where
    expire n@Ninja{slot} = void $ runAndRemoveIf (Trap.isExpiring n) slot slot

-- | Trap engine.
apply :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Duration -> Text -> Trigger
         -> IntRunConstraint () -> m ()
apply direction classes unthrottled name trigger f = void $ runMaybeT do
    context@Context{new, target} <- P.context
    nUser <- P.nUser
    guard . not $ isCounter && nUser `is` Disable Counters
    dur   <- if not new || isChanneled then return unthrottled else
                hoistMaybe $ throttle nUser
    P.modify target $ Ninjas.addTrap context
        $ makeTrap context direction classes dur name trigger f
  where
    isChanneled = setFromList [Continues, Controlled] `intersects` classes
    isCounter = Trigger.isCounter trigger
    throttle n
      | isCounter = Duration.throttle (Effects.throttleCounters n) unthrottled
      | otherwise = Just unthrottled

makeTrap :: Context -> Trap.Direction -> EnumSet Class -> Duration
         -> Text -> Trigger -> IntRunConstraint () -> Trap
makeTrap ctx@Context { continues
                     , new
                     , skill
                     , user
                     } direction classes dur name trigger f = Trap
    { trigger
    , direction
    , skill   = skill'
    , user
    , name    = Skill.provideName skill name
    , effect  = \i -> To context $ f i
    , classes = classes'
    , dur
    }
  where
    setContinues
      | continues && dur <= 1 = insertSet Continues
      | continues || new      = deleteSet Continues
      | otherwise             = deleteSet Continues . deleteSet Invisible
    baseClasses = classes ++ (skill.classes `intersection` Class.inherited)
    extra = insertIf (Hidden ∈ baseClasses) Unremovable
          . insertSet Nonstacking
          . setContinues
          $ getClasses trigger
    classes' = baseClasses ++ extra
    skill'   = skill { Skill.classes = classes'
                     , Skill.require = mempty
                     }
    context  = ctx { Context.skill     = skill'
                   , Context.continues = False
                   , Context.new       = False
                   }
