-- | Actions that characters can use to affect 'Trap.Trap's.
module Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , onBreak, onBreak'
  , removeTrap
  , delay
  ) where
import ClassyPrelude

import Data.Enum.Set.Class (EnumSet)

import           Core.Util ((∈))
import qualified Class.Play as P
import           Class.Play (MonadPlay)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import           Model.Context (Context(Context))
import qualified Model.Delay as Delay
import           Model.Delay (Delay(Delay))
import           Model.Duration (Duration(..), Turns, incr, sync)
import           Model.Effect (Effect(..))
import qualified Model.Copy as Copy
import           Model.Copy (Copying(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Runnable as Runnable
import           Model.Runnable (RunConstraint)
import qualified Model.Skill as Skill
import qualified Model.Trap as Trap
import           Model.Trap (Trap(Trap), Trigger(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

-- | Adds a 'Trap.Trap' to 'Ninja.traps' that targets the person it was used on.
trap :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trap = trapWith Trap.Toward mempty
-- | 'Hidden' 'trap'.
trap' :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trap' = trapWith Trap.Toward $ singletonSet Hidden

-- | Adds a 'Trap.Trap' to 'Ninja.traps' that targets the person who triggers it.
trapFrom :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trapFrom = trapWith Trap.From mempty
-- | 'Hidden' 'trapFrom'.
trapFrom' :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trapFrom' = trapWith Trap.From $ singletonSet Hidden

-- | Adds a 'Trap.Trap' to 'Ninja.traps' with an effect that depends on a number
-- accumulated while the trap is in play and tracked with its 'Trap.tracker'.
trapPer  :: ∀ m. MonadPlay m
         => Turns -> Trigger -> (Int -> RunConstraint ()) -> m ()
trapPer  = trapFull Trap.Per mempty
-- | 'Hidden' 'trapPer'.
trapPer' :: ∀ m. MonadPlay m
         => Turns -> Trigger -> (Int -> RunConstraint ()) -> m ()
trapPer' = trapFull Trap.Per $ singletonSet Hidden

-- | Adds an 'OnBreak' 'Trap.Trap' for the used 'Skill.Skill' to 'Ninja.traps'.
-- 'OnBreak' traps are triggered when a 'Defense.Defense' with the same
-- 'Defense.name' is broken.
onBreak :: ∀ m. MonadPlay m => RunConstraint () -> m ()
onBreak f = do
    name    <- Skill.name <$> P.skill
    user    <- P.user
    nTarget <- P.nTarget
    when (Ninja.hasDefense name user nTarget) $
        trapFrom' 0 (OnBreak name) f

-- | Default 'onBreak': remove 'Model.Status.Status'es and
-- 'Model.Channel.Channel's that match 'Defense.name'. This is useful for '
-- Defense.Defense's that apply an effect or empower some action while active.
onBreak' :: ∀ m. MonadPlay m => m ()
onBreak' = do
    user <- P.user
    name <- Skill.name <$> P.skill
    onBreak do
        P.modify user $ Ninja.cancelChannel name
        P.modifyAll $ Ninja.clear name user

-- | Adds a 'Trap.Trap' to 'Ninja.traps'.
trapWith :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Turns -> Trigger
         -> RunConstraint () -> m ()
trapWith trapType clas dur tr f = trapFull trapType clas dur tr (const f)

-- | Trap engine.
trapFull :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Turns -> Trap.Trigger
         -> (Int -> RunConstraint ()) -> m ()
trapFull direction classes (Duration -> dur) trigger f = do
    skill   <- P.skill
    user    <- P.user
    target  <- P.target
    nUser   <- P.nUser
    nTarget <- P.nTarget
    let trapUser = Copy.source skill user
        ctx      = Context { Context.skill   = skill
                           , Context.user   = trapUser
                           , Context.target = case direction of
                                 Trap.From -> user
                                 _         -> target
                           , Context.new    = False
                           }
        newTrap  = Trap
            { Trap.trigger   = trigger
            , Trap.direction = direction
            , Trap.name      = Skill.name skill
            , Trap.desc      = Skill.desc skill
            , Trap.user      = trapUser
            , Trap.effect    = \i -> Runnable.To
                { Runnable.target = ctx
                , Runnable.run    = Execute.wrap (singletonSet Trapped) $ f i
                }
            , Trap.classes   = classes ++ (invis `omap` Skill.classes skill)
            , Trap.tracker   = 0
            , Trap.dur       = Copy.maxDur (Skill.copying skill) . incr .
                               (+ 2 * Effects.throttle throttled nUser) $
                               sync dur
            }
    unless (newTrap ∈ Ninja.traps nTarget) $ P.modify target
        \n -> n { Ninja.traps = newTrap : Ninja.traps n }
  where
    throttled = case trigger of
        OnCounter c  -> [Counter c]
        OnCounterAll -> [CounterAll All]
        OnReflectAll -> [ReflectAll]
        _            -> []
    invis InvisibleTraps = Invisible
    invis x              = x

-- | Saves an effect to a 'Delay.Delay', which is stored in 'Game.delays' and
-- triggered when it expires.
delay :: ∀ m. MonadPlay m => Turns -> RunConstraint () -> m ()
delay (Duration -> dur) f = do
    context  <- P.context
    let skill = Context.skill context
        user  = Context.user context
        del   = Delay
                { Delay.skill  = skill
                , Delay.user   = user
                , Delay.effect = Runnable.To
                    { Runnable.target = context
                    , Runnable.run    = Execute.wrap (singletonSet Delayed) f
                    }
                , Delay.dur    = dur'
                }
    unless (past $ Skill.copying skill) $ P.alter \game ->
        game { Game.delays = del : Game.delays game }
  where
    dur' = incr $ sync dur
    past (Shallow _ d) = dur' > d
    past (Deep    _ d) = dur' > d
    past NotCopied     = False

-- | Removes 'Ninja.traps' with matching 'Trap.name'. Uses 'Ninja.clearTrap'
-- internally.
removeTrap :: ∀ m. MonadPlay m => Text -> m ()
removeTrap name = do
    skill  <- P.skill
    user   <- P.user
    target <- P.target
    P.modify target . Ninja.clearTrap name $ Copy.source skill user
