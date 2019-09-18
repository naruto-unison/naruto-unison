-- | Actions that characters can use to affect @Trap@s.
module Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , onBreak, onBreak'
  , removeTrap
  , delay
  ) where
import ClassyPrelude

import Data.Enum.Set.Class (EnumSet)

import           Core.Util ((∈))
import qualified Class.Classed as Classed
import qualified Class.Play as P
import           Class.Play (MonadPlay)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import           Model.Context (Context(Context))
import qualified Model.Delay as Delay
import           Model.Duration (Duration(..), Turns, incr, sync)
import           Model.Effect (Effect(..))
import qualified Model.Copy as Copy
import           Model.Copy (Copying(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Runnable as Runnable
import           Model.Runnable (Runnable(..), RunConstraint)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)
import qualified Model.Trap as Trap
import           Model.Trap (Trap(Trap), Trigger(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))
import qualified Engine.Ninjas as Ninjas

-- | Adds a @Trap@ to 'Ninja.traps' that targets the person it was used on.
trap :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trap = trapWith Trap.Toward mempty
-- | 'Hidden' 'trap'.
trap' :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trap' = trapWith Trap.Toward $ singletonSet Hidden

-- | Adds a @Trap@ to 'Ninja.traps' that targets the person who triggers it.
trapFrom :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trapFrom = trapWith Trap.From mempty
-- | 'Hidden' 'trapFrom'.
trapFrom' :: ∀ m. MonadPlay m => Turns -> Trigger -> RunConstraint () -> m ()
trapFrom' = trapWith Trap.From $ singletonSet Hidden

-- | Adds a @Trap@ to 'Ninja.traps' with an effect that depends on a number
-- accumulated while the trap is in play and tracked with its 'Trap.tracker'.
trapPer  :: ∀ m. MonadPlay m
         => Turns -> Trigger -> (Int -> RunConstraint ()) -> m ()
trapPer  = trapFull Trap.Per mempty
-- | 'Hidden' 'trapPer'.
trapPer' :: ∀ m. MonadPlay m
         => Turns -> Trigger -> (Int -> RunConstraint ()) -> m ()
trapPer' = trapFull Trap.Per $ singletonSet Hidden

-- | Adds an 'OnBreak' @Trap@ for the used 'Skill.Skill' to 'Ninja.traps'.
-- @OnBreak@ traps are triggered when a 'Defense.Defense' with the same
-- 'Defense.name' is broken.
onBreak :: ∀ m. MonadPlay m => RunConstraint () -> m ()
onBreak f = do
    name    <- Skill.name <$> P.skill
    user    <- P.user
    nTarget <- P.nTarget
    when (Ninja.hasDefense name user nTarget) $
        trapFrom' 0 (OnBreak name) do
            f
            user' <- P.user
            P.modify user' . Ninjas.clearTraps $ OnBreak name

-- | Default 'onBreak': remove 'Model.Status.Status'es and
-- 'Model.Channel.Channel's that match 'Defense.name'. This is useful for
-- 'Defense.Defense's that apply an effect or empower some action while active.
onBreak' :: ∀ m. MonadPlay m => m ()
onBreak' = do
    user <- P.user
    name <- Skill.name <$> P.skill
    onBreak do
        P.modify user $ Ninjas.cancelChannel name
        P.modifyAll $ Ninjas.clear name user

-- | Adds a @Trap@ to 'Ninja.traps'.
trapWith :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Turns -> Trigger
         -> RunConstraint () -> m ()
trapWith trapType clas dur tr f = trapFull trapType clas dur tr $ const f

-- | Trap engine.
trapFull :: ∀ m. MonadPlay m
         => Trap.Direction -> EnumSet Class -> Turns -> Trigger
         -> (Int -> RunConstraint ()) -> m ()
trapFull direction classes (Duration -> dur) trigger f = do
    skill   <- P.skill
    target  <- P.target
    nUser   <- P.nUser
    nTarget <- P.nTarget
    let newTrap  = makeTrap skill nUser target
                   direction classes dur trigger f
    unless (newTrap ∈ Ninja.traps nTarget) $ P.modify target \n ->
        n { Ninja.traps = Classed.nonStack newTrap newTrap $ Ninja.traps n }

makeTrap :: Skill -> Ninja -> Slot
         -> Trap.Direction -> EnumSet Class -> Duration -> Trigger
         -> (Int -> RunConstraint ()) -> Trap
makeTrap skill nUser target direction classes dur trigger f = Trap
    { Trap.trigger   = trigger
    , Trap.direction = direction
    , Trap.name      = Skill.name skill
    , Trap.skill     = skill
    , Trap.user      = user
    , Trap.effect    = \i -> Runnable.To
        { Runnable.target = ctx
        , Runnable.run    = Execute.wrap (singletonSet Trapped) $ f i
        }
    , Trap.classes   = classes ++ (invis `omap` Skill.classes skill)
    , Trap.tracker   = 0
    , Trap.dur       = Copy.maxDur (Skill.copying skill) . incr $
                        sync dur + throttled nUser
    }
  where
    user = Ninja.slot nUser
    ctx  = Context { Context.skill  = skill
                   , Context.user   = user
                   , Context.target = target
                   , Context.new    = False
                   }
    throttled n
      | dur == 0               = 0
      | Trap.isCounter trigger = 2 * Effects.throttle [Reflect] n
      | otherwise              = 0
    invis InvisibleTraps = Invisible
    invis x              = x

-- | Saves an effect to a 'Delay.Delay', which is stored in 'Game.delays' and
-- triggered when it expires.
delay :: ∀ m. MonadPlay m => Turns -> RunConstraint () -> m ()
delay 0 _ = return () -- A Delay that lasts forever would be pointless!
delay (Duration -> dur) f = do
    context  <- P.context
    let skill = Context.skill context
        user  = Context.user context
        del   = Delay.new context dur $ Execute.wrap (singletonSet Delayed) f
    unless (past $ Skill.copying skill) $ P.modify user \n ->
        n { Ninja.delays = del : Ninja.delays n }
  where
    dur' = incr $ sync dur
    past (Shallow _ d) = dur' > d
    past (Deep    _ d) = dur' > d
    past NotCopied     = False

-- | Removes 'Ninja.traps' with matching 'Trap.name'.
-- Uses 'Ninjas.clearTrap' internally.
removeTrap :: ∀ m. MonadPlay m => Text -> m ()
removeTrap name = P.fromUser $ Ninjas.clearTrap name
