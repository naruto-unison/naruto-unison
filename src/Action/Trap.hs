-- | Actions that characters can use to affect 'Trap's.
module Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , onBreak, onBreak'
  , removeTrap
  , delay
  ) where
import ClassyPrelude.Yesod
import qualified Data.List as List
import           Data.Sequence ((|>))

import           Core.Util ((∈))
import qualified Class.Play as P
import           Class.Play (Play(..), PlayConstraint, PlayT)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Delay as Delay
import           Model.Duration (Duration(..), Turns, incr, sync)
import           Model.Effect (Effect(..))
import qualified Model.Copy as Copy
import           Model.Copy (Copying(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Model.Skill as Skill
import qualified Model.Trap as Trap
import           Model.Trap (Trigger(..))
import qualified Engine.Effects as Effects
import qualified Engine.Execute as Execute
import           Engine.Execute (Affected(..))

-- | Adds a 'Trap' to 'Ninja.traps' that targets the person it was used on.
trap :: ∀ m. PlayT m => Turns -> Trigger -> PlayConstraint () -> m ()
trap = trapWith Trap.To []
-- | 'Hidden' 'trap'.
trap' :: ∀ m. PlayT m => Turns -> Trigger -> PlayConstraint () -> m ()
trap' = trapWith Trap.To [Hidden]

-- | Adds a 'Trap' to 'Ninja.traps' that targets the person who triggers it.
trapFrom :: ∀ m. PlayT m => Turns -> Trigger -> PlayConstraint () -> m ()
trapFrom = trapWith Trap.From []
-- | 'Hidden' 'trapFrom'.
trapFrom' :: ∀ m. PlayT m => Turns -> Trigger -> PlayConstraint () -> m ()
trapFrom' = trapWith Trap.From [Hidden]

-- | Adds a 'Trap' to 'Ninja.traps' with an effect that depends on a number
-- accumulated while the trap is in play and tracked with its 'Trap.tracker'.
trapPer  :: ∀ m. PlayT m => Turns -> Trigger -> (Int -> PlayConstraint ()) 
         -> m ()
trapPer  = trapFull Trap.Per []
-- | 'Hidden' 'trapPer'.
trapPer' :: ∀ m. PlayT m => Turns -> Trigger -> (Int -> PlayConstraint ()) 
         -> m ()
trapPer' = trapFull Trap.Per [Hidden]

-- | Adds an 'OnBreak' 'Trap' for the 'Skill' used to 'Ninja.traps'.
-- 'OnBreak' traps are triggered when a 'Defense' with the same 'Defense.name'
-- is broken.
onBreak :: ∀ m. PlayT m => PlayConstraint () -> m ()
onBreak f = do
    name    <- Skill.name <$> P.skill
    user    <- P.user
    nTarget <- P.nTarget
    when (Ninja.hasDefense name user nTarget) $
        trapFrom' 0 (OnBreak name) f

-- | Default 'onBreak': remove 'Status'es and 'Channel's that match
-- 'Defense.name'. This is useful for 'Defense's that apply an effect or empower
-- some action while active.
onBreak' :: ∀ m. PlayT m => m ()
onBreak' = do
    user <- P.user
    name <- Skill.name <$> P.skill
    onBreak do
        P.modify $
            Game.alter (Ninja.clear name user <$>) .
            Game.adjust user (Ninja.cancelChannel name)

-- | Adds a 'Trap' to 'Ninja.traps'.
trapWith :: ∀ m. PlayT m => Trap.Direction -> [Class] -> Turns -> Trigger
         -> PlayConstraint () -> m ()
trapWith trapType clas dur tr f = trapFull trapType clas dur tr (const f)

-- | Trap engine.
trapFull :: ∀ m. PlayT m => Trap.Direction -> [Class] -> Turns 
         -> Trap.Trigger -> (Int -> PlayConstraint ()) -> m ()
trapFull direction classes (Duration -> dur) trigger f = do
    skill   <- P.skill
    user    <- P.user
    target  <- P.target
    nUser   <- P.nUser
    nTarget <- P.nTarget
    let trapUser = Copy.source skill user
        ctx      = Context.Context { Context.skill   = skill
                                    , Context.user   = trapUser
                                    , Context.target = case direction of
                                          Trap.From -> user
                                          _         -> target
                                    }
        newTrap  = Trap.Trap
            { Trap.trigger   = trigger
            , Trap.direction = direction
            , Trap.name      = Skill.name skill
            , Trap.desc      = Skill.desc skill
            , Trap.user      = trapUser
            , Trap.effect    = \i -> (ctx, Play $ Execute.wrap [Trapped] $ f i)
            , Trap.classes   = List.nub $
                               classes ++ (invis <$> Skill.classes skill)
            , Trap.tracker   = 0
            , Trap.dur       = Copy.maxDur (Skill.copying skill) . incr . 
                               (+ 2 * Effects.throttle throttled nUser) $
                               sync dur
            }
    unless (newTrap ∈ Ninja.traps nTarget) . P.modify . Game.adjust target $
        \n -> n { Ninja.traps = Ninja.traps n |> newTrap }
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
delay :: ∀ m. PlayT m => Turns -> PlayConstraint () -> m ()
delay (Duration -> dur) f = do
    context  <- P.context
    let skill = Context.skill context
        user  = Context.user context
        del   = Delay.Delay
                { Delay.skill  = skill
                , Delay.user   = user
                , Delay.effect = (context, Play $ Execute.wrap [Delayed] f)
                , Delay.dur    = dur'
                }
    unless (past $ Skill.copying skill) $ P.modify \game ->
        game { Game.delays = del : Game.delays game }
  where
    dur' = incr $ sync dur
    past (Shallow _ d) = dur' > d
    past (Deep    _ d) = dur' > d
    past NotCopied     = False

-- | Removes 'Ninja.traps' with matching 'Trap.name'. Uses 'Ninja.clearTrap'
-- internally.
removeTrap :: ∀ m. PlayT m => Text -> m ()
removeTrap name = do
    skill  <- P.skill
    user   <- P.user
    target <- P.target
    P.modify . Game.adjust target .
        Ninja.clearTrap name $ Copy.source skill user