module Action.Trap
  ( trap, trap', trapFrom, trapFrom', trapPer, trapPer', trapWith
  , onBreak, onBreak'
  , delay
  , removeTrap
  ) where
import ClassyPrelude.Yesod
import qualified Data.List as List
import           Data.Sequence ((|>))

import           Core.Util ((∈), incr, sync)
import qualified Class.Play as P
import           Class.Play (Play(..), PlayConstraint, PlayT)
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Delay as Delay
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

trapFull :: ∀ m. PlayT m => Trap.Direction -> [Class] -> Int -> Trap.Trigger
         -> (Int -> PlayConstraint ()) -> m ()
trapFull direction classes dur trigger f = do
    skill   <- P.skill
    source  <- P.source
    user    <- P.user
    target  <- P.target
    nSource <- P.nSource
    nTarget <- P.nTarget
    let trapSource = Copy.root skill source
        ctx        = Context.Context { Context.skill  = skill
                                     , Context.source = trapSource
                                     , Context.user   = trapSource
                                     , Context.target = case direction of
                                          Trap.From -> user
                                          _         -> target
                                     }
        newTrap    = Trap.Trap
            { Trap.trigger   = trigger
            , Trap.direction = direction
            , Trap.name      = Skill.name skill
            , Trap.desc      = Skill.desc skill
            , Trap.source    = trapSource
            , Trap.effect    = \i -> (ctx, Play $ Execute.wrap [Trapped] $ f i)
            , Trap.classes   = List.nub $
                               classes ++ (invis <$> Skill.classes skill)
            , Trap.tracker   = 0
            , Trap.dur       = Copy.maxDur (Skill.copying skill) . incr . sync $
                               signum dur * Effects.throttle throttled nSource
                                 + dur
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

trapWith :: ∀ m. PlayT m => Trap.Direction -> [Class] -> Int -> Trigger
         -> PlayConstraint () -> m ()
trapWith trapType clas dur tr f = trapFull trapType clas dur tr (const f)

-- | Adds a 'TrapTo' 'Trap'.
trap :: ∀ m. PlayT m => Int -> Trigger -> PlayConstraint () -> m ()
trap = trapWith Trap.To []
trap' :: ∀ m. PlayT m => Int -> Trigger -> PlayConstraint () -> m ()
trap' = trapWith Trap.To [Hidden]

-- | Adds a 'TrapFrom' 'Trap'.
trapFrom :: ∀ m. PlayT m => Int -> Trigger -> PlayConstraint () -> m ()
trapFrom = trapWith Trap.From []
trapFrom' :: ∀ m. PlayT m => Int -> Trigger -> PlayConstraint () -> m ()
trapFrom' = trapWith Trap.From [Hidden]

-- | Adds a 'TrapPer' 'Trap'.
trapPer  :: ∀ m. PlayT m => Int -> Trigger -> (Int -> PlayConstraint ()) -> m ()
trapPer  = trapFull Trap.Per []
trapPer' :: ∀ m. PlayT m => Int -> Trigger -> (Int -> PlayConstraint ()) -> m ()
trapPer' = trapFull Trap.Per [Hidden]

delay :: ∀ m. PlayT m => Int -> PlayConstraint () -> m ()
delay dur f = do
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

-- | Applies an 'OnBreak' trap for the 'Skill' used.
onBreak :: ∀ m. PlayT m => PlayConstraint () -> m ()
onBreak f = do
    name    <- Skill.name <$> P.skill
    source  <- P.source
    nTarget <- P.nTarget
    when (Ninja.hasDefense name source nTarget) $
        trapFrom' 0 (OnBreak name) f

-- | Default 'onBreak': remove 'Status'es and 'Channel's that match 'label'.
onBreak' :: ∀ m. PlayT m => m ()
onBreak' = do
    source <- P.source
    user   <- P.user
    name   <- Skill.name <$> P.skill
    onBreak do
        P.modify $
            Game.alter (Ninja.clear name source <$>) .
            Game.adjust user (Ninja.cancelChannel name)

-- | 'N.removeTrap'
removeTrap :: ∀ m. PlayT m => Text -> m ()
removeTrap name = do
    skill  <- P.skill
    source <- P.source
    target <- P.target
    P.modify . Game.adjust target .
        Ninja.clearTrap name $ Copy.root skill source
