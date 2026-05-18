-- | Action processing. The core of the game engine.
module Game.Action
  ( wrap
  , act
  , run, addChannels
  , chooseTargets, filterCounters
  , interruptions
  ) where

import ClassyPrelude hiding ((<|))

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bits (setBit, testBit)
import Data.Enum.Set (EnumSet, AsEnumSet(..))

import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import qualified Game.Engine.Trigger as Trigger
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill(Skill), Target(..))
import qualified Game.Model.Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((!!), (∈), (∉), intersects)

-- | Conditions that have already affected an action in progress.
-- Permits passive effects to bypass steps in the process and prevents infinite
-- recursion of 'Reflect's, 'Redirect's, etc.
data Affected
    = Redirected
    | Reflected
    | Targeted
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance AsEnumSet Affected

-- | Processes an action before it can be applied to the game. This is where
-- invincibility, usability, reflects, etc. all come into play.
-- If an action is applied directly instead of passing it to this function,
-- its exact effects will occur and nothing else.
wrap :: ∀ m. (MonadPlay m, MonadRandom m) => m () -> m ()
wrap = wrap' mempty

wrap' :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Affected -> m () -> m ()
wrap' affected f = void $ runMaybeT do
    Context{new, target, user, skill = skill@Skill{classes}} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget
    ninjas  <- P.ninjas

    guard $ Bypassing ∈ classes || not (nTarget `is` Nullify)

    guard $ Targeted ∈ affected -- already checked Requirement.targetable
            || Requirement.targetable skill nUser nTarget

    guard $ user == target
            || Targeted ∉ affected
            || N.alive nTarget
            || Necromancy ∈ classes

    let harm    = not $ Parity.allied user target
        allow x = harm && x ∉ affected

    let finish = do
            f
            when (allow Reflected)
                . P.withTargets (Effects.share nTarget)
                $ wrap' (insertSet Reflected affected) f

    lift if not new then
        f

    else if nUser `is` AntiCounter || nTarget `is` Uncounter then
        finish

    else
        fromMaybe finish
            $ do
              guard $ allow Redirected && Unreflectable ∉ classes
              t <- Trigger.redirect nTarget
              return . P.withTarget t $ wrap' (insertSet Redirected affected) f
          <|> do
              guard $ allow Reflected && Unreflectable ∉ classes
                      && Effects.reflect classes nTarget
              return do
                  P.trigger target [OnReflect]
                  P.with Context.reflect
                    $ wrap' (insertSet Reflected affected) f

    P.zipWith Traps.broken ninjas

-- | Transforms @Target@s into @Slot@s.
-- 'REnemy', 'RAlly', and 'RXAlly' targets are chosen at random.
chooseTargets :: ∀ m. (MonadPlay m, MonadRandom m)
              => [Runnable Target] -> m [[Runnable Slot]]
chooseTargets targets = do
    Context{user, skill = skill@Skill{require}} <- P.context
    nUser  <- P.nUser
    ninjas <- P.ninjas
    forM targets \target -> do
        choices <- chooseTarget $ Runnable.target target
        return [ target { Runnable.target = t }
                   | t <- choices
                   , let nTarget = ninjas !! Slot.toInt t
                   , Requirement.succeed require user nTarget
                   , Requirement.targetable skill nUser nTarget
                   ]

fromContext :: ∀ m a. MonadPlay m => (Context -> a) -> m a
fromContext f = f <$> P.context

chooseRandomTarget :: ∀ m. MonadRandom m => [Slot] -> m [Slot]
chooseRandomTarget slots = maybeToList <$> R.choose slots

-- | Transforms a @Target@ into @Slot@s.
-- 'REnemy', 'RAlly', and 'RXAlly' targets are chosen at random.
chooseTarget :: ∀ m. (MonadPlay m, MonadRandom m) => Target -> m [Slot]
chooseTarget Self = fromContext \Context{user} ->
    singleton user

chooseTarget Ally = fromContext \Context{target, user} ->
    [target | Parity.allied user target]

chooseTarget XAlly = fromContext \Context{target, user} ->
    [target | user /= target && Parity.allied user target]

chooseTarget Allies = fromContext \Context{user} ->
    Slot.allies user

chooseTarget RAlly = chooseRandomTarget =<< chooseTarget Allies

chooseTarget XAllies = fromContext \Context{user} ->
    delete user $ Slot.allies user

chooseTarget RXAlly = chooseRandomTarget =<< chooseTarget XAllies

chooseTarget Enemy = fromContext \Context{target, user} ->
    [target | not $ Parity.allied user target]

chooseTarget Enemies = fromContext \Context{user} ->
    Slot.enemies user

chooseTarget XEnemies = fromContext \Context{target, user} ->
    delete target $ Slot.enemies user

chooseTarget REnemy = chooseRandomTarget =<< chooseTarget Enemies

chooseTarget Everyone = return Slot.all

-- | Directs an effect tuple in a 'Skill' to a target. Uses 'wrap' internally.
targetEffect :: ∀ m. (MonadPlay m, MonadRandom m)
             => EnumSet Affected -> m () -> m ()
targetEffect affected f = do
    Context{target, user, skill = Skill{classes}} <- P.context

    if user == target then
        f

    else if Parity.allied user target then do
        wrap' affected f
        P.trigger user [OnHelp]
        P.trigger target [OnHelped]

    else do
        wrap' affected f
        P.trigger user [OnHarm]
        P.trigger target $ OnHarmed <$> toList classes

-- | Handles effects in a 'Skill'. Uses 'targetEffect' internally.
run :: ∀ m. (MonadPlay m, MonadRandom m) => [[Runnable Slot]] -> m ()
run = run' mempty

run' :: ∀ m. (MonadPlay m, MonadRandom m)
        => EnumSet Affected -> [[Runnable Slot]] -> m ()
run' affected xs = do
    Context{skill} <- P.context
    let local t context = context { Context.skill = skill, Context.target = t }
        exec (To t r)   = P.with (local t) $ targetEffect affected r
    traverse_ (traverse_ exec) xs

-- | If 'Skill.dur' is long enough to last for multiple turns, the 'Skill'
-- is added to 'N.channels'.
-- Uses 'Ninjas.addChannels' internally.
addChannels :: ∀ m. MonadPlay m => m ()
addChannels = do
    Context{skill, target, user} <- P.context
    P.modify user $ Ninjas.addChannels skill target

-- | Filters a list of targets to those capable of countering a skill.
filterCounters :: [[Runnable Slot]] -- ^ Effects of the skill to be countered.
               -> [Ninja] -> [Ninja]
filterCounters slots = filter $ testBit targetSet . Slot.toInt . N.slot
  where
    targetSet = foldl' go (0 :: Word8) $ join slots
    go x      = setBit x . Slot.toInt . Runnable.target

-- | Performs an action, passing its effects to 'wrap' and activating any
-- corresponding 'Trap.Trap's once it occurs.
act :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Context -> m ()
act ctx@Context{user, new, skill} = void $ runMaybeT do
    let Skill{charges, classes, dur, effects, require, start} = skill
    nUser      <- P.ninja user
    Game{chakra} <- P.game
    initial    <- P.ninjas

    guard $ N.alive nUser && require /= Unusable

    lift $ P.withContext ctx do
        if not new then
            P.withContinues $ run' (singletonSet Targeted)
              =<< chooseTargets effects
        else do
            P.modify user \n -> n { N.lastSkill = Just skill }
            P.trigger user $ OnAction <$> toList classes
            when (charges > 0)
                . P.modify user $ Cooldown.spendCharge skill
            startEfs   <- chooseTargets start
            contEfs    <- chooseTargets effects
            let bothEfs = startEfs ++ contEfs

            countering  <- filterCounters bothEfs . toList <$> P.enemies user
            traverse_ Trigger.absorb countering
            let counters = Trigger.userCounters (not $ null countering)
                           user classes nUser ++
                           (Trigger.targetCounters user classes =<< countering)
            if not $ Uncounterable ∈ classes
                  || nUser `is` AntiCounter
                  || null counters
            then do
                let countered = N.slot <$> countering
                    uncounter n
                      | slot == user     = Trigger.userUncounter classes n
                      | slot ∈ countered = Trigger.targetUncounter classes n
                      | otherwise        = n
                      where
                        slot = N.slot n
                P.modifyAll uncounter
                sequence_ counters
            else case dur of
                Instant -> run' (singletonSet Targeted) bothEfs
                _       -> do
                    run' (singletonSet Targeted) startEfs
                    P.withContinues $ run' (singletonSet Targeted) contEfs
                    addChannels
            P.modify user \n -> n { N.acted = True }
            when new
                . P.modify user $ Cooldown.update skill
        P.uncopied do
            Hook.action skill initial =<< P.ninjas
            Game{chakra = chakra'} <- P.game
            Hook.chakra skill chakra chakra'
        
        traverse_ (sequence_ . Traps.get user) =<< P.ninjas

        P.modifyAll $ unreflect . \n -> n { N.triggers = mempty }
        breakControls
  where
    unreflect n@Ninja{triggers}
      | OnReflect ∈ triggers = Ninjas.modifyStatuses
                               (Status.removeEffect Reflect) n
      | otherwise            = n

-- | Effects to run when a channeled skill is canceled.
interruptions :: Skill -> [Runnable Target]
interruptions Skill{interrupt, name} = To Enemy clear : To Ally clear : interrupt
  where
    clear :: ∀ m. MonadPlay m => m ()
    clear = P.fromUser $ Ninjas.clear name

-- | True for all targets except 'REnemy', 'RAlly', and 'RXAlly'.
nonRandom :: Target -> Bool
nonRandom RAlly  = False
nonRandom RXAlly = False
nonRandom REnemy = False
nonRandom _      = True

-- | Ends all Control channels without valid targets.
-- For example, if a Control skill targets an enemy, the channel will end
-- if the target becomes invulnerable or dies.
breakControls :: ∀ m. (MonadGame m, MonadRandom m) => m ()
breakControls = traverse_ breakN =<< P.ninjas
  where
    breakN n@Ninja{channels, newChans, slot} = traverse_
        (breakControl slot $ Effects.stun n)
        $ newChans ++ channels

breakControl :: ∀ m. (MonadGame m, MonadRandom m)
             => Slot -> EnumSet Class -> Channel -> m ()
breakControl user stuns Channel { dur = Control{}
                                , skill = skill@Skill{name, classes, effects}
                                , target
                                } =
    P.withContext Context
        { skill
        , user
        , target
        , new = False
        , continues = False
        } $ guardBreak $ do
            interruptTargets <- chooseTargets $ interruptions skill
            run interruptTargets
            P.modify user $ Ninjas.cancelChannel name
  where
    targets = filter (nonRandom . Runnable.target) effects
    guardBreak
        | stuns `intersects` classes = id
        | otherwise = whenM $ any null <$> chooseTargets targets

breakControl _ _ _ = return ()
