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
import qualified Game.Model.Channel as Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill, Target(..))
import qualified Game.Model.Skill as Skill
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
    new        <- P.new
    skill      <- P.skill
    user       <- P.user
    target     <- P.target
    nUser      <- P.nUser
    nTarget    <- P.nTarget
    ninjas     <- P.ninjas
    let classes = Skill.classes skill

    guard $ Bypassing ∈ classes || not (nTarget `is` Nullify)

    guard $ Targeted ∈ affected -- already checked Requirement.targetable
            || Requirement.targetable skill nUser nTarget

    guard $ user == target
            || Targeted ∉ affected
            || Ninja.alive nTarget
            || Necromancy ∈ classes

    let harm    = not $ Parity.allied user target
        allow x = harm && x ∉ affected

    let finish = do
            f
            when (allow Reflected) . P.withTargets (Effects.share nTarget) $
                wrap' (insertSet Reflected affected) f

    lift if not new then
        f

    else if nUser `is` AntiCounter || nTarget `is` Uncounter then
        finish

    else
        fromMaybe finish
            $ do
              guard $ allow Redirected && Unreflectable ∉ classes
              t <- Trigger.redirect nTarget
              return .
                  P.withTarget t $ wrap' (insertSet Redirected affected) f
          <|> do
              guard $ allow Reflected && Unreflectable ∉ classes
                      && Effects.reflect classes nTarget
              return do
                  P.trigger target [OnReflect]
                  P.with Context.reflect $
                      wrap' (insertSet Reflected affected) f

    P.zipWith Traps.broken ninjas

-- | Transforms @Target@s into @Slot@s.
-- 'REnemy', 'RAlly', and 'RXAlly' targets are chosen at random.
chooseTargets :: ∀ m. (MonadPlay m, MonadRandom m)
              => [Runnable Target] -> m [[Runnable Slot]]
chooseTargets targets = do
    skill  <- P.skill
    user   <- P.user
    nUser  <- P.nUser
    ninjas <- P.ninjas
    forM targets \target -> do
        choices <- chooseTarget $ Runnable.target target
        return [ target { Runnable.target = t }
                   | t <- choices
                   , let nTarget = ninjas !! Slot.toInt t
                   , Requirement.succeed (Skill.require skill) user nTarget
                   , Requirement.targetable skill nUser nTarget
                   ]

-- | Transforms a @Target@ into @Slot@s.
-- 'REnemy', 'RAlly', and 'RXAlly' targets are chosen at random.
chooseTarget :: ∀ m. (MonadPlay m, MonadRandom m) => Target -> m [Slot]

chooseTarget Self =
    singleton <$> P.user

chooseTarget Ally = do
    user   <- P.user
    target <- P.target
    return [target | Parity.allied user target]

chooseTarget XAlly = do
    user   <- P.user
    target <- P.target
    return [target | user /= target && Parity.allied user target]

chooseTarget Allies =
    Slot.allies <$> P.user

chooseTarget RAlly =
    maybeToList <$> (Slot.allies <$> P.user >>= R.choose)

chooseTarget XAllies = do
    user <- P.user
    return . delete user $ Slot.allies user

chooseTarget RXAlly = do
    user <- P.user
    maybeToList <$> R.choose (delete user $ Slot.allies user)

chooseTarget Enemy = do
    user   <- P.user
    target <- P.target
    return [target | not $ Parity.allied user target]

chooseTarget Enemies =
    Slot.enemies <$> P.user

chooseTarget XEnemies = do
    user   <- P.user
    target <- P.target
    return . delete target $ Slot.enemies user

chooseTarget REnemy =
    maybeToList <$> (Slot.enemies <$> P.user >>= R.choose)

chooseTarget Everyone = return Slot.all

-- | Directs an effect tuple in a 'Skill' to a target. Uses 'wrap' internally.
targetEffect :: ∀ m. (MonadPlay m, MonadRandom m)
             => EnumSet Affected -> m () -> m ()
targetEffect affected f = do
    user   <- P.user
    target <- P.target

    if user == target then
        f

    else if Parity.allied user target then do
        wrap' affected f
        P.trigger user [OnHelp]
        P.trigger target [OnHelped]

    else do
        wrap' affected f
        P.trigger user [OnHarm]
        classes <- Skill.classes <$> P.skill
        P.trigger target $ OnHarmed <$> toList classes

-- | Handles effects in a 'Skill'. Uses 'targetEffect' internally.
run :: ∀ m. (MonadPlay m, MonadRandom m) => [[Runnable Slot]] -> m ()
run = run' mempty

run' :: ∀ m. (MonadPlay m, MonadRandom m)
        => EnumSet Affected -> [[Runnable Slot]] -> m ()
run' affected xs = do
    skill              <- P.skill
    let local t context = context { Context.skill = skill, Context.target = t }
        exec (To t r)   = P.with (local t) $ targetEffect affected r
    traverse_ (traverse_ exec) xs

-- | If 'Skill.dur' is long enough to last for multiple turns, the 'Skill'
-- is added to 'Ninja.channels'.
-- Uses 'Ninjas.addChannels' internally.
addChannels :: ∀ m. MonadPlay m => m ()
addChannels = do
    skill  <- P.skill
    target <- P.target
    user   <- P.user
    P.modify user $ Ninjas.addChannels skill target

-- | Filters a list of targets to those capable of countering a skill.
filterCounters :: [[Runnable Slot]] -- ^ Effects of the skill to be countered.
               -> [Ninja] -> [Ninja]
filterCounters slots = filter $ testBit targetSet . Slot.toInt . Ninja.slot
  where
    targetSet = foldl' go (0 :: Word8) $ join slots
    go x      = setBit x . Slot.toInt . Runnable.target

-- | Performs an action, passing its effects to 'wrap' and activating any
-- corresponding 'Trap.Trap's once it occurs.
act :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Context -> m ()
act ctx@Context { user, skill, new } = void $ runMaybeT do
    nUser      <- P.ninja user
    chakras    <- Game.chakra <$> P.game
    initial    <- P.ninjas
    let classes = Skill.classes skill

    guard $ Ninja.alive nUser && Skill.require skill /= Unusable

    lift $ P.withContext ctx do
        if not new then
            P.withContinues $
            run' (singletonSet Targeted) =<< chooseTargets (Skill.effects skill)
        else do
            P.modify user \n -> n { Ninja.lastSkill = Just skill }
            P.trigger user $ OnAction <$> toList classes
            when (Skill.charges skill > 0) .
                P.modify user $ Cooldown.spendCharge skill
            startEfs   <- chooseTargets $ Skill.start skill
            contEfs    <- chooseTargets $ Skill.effects skill
            let bothEfs = startEfs ++ contEfs

            countering  <- filterCounters bothEfs . toList <$> P.enemies user
            traverse_ Trigger.absorb countering
            let counters = Trigger.userCounters (not $ null countering)
                           user classes nUser ++
                           (Trigger.targetCounters user classes =<< countering)
            if not $
              Uncounterable ∈ classes
              || nUser `is` AntiCounter
              || null counters
            then do
                let countered = Ninja.slot <$> countering
                    uncounter n
                      | slot == user     = Trigger.userUncounter classes n
                      | slot ∈ countered = Trigger.targetUncounter classes n
                      | otherwise        = n
                      where
                        slot = Ninja.slot n
                P.modifyAll uncounter
                sequence_ counters
            else case Skill.dur skill of
                Instant -> run' (singletonSet Targeted) bothEfs
                _       -> do
                    run' (singletonSet Targeted) startEfs
                    P.withContinues $ run' (singletonSet Targeted) contEfs
                    addChannels
            P.modify user \n -> n { Ninja.acted = True }
            when new . P.modify user $ Cooldown.update skill
        P.uncopied do
            Hook.action skill initial =<< P.ninjas
            Hook.chakra skill chakras . Game.chakra =<< P.game
        traverse_ (sequence_ . Traps.get user) =<< P.ninjas

        P.modifyAll $ unreflect . \n -> n { Ninja.triggers = mempty }
        breakControls
  where
    unreflect n
      | OnReflect ∈ Ninja.triggers n =
          Ninjas.modifyStatuses (Status.removeEffect Reflect) n
      | otherwise = n

-- | Effects to run when a channeled skill is canceled.
interruptions :: Skill -> [Runnable Target]
interruptions skill = To Enemy clear : To Ally clear : Skill.interrupt skill
  where
    clear :: ∀ m. MonadPlay m => m ()
    clear = P.fromUser . Ninjas.clear $ Skill.name skill

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
    breakN n = traverse_ (breakControl (Ninja.slot n) $ Effects.stun n) $
               Ninja.newChans n ++ Ninja.channels n

breakControl :: ∀ m. (MonadGame m, MonadRandom m)
             => Slot -> EnumSet Class -> Channel -> m ()
breakControl user stuns Channel{ dur = Control{}, skill, target }
  | stuns `intersects` Skill.classes skill = P.withContext context doBreak
  | otherwise = P.withContext context do
      targets <- chooseTargets . filter (nonRandom . Runnable.target) $
                Skill.effects skill
      when (any null targets) doBreak
  where
    doBreak = do
        interruptTargets <- chooseTargets $ interruptions skill
        run interruptTargets
        P.modify user . Ninjas.cancelChannel $ Skill.name skill
    context = Context { skill, user, target, new = False, continues = False }
breakControl _ _ _ = return ()
