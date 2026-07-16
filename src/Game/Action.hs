-- | Action processing. The core of the game engine.
module Game.Action
  ( wrap
  , act
  , run, runTargeted
  , chooseTargets
  , runInterruptions
  ) where

import ClassyPrelude hiding ((<|))

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Enum.Set (EnumSet, AsEnumSet(..))

import           Class.Classed (Classed(..))
import           Class.Hook (MonadHook)
import qualified Class.Hook as Hook
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Class.TurnBased (TurnBased)
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Counter as Counter
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Traps as Traps
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Channel as Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context as Context
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import           Game.Model.ID (HasID, ID(ID))
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as N
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill(Skill), Target(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉), intersects)

-- | Conditions that have already affected an action in progress.
-- Permits passive effects to bypass steps in the process and prevents infinite
-- recursion of 'Reflect's, 'Redirect's, etc.
data Affected
    = Redirected
    | Reflected
    | Targeted
    deriving (Bounded, Enum, Eq, Ord, Show)

instance AsEnumSet Affected

-- | Processes an action before it can be applied to the game. This is where
-- invincibility, usability, reflects, etc. all come into play.
-- If an action is applied directly instead of passing it to this function,
-- its exact effects will occur and nothing else.
wrap :: ∀ m. MonadPlay m => m () -> m ()
wrap = wrap' mempty

wrap' :: ∀ m. MonadPlay m => EnumSet Affected -> m () -> m ()
wrap' affected f = void $ runMaybeT do
    Context{new, target, user, skill = skill@Skill{classes}} <- P.context
    nUser   <- P.nUser
    nTarget <- P.nTarget

    let allied = Parity.allied user target

    guard $ allied || Bypassing ∈ classes || not (nTarget `is` Nullify)

    guard $ Targeted ∈ affected -- already checked Requirement.targetable
            || Requirement.targetable skill nUser nTarget

    guard $ user == target
            || Targeted ∈ affected
            || N.alive nTarget
            || Necromancy ∈ classes

    let allow x = not allied && x ∉ affected
        finish  = do
            f
            when (allow Reflected)
                . P.withTargets (Effects.share nTarget)
                $ wrap' (insertSet Reflected affected) f

    lift if not new then
        f

    else if nUser `is` AntiCounter || nTarget `is` Uncounter then
        finish

    else fromMaybe finish
        $ do
            guard $ allow Redirected && Unreflectable ∉ classes
            t <- Effects.redirect nTarget
            return . P.withTarget t $ wrap' (insertSet Redirected affected) f
      <|> do
            guard $ allow Reflected && Unreflectable ∉ classes
                    && Effects.reflect classes nTarget
            return do
                P.trigger target [OnReflect]
                P.with Context.reflect $ wrap' (insertSet Reflected affected) f

-- | Transforms @Target@s into @Slot@s.
-- 'REnemy', 'RAlly', and 'RXAlly' targets are chosen at random.
targeted :: ∀ m. MonadPlay m
         => [Runnable Target] -> m [[Runnable Slot]]
targeted targets = do
    Context{skill} <- P.context
    nUser <- P.nUser
    ninjas <- P.ninjas
    forM targets \(To target runner) -> do
        choices <- chooseTargets target
        return [ To t runner | t <- choices,
                               Requirement.targetable skill nUser
                               $ ninjas `Slot.index` t ]

fromContext :: ∀ a m. MonadPlay m => (Context -> a) -> m a
fromContext f = f <$> P.context

chooseRandomTarget :: ∀ m. MonadPlay m => [Slot] -> m [Slot]
chooseRandomTarget slots = do
    Context{skill, user} <- P.context
    ninjas <- P.ninjas
    let nUser = ninjas `Slot.index` user
    let isValidSlot slot = Requirement.targetable skill nUser
                         $ ninjas `Slot.index` slot
    target <- R.choose $ filter isValidSlot slots
    return $ maybeToList target

-- | Transforms a @Target@ into @Slot@s.
-- 'REnemy', 'RAlly', and 'RXAlly' targets are chosen at random.
chooseTargets :: ∀ m. MonadPlay m => Target -> m [Slot]
chooseTargets Self = fromContext \Context{user} ->
    singleton user

chooseTargets Ally = fromContext \Context{target, user} ->
    [target | Parity.allied user target]

chooseTargets XAlly = fromContext \Context{target, user} ->
    [target | user /= target && Parity.allied user target]

chooseTargets Allies = fromContext \Context{user} ->
    Slot.allies user

chooseTargets RAlly = chooseRandomTarget =<< chooseTargets Allies

chooseTargets XAllies = fromContext \Context{user} ->
    delete user $ Slot.allies user

chooseTargets RXAlly = chooseRandomTarget =<< chooseTargets XAllies

chooseTargets Enemy = fromContext \Context{target, user} ->
    [target | not $ Parity.allied user target]

chooseTargets Enemies = fromContext \Context{user} ->
    Slot.enemies user

chooseTargets XEnemies = fromContext \Context{target, user} ->
    delete target $ Slot.enemies user

chooseTargets REnemy = chooseRandomTarget =<< chooseTargets Enemies

chooseTargets RXEnemy = chooseRandomTarget =<< chooseTargets XEnemies

chooseTargets Everyone = return Slot.all

-- | Directs an effect tuple in a 'Skill' to a target. Uses 'wrap' internally.
targetEffect :: ∀ m. MonadPlay m
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
run :: ∀ m. MonadPlay m => [[Runnable Slot]] -> m ()
run = run' mempty

run' :: ∀ m. MonadPlay m
        => EnumSet Affected -> [[Runnable Slot]] -> m ()
run' affected xs = do
    Context{skill} <- P.context
    let local t context = context { Context.skill = skill, Context.target = t }
        exec (To t r)   = P.with (local t) $ targetEffect affected r
    mapM_ (mapM_ exec) xs

runTargeted :: ∀ m. MonadPlay m => [Runnable Target] -> m ()
runTargeted effects = run =<< targeted effects

-- | Performs an action, passing its effects to 'wrap' and activating any
-- corresponding 'Trap.Trap's once it occurs.
act :: ∀ m. (MonadGame m, MonadHook m, MonadRandom m) => Context -> m ()
act context@Context { user
                    , new
                    , target
                    , skill = skill@Skill{classes}
                    } = void $ runMaybeT do
    Game{chakra} <- P.game
    nUser   <- P.ninja user
    initial <- P.ninjas

    guard $ N.alive nUser && Unusable ∉ skill.require

    lift $ P.withContext context do
        if not new then do
            mainEffects <- targeted $ skill.always ++ skill.effects
            P.withContinues $ run' (singleton Targeted) mainEffects
        else do
            P.modify user \n -> n { N.lastSkill = Just skill }
            P.trigger user $ OnAction <$> toList classes
            when (Skill.hasCharges skill)
                . P.modify user $ Ninjas.spendCharge skill
            startEffects <- targeted skill.start
            mainEffects  <- targeted $ skill.always ++ skill.effects

            let allEffects = startEffects ++ mainEffects

            countering   <- Counter.filterCounters allEffects . toList
                            <$> P.enemies user
            forM_ countering \n ->
                when (n `is` Absorb) $ P.alterGame $ Game.addChakra n skill.cost

            let counters = Counter.userCounters (not $ null countering)
                           user classes nUser ++
                           concatMap (Counter.targetCounters user classes)
                           countering
            if not $ Uncounterable ∈ classes
                  || nUser `is` AntiCounter
                  || null counters
            then do
                let countered = N.slot <$> countering
                    uncounter n
                      | n.slot == user     = Counter.userUncounter classes n
                      | n.slot ∈ countered = Counter.targetUncounter classes n
                      | otherwise          = n
                P.modifyAll uncounter
                sequence_ counters
            else case skill.dur of
                Instant -> run' (singleton Targeted) allEffects
                _       -> do
                    P.modify user $ Ninjas.addChannels skill target
                    run' (singleton Targeted) startEffects
                    P.withContinues $ run' (singleton Targeted) mainEffects
            P.modify user $ Cooldown.update skill
        P.uncopied do
            Hook.action skill initial =<< P.ninjas
            Game{chakra = chakra'} <- P.game
            Hook.chakra skill chakra chakra'

        Traps.runTriggers user
        Traps.runDeaths $ Just user

        P.modifyAll unreflect
        breakControls
  where
    unreflect n
      | OnReflect ∈ n.triggers = Ninjas.modifyStatuses
                                 (Status.removeEffect Reflect) n
      | otherwise              = n

-- | Effects to run when a channeled skill is canceled.
runInterruptions :: ∀ m. (MonadGame m, MonadRandom m) => Slot -> Channel -> m ()
runInterruptions user Channel{dur, target, skill} = do
    P.withContext context $ runTargeted skill.end
    when (Channel.isControl dur)
        $ P.modifyAll removeInterrupted
  where
    context = Context
        { skill
        , user
        , target
        , new = False
        , continues = False
        }
    channelID = ID { user, owner = user, name = skill.name }
    removeInterrupted :: Ninja -> Ninja
    removeInterrupted n
      | hasInterrupted = Ninjas.modifyAll (filter uninterrupted) n
      | otherwise      = n
      where
        hasInterrupted = any interrupted n.statuses
                      || any interrupted n.traps
                      || any interrupted n.defense
                      || any interrupted n.barrier
    interrupted :: ∀ a. (Classed a, HasID a, TurnBased a) => a -> Bool
    interrupted a = Controlled ∈ getClasses a && ID.from a == channelID
    uninterrupted :: ∀ a. (Classed a, HasID a, TurnBased a) => a -> Bool
    uninterrupted = not . interrupted

-- | Ends all Control channels without valid targets.
-- For example, if a Control skill targets an enemy, the channel will end
-- if the target becomes invulnerable or dies.
breakControls :: ∀ m. (MonadGame m, MonadRandom m) => m ()
breakControls = mapM_ breakN =<< P.ninjas
  where
    breakN n = mapM_ (breakControl n.slot $ Effects.stun n) n.channels

breakControl :: ∀ m. (MonadGame m, MonadRandom m)
             => Slot -> EnumSet Class -> Channel -> m ()
breakControl user stuns chan@Channel{dur = Control{}, skill, target} =
    P.withContext context $ ifBroken do
        P.modify user $ Ninjas.cancelChannel $ ID.from skill
        runInterruptions user chan
  where
    context = Context
        { skill
        , user
        , target
        , new = False
        , continues = False
        }
    breakable :: EnumSet Target
    breakable = setFromList
        [ Ally
        , Allies
        , XAlly
        , XAllies
        , Enemy
        , Enemies
        , XEnemies
        , Everyone
        ]
    targets = filter ((∈ breakable) . Runnable.target) skill.effects
    ifBroken
        | stuns `intersects` skill.classes = id
        | null targets = const $ return ()
        | otherwise    = whenM $ all null <$> targeted targets

breakControl _ _ _ = return ()
