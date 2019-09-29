-- | Action processing. The core of the game engine.
module Game.Action
  ( wrap
  , Affected(..)
  , act
  , run, addChannels
  , chooseTargets, filterCounters
  , interruptions
  ) where

import ClassyPrelude hiding ((<|))

import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Bits (setBit, testBit)
import           Data.Either (isLeft)
import           Data.Enum.Set.Class (EnumSet, AsEnumSet(..))

import           Util ((!!), (—), (∈), (∉), intersects)
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Game.Model.Act as Act
import           Game.Model.Act (Act)
import qualified Game.Model.Channel as Channel
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Class (Class(..))
import qualified Game.Model.Context as Context
import           Game.Model.Context (Context(Context))
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill, Target(..))
import qualified Game.Model.Slot as Slot
import           Game.Model.Slot (Slot)
import qualified Game.Model.Status as Status
import           Game.Model.Trigger (Trigger(..))
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Engine.Skills as Skills
import qualified Game.Engine.Traps as Traps
import qualified Game.Engine.Trigger as Trigger

-- Conditions that have already affected an action in progress.
-- Permits passive effects to bypass steps in the process and prevents infinite
-- recursion of 'Reflect's, 'Redirect's, etc.
data Affected
    = Channeled
    | Delayed
    | Interrupted
    | Redirected
    | Reflected
    | Swapped
    | Targeted
    | Trapped
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

instance AsEnumSet Affected where
    type EnumSetRep Affected = Word8

data Exit = Break | Done | Completed deriving (Eq)

-- | Processes an action before it can be applied to the game. This is where
-- invincibility, usability, reflects, etc. all come into play.
-- If an action is applied directly instead of passing it to this function,
-- its exact effects will occur and nothing else.
wrap :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Affected -> m () -> m ()
wrap affected f = void $ runMaybeT do
    skill       <- P.skill
    user        <- P.user
    target      <- P.target
    nUser       <- P.nUser
    nTarget     <- P.nTarget
    guard . not $ nTarget `is` Nullify
    startNinjas <- P.ninjas
    let classes  = Skill.classes skill
        targeted = Targeted ∈ affected
                   || Requirement.targetable (bypass skill) nUser nTarget
        exit     = if | Direct ∈ classes       -> Done
           --           | invinc                 -> Flagged
                      | Trapped ∈ affected     -> Done
                      | not targeted           -> Break
                      | not new                -> Done
                      | nTarget `is` Uncounter -> Done
                      | otherwise              -> Completed

    guard $ exit /= Break

    let harm      = not $ Parity.allied user target
        allow aff = harm && not (nUser `is` AntiCounter) && aff ∉ affected

    if exit == Done then
        lift $ withDirect skill f
    else lift . fromMaybe
        do
            f
            when (allow Reflected) . P.withTargets (Effects.share nTarget) $
                wrap (insertSet Reflected affected) f
        $ do
            guard $ allow Redirected
            t <- Trigger.redirect classes nTarget
            return . P.withTarget t $ wrap (insertSet Redirected affected) f
        <|> do
            guard $ allow Reflected
                    && Unreflectable ∉ classes && Effects.reflect nTarget
            return do
                P.trigger target [OnReflect]
                P.with Context.reflect $ wrap (insertSet Reflected affected) f
    P.zipWith Traps.broken startNinjas
  where
    new = not $ setFromList [Channeled, Delayed, Trapped] `intersects` affected
    bypass skill
      | Trapped ∈ affected =
          skill { Skill.classes = insertSet Bypassing $ Skill.classes skill }
      | otherwise = skill
    withDirect skill
      | new       = id
      | otherwise =
          P.with \ctx -> ctx
            { Context.skill =
                skill { Skill.classes = insertSet Direct $ Skill.classes skill }
            }

chooseTargets :: ∀ m. (MonadPlay m, MonadRandom m)
              => [Runnable Target] -> m [[Runnable Slot]]
chooseTargets targets = do
    skill  <- P.skill
    nUser  <- P.nUser
    ninjas <- P.ninjas
    forM targets \target -> do
        choices <- chooseTarget $ Runnable.target target
        return [ target { Runnable.target = t }
                   | t <- choices
                   , Requirement.targetable skill nUser $ ninjas !! Slot.toInt t
                   ]

-- | Transforms a @Target@ into @Slot@s. 'RAlly' and 'REnemy' targets are chosen
-- at random.
chooseTarget :: ∀ m. (MonadPlay m, MonadRandom m) => Target -> m [Slot]
chooseTarget Self = singleton <$> P.user
chooseTarget Ally = do
    user   <- P.user
    target <- P.target
    return [target | Parity.allied user target]
chooseTarget Allies  = Slot.allies <$> P.user
chooseTarget RAlly   = maybeToList <$> (chooseTarget Allies >>= R.choose)
chooseTarget XAllies = delete <$> P.user <*> chooseTarget Allies
chooseTarget XAlly   = do
    user   <- P.user
    target <- P.target
    return [target | user /= target && Parity.allied user target]
chooseTarget Enemy = do
    user   <- P.user
    target <- P.target
    return [target | not $ Parity.allied user target]
chooseTarget Enemies  = Slot.enemies <$> P.user
chooseTarget XEnemies = delete <$> P.target <*> chooseTarget Enemies
chooseTarget REnemy   = maybeToList <$> (chooseTarget Enemies >>= R.choose)
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
        wrap affected f
        P.trigger target [OnHelped]
    else do
        wrap affected f
        P.trigger user [OnHarm]
        classes <- Skill.classes <$> P.skill
        P.trigger target $ OnHarmed <$> toList classes

-- | Handles effects in a 'Skill'. Uses 'targetEffect' internally.
run :: ∀ m. (MonadPlay m, MonadRandom m)
        => EnumSet Affected -> [[Runnable Slot]] -> m ()
run affected xs = do
    skill      <- P.skill
    let local t ctx = ctx { Context.skill = skill, Context.target = t }
        execute (To t r) = P.with (local t) $ targetEffect affected r
    traverse_ (traverse_ execute) xs

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
filterCounters slots = filter $ (testBit targetSet . Slot.toInt) . Ninja.slot
  where
    acc x     = setBit x . Slot.toInt . Runnable.target
    targetSet = foldl' acc (0 :: Word8) $ join slots

setActed :: Ninja -> Ninja
setActed n = n { Ninja.acted = True }

-- | Performs an action, passing its effects to 'wrap' and activating any
-- corresponding 'Trap.Trap's once it occurs.
act :: ∀ m. (MonadGame m, MonadRandom m) => Act -> m ()
act a = do
    nUser     <- P.ninja user
    game      <- P.game
    let (affected, skill) = swapped nUser
        classes = Skill.classes skill
        charge  = Skill.charges skill > 0
        cost    = Skill.cost skill
        chakra  = Parity.getOf user $ Game.chakra game
        valid   = Ninja.alive nUser
                  && Skill.require skill /= Unusable
                  && not (new && Chakra.lack (chakra - cost))

    when valid $ P.withContext (ctx skill) do
        P.trigger user $ OnAction <$> toList classes

        if not new then
            run affected =<< chooseTargets (Skill.effects skill)
        else do
            P.modify user \n -> n { Ninja.lastSkill = Just skill }
            P.alter $ Game.adjustChakra user (— cost)
            P.modify user $ Cooldown.updateN charge skill s . setActed
            efs <- chooseTargets (Skill.start skill ++ Skill.effects skill)

            countering  <- filterCounters efs . toList <$> P.enemies user
            let harmed   = not $ null countering
            let counters =
                    Trigger.userCounters harmed user classes nUser
                    ++ (Trigger.targetCounters user classes =<< countering)
            if not $
              Uncounterable ∈ classes
              || nUser `is` AntiCounter
              || null counters then do
                let countered = Ninja.slot <$> countering
                    uncounter n
                      | slot == user     = Trigger.userUncounter classes n
                      | slot ∈ countered = Trigger.targetUncounter classes n
                      | otherwise        = n
                      where
                        slot = Ninja.slot n
                P.modifyAll uncounter
                sequence_ counters
            else do
                run affected efs
                addChannels
        traverse_ (traverse_ P.launch . Traps.get user) =<< P.ninjas

    P.modifyAll $ unreflect . \n -> n { Ninja.triggers = mempty }
    breakControls
  where
    s       = Act.skill a
    new     = isLeft s
    user    = Act.user a
    swapped nUser
      | nUser `is` Swap = (setFromList [Swapped, Targeted], Skills.swap skill)
      | otherwise       = (singletonSet Targeted, skill)
      where
        skill = Ninjas.skill s nUser
    ctx skill = Context { Context.skill  = skill
                        , Context.user   = user
                        , Context.target = Act.target a
                        , Context.new    = new
                        }
    unreflect n
      | OnReflect ∈ Ninja.triggers n =
          Ninjas.modifyStatuses (Status.removeEffect Reflect) n
      | otherwise = n

interruptions :: Skill -> [Runnable Target]
interruptions skill = To Enemy clear : To Ally clear : Skill.interrupt skill
  where
    clear :: ∀ m. MonadPlay m => m ()
    clear = P.fromUser . Ninjas.clear $ Skill.name skill

nonRandom :: Target -> Bool
nonRandom RAlly  = False
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
  | stuns `intersects` Skill.classes skill = P.withContext chanContext doBreak
  | otherwise = P.withContext chanContext do
      targets <- chooseTargets . filter (nonRandom . Runnable.target) $
                Skill.effects skill
      when (any null targets) doBreak
  where
    doBreak = do
        interruptTargets <- chooseTargets $ interruptions skill
        run (setFromList [Channeled, Interrupted]) interruptTargets
        P.modify user . Ninjas.cancelChannel $ Skill.name skill
    chanContext = Context { Context.skill  = skill
                          , Context.user   = user
                          , Context.target = target
                          , Context.new    = False
                          }
breakControl _ _ _ = return ()
