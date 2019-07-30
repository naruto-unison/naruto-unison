-- | Action processing. The core of the game engine.
module Engine.Execute
  ( wrap
  , Affected(..)
  , act
  , effect, effects, addChannels
  , copy
  ) where

import ClassyPrelude hiding ((<|))

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Either (isLeft)

import           Core.EnumSet (EnumSet)
import           Core.Util ((—), (∈), (∉), intersectsSet, updateVec)
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Class.TurnBased as TurnBased
import qualified Model.Act as Act
import           Model.Act (Act)
import qualified Model.Channel as Channel
import           Model.Channel (Channeling(..))
import qualified Model.Chakra as Chakra
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import           Model.Copy (Copying)
import           Model.Duration (Duration, incr, sync)
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import           Model.Ninja (is)
import qualified Model.Requirement as Requirement
import           Model.Requirement (Requirement(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))
import qualified Model.Status as Status
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import           Model.Trap (Trigger(..))
import qualified Engine.Adjust as Adjust
import qualified Engine.Cooldown as Cooldown
import qualified Engine.Effects as Effects
import qualified Engine.SkillTransform as SkillTransform
import qualified Engine.Traps as Traps
import qualified Engine.Trigger as Trigger

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
    | Trapped
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

type AffectedSet = EnumSet Word8 Affected

oldSet :: AffectedSet
oldSet = setFromList [Channeled, Delayed, Trapped]

-- | Adds a 'Copy.Copy' to 'Ninja.copies'.
copy :: ∀ m. MonadPlay m
     => Bool -- ^ Whether or not to call 'Ninja.clear' on matching 'Status.Status'es.
     -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
     -> Slot -- ^ Target.
     -> Skill -- ^ Skill.
     -> (Slot, Text, Int, Duration) -- ^ Output of 'Trigger.replace'.
     -> m ()
copy clear cop target skill (source, name, s, dur) = do
    P.modify target \n -> n { Ninja.copies = copier $ Ninja.copies n }
    when clear . P.modifyAll $ Ninja.clear name target
  where
    skill' = skill { Skill.cost     = 0
                   , Skill.cooldown = 0
                   , Skill.copying  = cop source $ sync dur - 1
                   }
    copier = updateVec s . Just . Copy.Copy skill' $
             sync if dur < -1 then dur + 1 else dur

data Exit = Flagged | Done | Completed deriving (Eq)
-- | Processes an action before it can be applied to the game. This is where
-- invincibility, usability, counters, reflects, etc. all come into play.
-- If an action is applied directly instead of passing it to this function,
-- its exact effects will occur and nothing else.
wrap :: ∀ m. (MonadPlay m, MonadRandom m) => AffectedSet -> m () -> m ()
wrap affected f = void $ runMaybeT do
    skill       <- P.skill
    user        <- P.user
    target      <- P.target
    nUser       <- P.nUser
    nTarget     <- P.nTarget
    startNinjas <- P.ninjas
    let classes  = Skill.classes skill
        targeted = Requirement.targetable (bypass skill) nUser nTarget
        invinc   = classes `intersectsSet` Effects.invincible nTarget
        exit     = if | Direct ∈ classes       -> Done
                      | invinc                 -> Flagged
                      | Trapped ∈ affected     -> Done
                      | not targeted           -> Flagged
                      | not new                -> Done
                      | nTarget `is` Uncounter -> Done
                      | otherwise              -> Completed

    -- P.flag target Flag.Targeted
    guard $ exit /= Flagged

    let harm      = Harmful ∈ classes && not (Parity.allied user target)
        allow aff = harm && not (nUser `is` AntiCounter) && aff ∉ affected
    when new do
        copies <- flip (Trigger.replace classes) harm <$> P.nUser
        traverse_ (copy True Copy.Shallow user skill) copies
        P.modify user \n -> n { Ninja.lastSkill = Just skill }

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
            guard $ allow Trapped
            (nt, st, f') <- Trigger.parry skill nTarget
            return do
                P.write target nt
                P.with (\ctx -> ctx
                        { Context.user   = Status.user st
                        , Context.target = Context.user ctx
                        }) . wrap (insertSet Trapped affected) $ P.play f'
        <|> do
            guard $ allow Trapped
            (n, nt, mPlay) <- Trigger.counter classes nUser nTarget
            return do
                P.write user n
                P.write target nt
                maybe (return ()) P.launch mPlay
        <|> do
            guard $ allow Reflected
            nt <- Trigger.reflect classes nUser nTarget
            return do
                P.write target nt
                P.with Context.reflect $ wrap (insertSet Reflected affected) f
    P.zipWith Traps.broken startNinjas
  where
    new = not $ oldSet `intersectsSet` affected
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

-- | Transforms a 'Target' into 'Slot's. 'RAlly' and 'REnemy' targets are chosen
-- at random.
chooseTarget :: ∀ m. (MonadPlay m, MonadRandom m) => Target -> m [Slot]
chooseTarget (Specific x) = return [x]
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
targetEffect :: ∀ m. (MonadPlay m, MonadRandom m) => AffectedSet -> m () -> m ()
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
        P.trigger target =<< (OnHarmed <$>) . toList . Skill.classes <$> P.skill


-- | Handles a single effect tuple in a 'Skill'. Uses 'targetEffect' internally.
effect :: ∀ m. (MonadPlay m, MonadRandom m) => AffectedSet -> (Target, m ())
       -> m ()
effect affected (target, f)  = do
      skill   <- P.skill
      targets <- chooseTarget target
      let localize t ctx = ctx { Context.skill = skill, Context.target = t }
      traverse_ (flip P.with (targetEffect affected f) . localize) targets

-- | Handles all effects of a 'Skill'. Uses 'effect' internally.
effects :: ∀ m. (MonadPlay m, MonadRandom m) => AffectedSet -> m ()
effects affected = traverse_ (effect affected . second P.play)
                   =<< getEffects <$> P.skill
  where
    getEffects skill
      | Channeled ∈ affected = Skill.effects skill
      | otherwise            = Skill.start skill ++ Skill.effects skill

-- | If 'Skill.dur' is long enough to last for multiple turns, the 'Skill'
-- is added to 'Ninja.channels'.
addChannels :: ∀ m. MonadPlay m => m ()
addChannels = do
    skill    <- P.skill
    user     <- P.user
    target   <- P.target
    let chan  = Skill.channel skill
        dur   = Copy.maxDur (Skill.copying skill) . incr $ TurnBased.getDur chan
        chan' = Channel.Channel { Channel.source = Copy.source skill user
                                , Channel.skill  = skill
                                , Channel.target = target
                                , Channel.dur    = TurnBased.setDur dur chan
                                }
    unless (chan == Instant || dur == 1 || dur == 2) $
        P.modify user \n -> n { Ninja.newChans = chan' : Ninja.newChans n }

-- | Performs an action, passing its effects to 'wrap' and activating any
-- corresponding 'Trap.Trap's once it occurs.
act :: ∀ m. (MonadGame m, MonadRandom m) => Act -> m ()
act a = do
    P.modifyAll Adjust.effects
    nUser     <- P.ninja user
    game      <- P.game
    let skill' = Adjust.skill s nUser
    (affected, skill) <- case Trigger.swap (Skill.classes skill') nUser of
        Just swapped -> do
            P.write user nUser
                { Ninja.statuses = swapped `delete` Ninja.statuses nUser }
            return (singletonSet Swapped, SkillTransform.swap swapped skill')
        Nothing -> return (mempty, skill')
    let charge = Skill.charges skill > 0
        cost   = Skill.cost skill
        valid = Ninja.alive nUser
                && Skill.require skill /= Unusable
                && not (new && Chakra.lack (Game.getChakra user game - cost))

    when valid $ P.withContext (ctx skill) do
        P.trigger user $ OnAction <$> toList (Skill.classes skill)
        case Trigger.snareTrap skill nUser of
            Just (n', sn) -> P.write user case s of
                Left s' | s' <= 3 -> Cooldown.update charge sn skill s' n'
                _                 -> n'
            Nothing -> do
                when new . P.alter $ Game.adjustChakra user (— cost)
                P.modify user $ Cooldown.updateN charge skill s
                effects affected
                when new addChannels
        traverse_ (traverse_ P.launch . Traps.get user) =<< P.ninjas
        -- P.modifyAll \n -> n { Ninja.triggers = mempty } TODO
  where
    s       = Act.skill a
    new     = isLeft s
    user    = Act.user a
    ctx skill = Context.Context { Context.skill  = skill
                                , Context.user   = user
                                , Context.target = Act.target a
                                , Context.new    = new
                                }
{-
invincEffects :: [Effect]
invincEffects = enumerate Invulnerable ++ enumerate Invincible

-- | After an action takes place, activates any corresponding 'Trap.Trap's.
trigger :: ∀ m. (MonadPlay m, MonadRandom m) => AffectedSet -> Game -> m ()
trigger affected gameBefore = void $ runMaybeT do
    user  <- P.user
    total <- sum . Game.zipNinjasWith Ninja.healthLost gameBefore <$> P.game
    P.modify . Game.adjust user $ Traps.t_rack PerDamage total
    guard $ null affected

    classes     <- fromList . Skill.classes <$> P.skill
    let nUser    = Traps.t_rack PerDamage total $ Game.ninja user gameBefore
        counters = Traps.g_et user True (OnCounter Uncounterable) nUser
                   ++ do
                      guard $ Harmful ∈ classes && Uncounterable ∉ classes
                      cla <- classes
                      Traps.g_et user True (OnCounter cla) nUser
    if not $ null counters || affectedBy [Reflected, Trapped] then do
        updatedCopies <- Game.zipNinjasWith updateCopies gameBefore <$> P.game
        P.modify $ const gameBefore { Game.ninjas = updatedCopies }
        traverse_ P.launch counters
  where
    affectedBy = (`intersects` affected)
    -- Ensures new 'Ninja.copies' are preserved even if an 'OnCounter'
    -- nullifies the rest of the action.
    updateCopies n n' = n { Ninja.copies = Ninja.copies n' }
    filterOn xs f     = snd <$> filter (uncurry f) xs
-}
