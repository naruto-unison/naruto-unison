-- | Action processing. The core of the game engine.
module Engine.Execute
  ( wrap
  , Affected(..)
  , act
  , effect
  , copy
  ) where

import ClassyPrelude hiding ((<|))

import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Either (isLeft)
import qualified Data.HashSet as Set
import qualified Data.List as List
import qualified Data.Sequence as Seq

import           Core.Util ((—), (∈), (∉), enumerate, intersects)
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
import qualified Model.ChannelTag as ChannelTag
import           Model.Class (Class(..))
import qualified Model.Context as Context
import qualified Model.Copy as Copy
import           Model.Copy (Copying)
import           Model.Duration (Duration, incr, sync)
import           Model.Effect (Effect(..))
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Flag(..))
import qualified Model.Requirement as Requirement
import           Model.Requirement (Requirement(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))
import qualified Model.Status as Status
import qualified Model.Slot as Slot
import           Model.Slot (Slot)
import qualified Model.Trap as Trap
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
    deriving (Enum, Show, Eq)

-- | Adds a 'Copy.Copy' to 'Ninja.copies'.
copy :: Bool -- ^ Whether or not to call 'Ninja.clear' on matching 'Status.Status'es.
     -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
     -> Slot -- ^ 'Game.ninjas' index of the 'Ninja.Ninja' user of the action.
     -> Skill -- ^ The 'Skill' to copy.
     -> (Slot, Text, Int, Duration) -- ^ Output of 'Trigger.replace'.
      -> Game -> Game
copy clear cop target skill (user, name, s, dur) game
  | clear     = Game.alter (Ninja.clear name target <$>) adjusted
  | otherwise = adjusted
  where
    adjusted = Game.adjust target copier game
    copied   = Just . Copy.Copy skill' $ sync if dur < -1 then dur + 1 else dur
    skill'   = skill
               { Skill.cost     = 0
               , Skill.cooldown = 0
               , Skill.copying  = cop user $ sync dur - 1
               }
    copier n = n { Ninja.copies = Seq.update s copied $ Ninja.copies n }

data Exit = Flagged | Done | Mimicked | Completed deriving (Eq)
-- | Processes an action before it can be applied to the game. This is where
-- invincibility, usability, counters, reflects, etc. all come into play.
-- If an action is applied directly instead of passing it to this function,
-- its exact effects will occur and nothing else.
wrap :: ∀ m. (MonadPlay m, MonadRandom m) => [Affected] -> m () -> m ()
wrap affected f = void $ runMaybeT do
    skill       <- P.skill
    user        <- P.user
    target      <- P.target
    game        <- P.game
    let nUser    = Game.ninja user game
        nTarget  = Game.ninja target game
        classes  = Skill.classes skill
        targeted = Requirement.targetable (bypass skill) nUser nTarget
        invinc   = classes `intersects` Effects.invincible nTarget
        exit     = if | Direct ∈ classes               -> Done
                      | invinc                         -> Flagged
                      | Trapped ∈ affected             -> Done
                      | not targeted                   -> Flagged
                      | skill ∈ Ninja.parrying nTarget -> Flagged
                      | not new                        -> Done
                      | Ninja.is Uncounter nTarget     -> Mimicked
                      | otherwise                      -> Completed

    when new . P.modify $ Game.adjust target \n ->
        n { Ninja.flags = Set.insert Targeted $ Ninja.flags n }
    guard $ exit /= Flagged

    unless ( target == user
          || Skill.channel skill == Instant
          || Interrupted ∈ affected
          || not (Ninja.isChanneling (Skill.name skill) nUser)
           ) . P.modify $ Game.adjust target \n ->
              n { Ninja.tags = ChannelTag.new skill user : Ninja.tags n }

    let harm      = Harmful ∈ classes && not (Parity.allied user target)
        allow aff = harm && not (Ninja.is AntiCounter nUser) && aff ∉ affected
    when new do
        copies <- flip (Trigger.replace classes) harm <$> P.nUser
        traverse_ (P.modify . copy True Copy.Shallow user skill) copies
        P.modify $ Game.adjust user \n -> n { Ninja.lastSkill = Just skill }
    mimicked <- P.game

    if exit /= Completed then do
        lift $ withDirect skill f
        guard $ exit /= Done

        when (Ninja.is Silence nUser) do
            onlyDmgNs <- Game.zipNinjasWith onlyDmg mimicked <$> P.game
            P.modify . const $ mimicked { Game.ninjas = onlyDmgNs }

    else lift . fromMaybe
        do
            f
            (nTarget', traps) <- Traps.broken nTarget <$> P.nTarget
            P.modify $ Game.setNinja target nTarget'
            traverse_ P.launch traps
            when (allow Reflected) . P.withTargets (Effects.share nTarget) $
                wrap (Reflected : affected) f
        $ do
            guard $ allow Redirected
            t <- Trigger.redirect classes nTarget
            return . P.withTarget t $ wrap (Redirected : affected) f
        <|> do
            guard $ allow Trapped
            (nt, st, f') <- Trigger.parry skill nTarget
            return do
                P.modify $ Game.setNinja target nt
                P.with (\ctx -> ctx
                              { Context.user   = Status.user st
                              , Context.target = Context.user ctx
                              }) . wrap (Trapped : affected) $ P.play f'
        <|> do
            guard $ allow Trapped
            (n, nt, mTrap) <- Trigger.counter classes nUser nTarget
            return do
                P.modify $ Game.setNinja user n . Game.setNinja target nt
                case mTrap of
                    Nothing   -> return ()
                    Just trap -> P.launch $ Trap.effect trap 0
        <|> do
            guard $ allow Reflected
            nt <- Trigger.reflect classes nUser nTarget
            return do
                P.modify $ Game.setNinja target nt
                P.with Context.reflect $ wrap (Reflected : affected) f
  where
    new = not $ [Channeled, Delayed, Trapped] `intersects` affected
    bypass skill
      | Trapped ∈ affected =
          skill { Skill.classes = Bypassing : Skill.classes skill }
      | otherwise = skill
    withDirect skill
      | Trapped ∈ affected && TrapAttack ∉ Skill.classes skill =
          P.withSkill skill { Skill.classes = Direct : Skill.classes skill }
      | otherwise = id
    onlyDmg n n' =
        n { Ninja.health   = min (Ninja.health n) (Ninja.health n')
          , Ninja.statuses = Ninja.statuses n'
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
chooseTarget XAllies = List.delete <$> P.user <*> chooseTarget Allies
chooseTarget XAlly   = do
    user   <- P.user
    target <- P.target
    return [target | user /= target && Parity.allied user target]
chooseTarget Enemy = do
    user   <- P.user
    target <- P.target
    return [target | not $ Parity.allied user target]
chooseTarget Enemies  = Slot.enemies <$> P.user
chooseTarget XEnemies = List.delete <$> P.target <*> chooseTarget Enemies
chooseTarget REnemy   = maybeToList <$> (chooseTarget Enemies >>= R.choose)
chooseTarget Everyone = return Slot.all

-- | Handles a single effect tuple in a 'Skill'. Uses 'wrap' internally.
effect :: ∀ m. (MonadPlay m, MonadRandom m) => [Affected] -> (Target, m ()) -> m ()
effect affected (target, f)  = do
      skill   <- P.skill
      targets <- chooseTarget target
      let localize t ctx = ctx { Context.skill = skill, Context.target = t }
      traverse_ (flip P.with (wrap affected f) . localize) targets

-- | Handles all effects of a 'Skill'. Uses 'wrap' internally.
effects :: ∀ m. (MonadPlay m, MonadRandom m) => [Affected] -> m ()
effects affected = traverse_ (effect affected . second P.play) =<<
                   getEffects <$> P.skill
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
    unless (chan == Instant || dur == 1 || dur == 2) .
        P.modify $ Game.adjust target \n ->
            n { Ninja.newChans = chan' : Ninja.newChans n }

-- | Performs an action, passing its effects to 'wrap' and activating any
-- corresponding 'Trap.Trap's once it occurs.
act :: ∀ m. (MonadGame m, MonadRandom m) => [Affected] -> Act -> m ()
act affected' a = do
    P.modify $ Game.alter (Adjust.effects <$>)
    game      <- P.game
    let nUser  = Game.ninja user game
        skill' = Adjust.skill s nUser
    (affected, skill) <- case Trigger.swap (Skill.classes skill') nUser of
        Just swapped -> do
            P.modify $ Game.setNinja user nUser
                { Ninja.statuses = List.delete swapped $ Ninja.statuses nUser }
            return (Swapped : affected', SkillTransform.swap swapped skill')
        Nothing -> return (affected', skill')
    let charge = Skill.charges skill > 0
        cost   = Skill.cost skill
        usable = not $ Skill.require skill == Unusable
                    || Chakra.lack (Game.getChakra user game - cost)
        valid  = Ninja.alive nUser && (channed || usable) && case s of
            Left _   -> True
            Right s' -> channed || Ninja.isChanneling (Skill.name s') nUser

    when valid . P.withContext (ctx skill) $ case Trigger.snareTrap skill nUser of
      Just (n', sn) -> P.modify . Game.setNinja user $ case s of
          Left s' | s' <= 3 -> Cooldown.update charge sn skill s' n'
          _                 -> n'
      Nothing -> do
          unless channed . P.modify $ Game.adjustChakra user (— cost)
          P.modify $ Cooldown.updateGame charge skill user s
          game' <- P.game
          effects affected
          when (isLeft s) addChannels
          trigger affected game'
  where
    s       = Act.skill a
    user    = Act.user a
    channed = Channeled ∈ affected'
    ctx skill = Context.Context { Context.skill  = skill
                                , Context.user   = user
                                , Context.target = Act.target a
                                }

invincEffects :: [Effect]
invincEffects = (Invulnerable <$> enumerate) ++ (Invincible <$> enumerate)

-- | After an action takes place, activates any corresponding 'Trap.Trap's.
trigger :: ∀ m. (MonadPlay m, MonadRandom m) => [Affected] -> Game -> m ()
trigger affected gameBefore = void $ runMaybeT do
    user  <- P.user
    total <- sum . Game.zipNinjasWith Ninja.healthLost gameBefore <$> P.game
    P.modify . Game.adjust user $ Traps.track PerDamage total
    guard $ null affected

    classes     <- fromList . Skill.classes <$> P.skill
    let nUser    = Traps.track PerDamage total $ Game.ninja user gameBefore
        counters = Traps.get user True (OnCounter Uncounterable) nUser
                   ++ Traps.getClassed classes user
                      (Harmful ∈ classes && Uncounterable ∉ classes)
                      OnCounter nUser
    if not $ null counters || affectedBy [Reflected, Trapped] then do
        updatedCopies <- Game.zipNinjasWith updateCopies gameBefore <$> P.game
        P.modify $ const gameBefore { Game.ninjas = updatedCopies }
        traverse_ P.launch counters
    else do
        guard . (gameBefore /=) =<< P.game
        nPairs     <- zip (Game.ninjas gameBefore) . Game.ninjas <$> P.game
        chakra     <- (Game.chakra gameBefore /=) . Game.chakra <$> P.game
        isImmune   <- (invincEffects `intersects`) . Ninja.effects <$> P.nUser
        let immuned = isImmune
                      && not (invincEffects `intersects` Ninja.effects nUser)
            (allies, enemies)
              | Parity.even user = Parity.split nPairs
              | otherwise        = swap $ Parity.split nPairs
            harmed  = filterOn enemies (/=)
            helped  = filterOn allies (/=)
            damaged = filterOn enemies \n n' -> Ninja.health n' < Ninja.health n
            healed  = filterOn allies  \n n' -> Ninja.health n' > Ninja.health n
            stunned = filterOn enemies \n n' -> Ninja.isAny Stun n'
                                                && not (Ninja.isAny Stun n)
            getClassed  = Traps.getClassed classes user
            flagHarmed
              | null harmed = id
              | otherwise   = Set.insert Harmed
        P.modify $ Game.adjust user \n ->
            n { Ninja.flags = flagHarmed . Set.insert Acted $ Ninja.flags n }
        traverse_ (traverse_ P.launch) -- cheaper than a 'join'
            [ getClassed     True    OnAction      nUser
            , Traps.get user chakra  OnChakra      nUser
            , Traps.get user immuned OnImmune      nUser
            , Traps.getTo    harmed  OnHarm        nUser
            , Traps.getTo    damaged OnDamage      nUser
            , Traps.getTo    stunned OnStun        nUser
            , getClassed     True    OnHarmed  =<< harmed
            , Traps.get user True    OnHelped  =<< helped
            , getClassed     True    OnDamaged =<< damaged
            , Traps.get user True    OnHealed  =<< healed
            , Traps.get user True    OnStunned =<< stunned
            ]
  where
    affectedBy = (`intersects` affected)
    -- Ensures new 'Ninja.copies' are preserved even if an 'OnCounter'
    -- nullifies the rest of the action.
    updateCopies n n' = n { Ninja.copies = Ninja.copies n' }
    filterOn xs f     = snd <$> filter (uncurry f) xs
