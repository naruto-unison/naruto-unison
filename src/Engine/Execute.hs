module Engine.Execute
  ( Affected(..)
  , wrap
  , act
  , effect
  , copy
  ) where

import ClassyPrelude.Yesod hiding ((<|), redirect)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Either (isLeft)
import qualified Data.HashSet as Set
import qualified Data.List as List
import qualified Data.Sequence as Seq

import           Core.Util ((—), (∈), (∉), incr, intersects, sync)
import qualified Class.Parity as Parity
import qualified Class.Play as P
import           Class.Play (GameT, Play, PlayT)
import qualified Class.Random as R
import           Class.Random (RandomT)
import qualified Class.TurnBased as TurnBased
import qualified Model.Act as Act
import           Model.Act (Act)
import qualified Model.Channel as Channel
import           Model.Channel (Channeling(..))
import qualified Model.Chakra as Chakra
import qualified Model.ChannelTag as ChannelTag
import           Model.Class (Class(..))
import qualified Model.Context as Context
import           Model.Context (Context)
import qualified Model.Copy as Copy
import           Model.Copy (Copying)
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

data Affected
    = Channeled
    | Countered
    | Delayed
    | Disrupted
    | Parrying
    | Redirected
    | Reflected
    | Swapped
    | Trapped
    deriving (Enum, Show, Eq)

copy :: Bool -> (Slot -> Int -> Copying) -> Slot -> Skill
     -> (Slot, Text, Int, Int) -> Game -> Game
copy clear cop source skill (user, name, s, dur) game
  | clear     = Game.alter (Ninja.clear name user <$>) adjusted
  | otherwise = adjusted
  where
    adjusted = Game.adjust user copier game
    copied   = Just . Copy.Copy skill' . sync $
               if dur < (-1) then dur + 1 else dur
    skill'   = skill
               { Skill.cost     = 0
               , Skill.cooldown = 0
               , Skill.copying  = cop source (sync dur - 1)
               }
    copier n = n { Ninja.copies = Seq.update s copied $ Ninja.copies n }

data Exit = Flagged | Done | Mimicked | Completed deriving (Eq)

wrap :: ∀ m. (PlayT m, RandomT m) => [Affected] -> m () -> m ()
wrap affected f = void $ runMaybeT do
    skill       <- P.skill
    source      <- P.source
    user        <- P.user
    target      <- P.target
    game        <- P.game
    let nSource  = Game.ninja source game
        nUser    = Game.ninja user game
        nTarget  = Game.ninja target game
        classes  = Skill.classes skill
        targeted = Requirement.targetable (bypass skill) nSource nUser nTarget
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

    unless ( target == source
          || Skill.channel skill == Instant
          || Disrupted ∈ affected
          || not (Ninja.isChanneling (Skill.name skill) nUser)
           ) . P.modify $ Game.adjust target \n ->
              n { Ninja.tags = ChannelTag.new skill source : Ninja.tags n }

    let harm = not $ Parity.allied target source && Parity.allied target user
    when new do
        copies <- flip (Trigger.replace classes) harm <$> P.nUser
        traverse_ (P.modify . copy True Copy.Shallow user skill) copies
        P.modify . Game.adjust user $ Ninja.setLast skill
    mimicked <- P.game

    if exit /= Completed then do
      lift $ P.withSkill skill { Skill.classes = affs classes ++ classes } f
      guard $ exit /= Done

      when (Ninja.is Silence nSource) do
          onlyDmgNs <- zipWith onlyDmg (Game.ninjas mimicked) .
                       Game.ninjas <$> P.game
          P.modify . const $ mimicked { Game.ninjas = onlyDmgNs }

    else lift do
      let allow aff = harm && not (Ninja.is AntiCounter nUser)
                      && aff ∉ affected
      case guard (allow Redirected) >>
           Trigger.redirect classes nTarget of
        Just x  -> P.withTarget x $ wrap (Redirected : affected) f
        Nothing -> case guard (allow Redirected) >>
                        Trigger.reapply classes nTarget of
          Just x -> P.withTarget x do
            P.withSkill skill { Skill.classes = Unshifted : classes } $
                wrap (Redirected : affected) f
            P.withTarget x $ wrap (Redirected : affected) f
          Nothing -> case guard (allow Countered) >>
                          Trigger.parry skill nTarget of
            Just (nt'Trapped, st, f') -> do
              P.modify $ Game.setNinja target nt'Trapped
              P.with (\ctx -> ctx { Context.source = Status.source st
                                 , Context.user   = Status.user st
                                 , Context.target = Context.user ctx
                                 }) . wrap (Trapped : affected) $ P.play f'
            Nothing -> case guard (allow Reflected) >>
                            Trigger.reflect classes nUser nTarget of
              Just x -> do
                  P.modify $ Game.setNinja target x
                  P.with Context.reflect $ wrap (Reflected : affected) f
              Nothing -> case guard (allow Countered) >>
                         Trigger.counter classes nUser nTarget of
                Just x -> P.modify $ Game.setNinja target x
                Nothing -> do
                  f
                  (nTarget', traps) <- Traps.broken nTarget <$> P.nTarget
                  P.modify $ Traps.entrap user traps .
                            Game.setNinja target nTarget'
  where
    new = not $ [Channeled, Delayed, Trapped] `intersects` affected
    shifted = [Redirected, Reflected, Swapped] `intersects` affected
    bypass skill
      | Countered ∈ affected =
          skill { Skill.classes = Bypassing : Skill.classes skill }
      | otherwise = skill
    affs classes = fst <$> filter snd
        [ (Shifted, shifted && not ([Unshifted, Shifted] `intersects` classes))
        , (Direct,  Trapped ∈ affected && TrapAttack ∉ classes)
        ]
    onlyDmg n n' =
        n { Ninja.health   = min (Ninja.health n) (Ninja.health n')
          , Ninja.statuses = Ninja.statuses n'
          }

chooseTarget :: ∀ m. (PlayT m, RandomT m) => Target -> m [Slot]
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

effect :: ∀ m. (PlayT m, RandomT m) => [Affected] -> (Target, m ()) -> m ()
effect affected (target, f)
  | counter && (parry && target == Ally || not parry && anyone) = return ()
  | otherwise = do
      skill   <- unshift <$> P.skill
      targets <- chooseTarget target
      let localize t ctx = ctx { Context.skill = skill, Context.target = t }
      traverse_ (flip P.with (wrap affected f) . localize) targets
  where
    counter = Countered ∈ affected
    parry = Parrying ∈ affected
    anyone = target ∈ [Self, Allies, Enemies, Everyone]
    unshift skill = case target of
        Self -> skill { Skill.classes = Unshifted : Skill.classes skill }
        _    -> skill

effects :: ∀ m. (PlayT m, RandomT m) => [Affected] -> m ()
effects affected = traverse_ (effect affected . second P.play) =<<
                   getEffects <$> P.skill
  where
    getEffects skill
      | Channeled ∈ affected = Skill.effects skill
      | otherwise            = Skill.start skill ++ Skill.effects skill

trigger :: [Affected] -> Skill -> Slot
        -> Game -- ^ New
        -> Game -- ^ Old
        -> Game
trigger affected skill user game game'Pre
  | Channeled ∈ affected = game'
  | null counters             = game'Tr
  | otherwise = Traps.entrap user counters $
                Game.adjust user Ninja.clearCounters
                game { Game.ninjas = ns'Cp }
  where
    classes   = Skill.classes skill
    n         = Game.ninja user game
    n'Pre     = Game.ninja user game'Pre
    dmgTot    = sum $ zipWith Ninja.healthLost
                (Game.ninjas game) (Game.ninjas game'Pre)
    n'        = n'Pre { Ninja.traps = updatePer <$> Ninja.traps n'Pre }
    -- Game with user's damage/healing tracking updated.
    game'     = Game.setNinja user n' game'Pre
    ns'Cp     = zipWith cop (Game.ninjas game) (Game.ninjas game')
    swp (x, y)
      | Parity.even user = (x, y)
      | otherwise        = (y, x)
    (allies,  enemies)  = swp . Parity.split $ Game.ninjas game
    (allies', enemies') = swp . Parity.split $ Game.ninjas game'
    als       = [(a, a') | a  <- allies
                         | a' <- allies']
    ens       = [(a, a') | a  <- enemies
                         | a' <- enemies']
    allNs     = [(nt, nt') | nt  <- Game.ninjas game,  Ninja.slot nt  /= user
                           | nt' <- Game.ninjas game', Ninja.slot nt' /= user]
    chakraF   = Game.chakra game /= Game.chakra game'
    harmful   = not (null harmed) || Ninja.barrier n /= Ninja.barrier n'
    counters  = Traps.get True (OnCounter Uncounterable) n'
                ++ Traps.getClass
                   (harmful && Uncounterable ∉ classes)
                   OnCounter classes n'
    healed   = [ nt' | (nt, nt') <- als, Ninja.health nt < Ninja.health nt' ]
    helped   = [ nt' | (nt, nt') <- als, nt /= nt' ]
    harmed   = [ nt' | (nt, nt') <- ens, nt /= nt' ]
    dmgEns   = [ nt' | (nt, nt') <- allNs, Ninja.health nt > Ninja.health nt' ]
    damaged  = [ nt' | (nt, nt') <- allNs, Ninja.health nt > Ninja.health nt' ]
    stunned  = [ nt' | (nt, nt') <- ens
                     , Ninja.isAny Stun nt'
                     , not $ Ninja.isAny Stun nt ]
    addFlags = fst <$> filter snd
               [ (Acted,  game'Pre /= game)
               , (Harmed, harmful)
               ]
    n'Acted  =
        n' { Ninja.flags = foldl' (flip Set.insert) (Ninja.flags n') addFlags }
    n'Taunt  = case ( toList harmed, Effects.taunting n' ) of
        ( [n'Taunted], Just (dur, st) ) ->
            let st' = st { Status.source  = Ninja.slot n'Taunted
                         , Status.user    = Ninja.slot n'Taunted
                         , Status.effects = [Taunt]
                         , Status.bombs   = []
                         , Status.dur     = sync dur
                         , Status.maxDur  = sync dur
                         }
            in n'Acted
               { Ninja.statuses = st' : List.delete st (Ninja.statuses n')}
        _ -> n'Acted
    -- Game with user's flags and taunts updated.
    game'Fl  = Game.setNinja user n'Taunt game'
    -- The following traps are applied in order.
    -- 1) Per-damage traps.
    trapsP   = Traps.getTracked True TrackDamage n'
               ++ Traps.getPer (not $ null dmgEns) PerDamage dmgTot n'
    -- 2) TrapTos on the user.
    trapsC   = join $ fromList
               [ counters
               , Traps.getTo chakraF               OnChakra   n'
               , Traps.getTo (not $ null dmgEns)   OnDamage   n'
               , Traps.getTo (not $ null stunned)  OnStun     n'
               , Traps.getTo harmful               OnHarm     n'
               , Traps.getClass (game'Pre /= game) OnAction   classes n'
               , Traps.getTo (was Invulnerable (n', n)) OnImmune   n'
               ]
    -- 3) Traps on targets.
    trapsT   = join . join $ fromList
               [ Traps.get True OnHelped          <$> helped
               , Traps.get True OnHealed          <$> healed
               , Traps.get True OnStunned         <$> stunned
               , Traps.getClass True OnHarmed classes  <$> harmed
               , Traps.getClass True OnDamaged classes <$> damaged
               ]
    -- 4) TrapFroms on the user.
    trapsF   = join . join $ fromList
               [ Traps.getFrom OnDamage n' <$> damaged
               , Traps.getFrom OnStun   n' <$> stunned
               , Traps.getFrom OnHarm   n' <$> harmed
               ] :: Seq (Context, Play ())
    -- Game with Traps applied.
    allTraps = trapsP ++ trapsC ++ trapsT ++ trapsF
    game'Tr  = game'Fl { Game.traps = Game.traps game'Fl ++ allTraps }
    cop t t'  = t { Ninja.copies = Ninja.copies t' }
    was ef (nt, nt') = Ninja.isAny ef nt' && not (Ninja.isAny ef nt)
    updatePer trap = case Trap.trigger trap of
        TrackDamage -> trap { Trap.tracker = dmgTot + Trap.tracker trap }
        _           -> trap

addChannels :: ∀ m. PlayT m => m ()
addChannels = do
    skill    <- P.skill
    user     <- P.user
    target   <- P.target
    let chan  = Skill.channel skill
        dur   = Copy.maxDur (Skill.copying skill) . incr . sync $
                TurnBased.getDur chan
        chan' = Channel.Channel { Channel.root   = Copy.root skill user
                                , Channel.skill  = skill
                                , Channel.target = target
                                , Channel.dur    = TurnBased.setDur dur chan
                                }
    unless (chan == Instant || dur == 1 || dur == 2) .
        P.modify $ Game.adjust target \n ->
            n { Ninja.newChans = chan' : Ninja.newChans n }

act :: ∀ m. (GameT m, RandomT m) => [Affected] -> Act -> m ()
act affected' a = do
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
          P.modify $ Cooldown.updateGame charge 0 skill user s
          pre <- P.game
          effects affected
          when (isLeft s) addChannels
          P.modify $ trigger affected skill user pre
  where
    s       = Act.skill a
    user    = Act.user a
    channed = Channeled ∈ affected'
    ctx skill = Context.Context { Context.skill  = skill
                                , Context.source = user
                                , Context.user   = user
                                , Context.target = Act.target a
                                }
