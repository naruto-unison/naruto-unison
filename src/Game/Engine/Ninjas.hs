
-- | 'Ninja' processing.
module Game.Engine.Ninjas
  ( modifyStatuses, modifyAll
  , apply
  , processEffects, processSkills

  , nextAlternate

  , decrement

  , factory

  , adjustHealth
  , sacrifice
  , setHealth
  , kill
  , bury

  , addTrap
  , addStatus
  , addBarrier
  , addDefense
  , increaseDefense
  , removeDefense
  , clearBarrier
  , clearDefense

  , clear
  , clearTrap, clearTraps, clearAnyTraps
  , cure
  , cureBane
  , purge
  , removeStacks

  , addChannels
  , cancelChannel, cancelOldChannel

  , copy, copyAll
  , recharge, rechargeAll, spendCharge

  , prolong, prolongControlled
  , prolongChannel
  , renameChannels
  , refresh
  ) where

import ClassyPrelude

import qualified Data.Sequence as Seq

import           Class.Classed (Classed(..))
import qualified Class.Parity as Parity
import           Class.Stackable ((.:))
import           Class.TurnBased (TurnBased)
import qualified Class.TurnBased as TurnBased
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Skills as Skills
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Channel as Channel
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Copy (Copy(Copy))
import qualified Game.Model.Copy as Copy
import           Game.Model.Destructible (Destructible(Destructible))
import qualified Game.Model.Destructible as Destructible
import           Game.Model.Duration (Duration(..), sync)
import           Game.Model.Effect (Amount(..), Effect(..))
import qualified Game.Model.Effect as Effect
import qualified Game.Model.Face as Face
import           Game.Model.ID (HasID, ID(ID))
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status
import           Game.Model.Trap (Trap(Trap))
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈), (∉))

processSkills :: Ninja -> Ninja
processSkills n = n { N.skills = zipWith getSkill skills n.copies }
  where
    skills = toNullable n.character.skills
    getSkill (base:|alts) mcopy = Requirement.usable True n
        . Skills.change n
        $ fromMaybe (own base) $ copied <|> (own <$> alternate)
      where
        own x     = x { Skill.owner = n.slot }
        copied    = Copy.skill <$> mcopy
        alternate = do
            alt <- Effects.alternate base.name n
            find ((== alt) . Skill.name) alts

-- | Cycles a skill through its list of alternates.
nextAlternate :: Text -> Ninja -> Maybe Text
nextAlternate baseName n = do
    _:|alts <- find ((== baseName) . Skill.name . head) n.character.skills
    headMay . dropUntilAlt $ Skill.name <$> alts
  where
    dropUntilAlt alts = case Effects.alternate baseName n of
        Just alt -> drop 1 $ dropWhile ((/= alt)) alts
        Nothing  -> alts

-- | Modifies @Effect@s when they are first added to a @Ninja@ due to @Effect@s
-- already added.
apply :: Ninja -> Ninja -> [Effect] -> [Effect]
apply n nt fs = adjustEffect <$> filter keepEffects fs
  where
    adjustEffect (Reduce cla Flat x) = Reduce cla Flat $ x - Effects.unreduce n
    adjustEffect f                   = f
    keepEffects Invulnerable{}       = not $ nt `is` Expose
    keepEffects _                    = True

-- | Fills 'N.effects' with the effects of 'N.statuses', modified by
-- 'NoIgnore', 'Seal', 'Boost', and so on.
processEffects :: Ninja -> Ninja
processEffects n@Ninja{barrier, defense, statuses} =
    n { N.effects = processed
      , N.face    = Face.new <$> find ((Face ∈) . Status.effects) statuses
      }
  where
    destrEffects  = [ effect | destructibles <- [barrier, defense],
                               Destructible{effects} <- destructibles,
                               effect <- effects ]
    statusEffects = [ effect | Status{effects, amount} <- statuses,
                               oneEffect <- effects,
                               effect <- replicate amount oneEffect ]
    allEffects = destrEffects ++ statusEffects

    hasEffect ef = ef ∈ allEffects
    hasNoIgnore  = hasEffect NoIgnore
    hasEnrage    = not hasNoIgnore && hasEffect Enrage
    hasSeal      = hasEffect Seal
    hasExpose    = not hasEnrage && hasEffect Expose
    hasFocus     = not hasSeal && not hasNoIgnore && hasEffect Focus

    allow ef
      | not (Effect.bypassEnrage ef) && hasEnrage = False
      | Effect.helpful ef && hasSeal = False
    allow Disable{} = not hasFocus
    allow Silence   = not hasFocus
    allow Stun{}    = not hasFocus
    allow Enrage    = not hasNoIgnore
    allow Focus     = not hasNoIgnore
    allow Nullify   = not hasNoIgnore
    allow (Bleed _ _ i)  = i >= 0 || not hasExpose
    allow (Reduce _ _ i) = i <= 0 || not hasExpose
    allow _ = True

    allowed = filter allow allEffects

    boost
      | hasSeal   = 1
      | otherwise = product $ 1 : [x | Boost x <- allEffects]

    processed
      | boost == 1 = allowed
      | otherwise  = map boostHelpful allowed
      where
        boostHelpful ef
          | Effect.helpful ef = Effect.adjust (* boost) ef
          | otherwise         = ef

-- | Alters 'statuses' and then calls 'processEffects'.
modifyStatuses :: ([Status] -> [Status]) -> Ninja -> Ninja
modifyStatuses f n = processEffects n { N.statuses = f n.statuses }

modifyAll :: (∀ a. (Classed a, HasID a, TurnBased a) => [a] -> [a])
          -> Ninja -> Ninja
modifyAll f n = processEffects n { N.defense  = f n.defense
                                 , N.barrier  = f n.barrier
                                 , N.statuses = f n.statuses
                                 , N.traps    = f n.traps
                                 }

-- | Factory resets a @Ninja@ to its starting values.
factory :: Ninja -> Ninja
factory n = N.new n.slot n.character

-- | Modifies 'health', restricting the value within [0, 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth _ n@Ninja{health = 0} = n
adjustHealth f n = n { N.health = health' }
  where
    healthF = f n.health
    health'
      | healthF > 100 = 100
      | healthF >   0 = healthF
      | n `is` Endure = 1
      | otherwise     = 0

-- | Sets 'health', restricting the value within [0, 100].
setHealth :: Int -> Ninja -> Ninja
setHealth = adjustHealth . const

-- | Sacrifices some amount of the target's 'N.health' down to a minimum.
sacrifice :: Int -> Int -> Ninja -> Ninja
sacrifice minhp hp = adjustHealth $ max minhp . (- hp)

-- | Applies 'Class.TurnBased.decr' to all of a @Ninja@'s 'Class.TurnBased'
-- types.
decrement :: Ninja -> Ninja
decrement n = processSkills $ processEffects
    n { N.defense   = mapMaybe TurnBased.decrement n.defense
      , N.statuses  = mapMaybe TurnBased.decrement n.statuses
      , N.barrier   = mapMaybe TurnBased.decrement n.barrier
      , N.channels  = mapMaybe (TurnBased.decrement . setNotNew) n.channels
      , N.traps     = mapMaybe TurnBased.decrement n.traps
      , N.copies    = (TurnBased.decrement =<<) <$> n.copies
      , N.cooldowns = (max 0 . subtract 1) <$> n.cooldowns
      , N.acted     = False
      }
  where
    setNotNew chan = chan { Channel.new = False }

addTrap :: Trap -> Ninja -> Ninja
addTrap trap n
  | any (conflicts trap) n.traps = n
  | otherwise                    = n { N.traps = trap : n.traps }
  where
    conflicts = (==) `on` \Trap{user, direction, trigger, classes, dur, name} ->
        (user, direction, trigger, classes, dur, name)

checkEffects :: [Effect] -> Ninja -> Ninja
checkEffects [] n = n
checkEffects _  n = processEffects n

addStatus :: Status -> Ninja -> Ninja
addStatus st n = checkEffects st.effects $ n { N.statuses = st .: n.statuses }

checkDestructibleEffects :: [Destructible] -> Ninja -> Ninja
checkDestructibleEffects xs n
  | any hasEffects xs = processEffects n
  | otherwise         = n
  where
   hasEffects Destructible{effects} = not $ null effects

addBarrier :: Destructible -> Ninja -> Ninja
addBarrier barrier n = case barrier.amount `compare` 0 of
    LT -> n { N.defense = Destructible.negate barrier .: n.defense }
    EQ -> n
    GT -> checkEffects barrier.effects n { N.barrier = barrier .: n.barrier }

addDefense :: Destructible -> Ninja -> Ninja
addDefense defense n = case defense.amount `compare` 0 of
    LT -> n { N.barrier = Destructible.negate defense .: n.barrier }
    EQ -> n
    GT -> checkEffects defense.effects n { N.defense = defense .: n.defense }

increaseDefense :: Int -- ^ 'Destructible.amount'.
                -> ID -- ^ 'Destructible.name'.
                -> Ninja -> Ninja
increaseDefense amount defenseID n = n { N.defense = addFirst n.defense }
  where
    addFirst [] = []
    addFirst (x:xs)
      | ID.from x == defenseID = addAmount x : xs
      | otherwise              = x : addFirst xs
    addAmount x = x { Destructible.amount = amount + x.amount }

removeDefense :: ID -- ^ 'Destructible.name'.
              -> Ninja -> Ninja
removeDefense defenseID n = processEffects
    n { N.defense = filter ((/= defenseID) . ID.from) n.defense }

clearBarrier :: Ninja -> Ninja
clearBarrier n = checkDestructibleEffects n.barrier n { N.barrier = mempty }

clearDefense :: Ninja -> Ninja
clearDefense n = checkDestructibleEffects n.defense n { N.defense = mempty }

-- | Deletes matching 'statuses'.
clear :: ID -- ^ 'Status.name'.
      -> Ninja -> Ninja
clear statusID = modifyStatuses . filter $ (/= statusID) . ID.from

-- | Deletes matching 'traps'.
clearTrap :: ID -- ^ 'Trap.name'.
          -> Ninja -> Ninja
clearTrap trapID n = n { N.traps = filter ((/= trapID) . ID.from) n.traps }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearTraps :: Trigger -> Ninja -> Ninja
clearTraps tr n = n { N.traps = filter ((/= tr) . Trap.trigger) n.traps }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearAnyTraps :: HashSet Trigger -> Ninja -> Ninja
clearAnyTraps trs n
  | null n.traps || null trs = n
  | otherwise = n { N.traps = filter ((∉ trs) . Trap.trigger) n.traps }

-- | Adds channels with a specific target.
addChannels :: Skill -> Slot -> Ninja -> Ninja
addChannels Skill{dur = Instant} _ n = n
addChannels skill@Skill{dur} target n = n { N.channels = chan : n.channels }
  where
    chan = Channel
        { target
        , skill = skill { Skill.require = mempty }
        , dur   = TurnBased.increment dur
        , new   = True
        }

-- | Deletes matching 'channels'.
cancelChannel :: ID -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel(ID.fromOwner -> channelID) n =
    n { N.channels = filter ((/= channelID) . ID.from) n.channels }

-- | Deletes matching 'channels' if they are not 'Channel.new'.
cancelOldChannel :: ID -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelOldChannel (ID.fromOwner -> channelID) n =
    n { N.channels = filter retain n.channels }
  where
    retain Channel{new = True} = True
    retain channel = channelID /= ID.from channel

-- | Copies all 'Skill's from a source into 'N.copies'.
copyAll :: Duration -- ^ 'Copy.dur'.
        -> Ninja -- ^ Person whose skills are being copied.
        -> Ninja -> Ninja
copyAll dur Ninja{skills} n = n { N.copies = cop <$> skills }
  where
    dur'
      | Parity.even dur = dur
      | otherwise       = succ dur
    cop skill = Just Copy { skill, dur = dur' }

-- | Copies a matching 'Skill' from a source into 'N.copies'.
copy :: Duration -- ^ 'Copy.dur'.
      -> [Int] -- ^ Skill slots, in the range @[0, length 'N.skills')@.
      -> Skill -- ^ 'Copy.skill'.
      -> Ninja -> Ninja
copy dur slots skill n =
    n { N.copies = fromList . toList $ foldl' go seqCopies slots }
  where
    seqCopies = fromList $ toList n.copies
    go acc slot = Seq.update slot (Just Copy { skill, dur }) acc

filterEffects :: (Slot -> Effect -> Bool) -> Ninja -> Ninja
filterEffects predicate n = modifyStatuses (mapMaybe f) n
  where
    f st@Status{classes, effects, user}
      | null effects          = Just st
      | Unremovable ∈ classes = Just st
      | null kept             = Nothing
      | otherwise             = Just st { Status.effects = kept }
      where
        kept = filter (predicate user) effects

-- | Removes harmful effects. Does not work if the target has 'Plague'.
cure :: (Effect -> Bool) -> Ninja -> Ninja
cure match n
  | n `is` Plague = n
  | otherwise     = filterEffects keep n
  where
    keep user effect = user == n.slot
                    || Effect.helpful effect
                    || Effect.sticky effect
                    || not (match effect)

-- | Cures 'Bane' 'statuses'.
cureBane :: Ninja -> Ninja
cureBane n
  | n `is` Plague = n
  | otherwise     = modifyStatuses (filter keep) n
  where
    keep Status{classes, user} = n.slot == user
                                 || Bane ∉ classes
                                 || Unremovable ∈ classes

kill :: Bool -- ^ Can be prevented by 'Endure'.
     -> Ninja -> Ninja
kill endurable n
  | endurable = setHealth 0 n
  | otherwise = clearTraps Resurrect $ n { N.health = 0 }

-- | Cleans up effects on a ninja who has died.
bury :: Ninja -> Ninja
bury n
  | N.alive n = n
  | otherwise = modifyAll onlyNecromancy
                n { N.channels = onlyNecromancy n.channels }
  where
    onlyNecromancy :: ∀ o. (IsSequence o, Classed (Element o)) => o -> o
    onlyNecromancy = filter $ (Necromancy ∈) . getClasses

-- | Extends the duration of matching 'statuses'.
prolong :: Duration -- ^ Added to 'Status.dur'.
        -> ID -- ^ 'Status.name'.
        -> Ninja -> Ninja
prolong dur statusID = prolongIf ((== statusID) . ID.from) dur

-- | Extends the duration of matching 'statuses' that are 'Controlled'.
prolongControlled :: Duration -- ^ Added to 'Status.dur'.
        -> ID -- ^ 'Status.name'.
        -> Ninja -> Ninja
prolongControlled dur statusID = prolongIf matches dur
  where
    matches st = Controlled ∈ st.classes && ID.from st == statusID

prolongIf :: (Status -> Bool) -> Duration -> Ninja -> Ninja
prolongIf condition dur n
  | dur < 0 = processEffects n'
  | otherwise = n'
  where
    n' = n { N.statuses = mapMaybe doProlong n.statuses }
    doProlong st
      | condition st = prolong' dur st
      | otherwise    = Just st

-- | Extends the duration of a single 'Status'.
prolong' :: Duration -- ^ Added to 'Status.dur'.
         -> Status -> Maybe Status
prolong' _ st@Status{dur = Permanent} = Just st
prolong' Permanent st = Just st { Status.dur = Permanent }
prolong' (Duration dur) st
  | statusDur' < 0 = Nothing
  | otherwise      = Just
        st { Status.dur    = statusDur'
           , Status.maxDur = max st.maxDur statusDur'
           }
    where
      statusDur' = st.dur + Duration dur'
      dur'
        | odd $ sync st.dur + dur = dur
        | dur < 0                 = dur + 1
        | otherwise               = dur - 1

prolongChannel :: Duration -> ID -> Ninja -> Ninja
prolongChannel dur (ID.fromOwner -> channelID) n =
    n { N.channels = f <$> n.channels }
  where
    f chan
      | ID.from chan /= channelID || not (prolongs chan.dur) = chan
      | otherwise = TurnBased.addDur dur chan
    prolongs (Ongoing (Duration i)) = i >= 0
    prolongs (Action  (Duration i)) = i >= 0
    prolongs _                      = False

renameChannels :: (Text -> Text) -> Ninja -> Ninja
renameChannels rename n = n { N.channels = f <$> n.channels }
  where
    f chan@Channel{new = True} = chan
    f chan@Channel{skill}      = chan
        { Channel.skill = skill { Skill.name = rename skill.name } }

-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge = filterEffects keep
  where
    keep _ effect = Effect.sticky effect || not (Effect.helpful effect)

-- | Resets the duration of matching 'statuses' to their 'Status.maxDur'.
refresh :: ID -- ^ 'Status.name'.
        -> Ninja -> Ninja
refresh statusID n = n { N.statuses = f <$> n.statuses }
  where
    f st
      | ID.from st == statusID = st { Status.dur = st.maxDur }
      | otherwise              = st

-- | Replicates 'removeStack'.
removeStacks :: Int -- ^ Subtracted from 'Status.amount'.
             -> ID -- ^ 'Status.name'.
             -> Ninja -> Ninja
removeStacks i statusID = modifyStatuses $ Status.remove i statusID

-- | Resets 'charges' to @mempty@s.
rechargeAll :: Ninja -> Ninja
rechargeAll n = n { N.charges = mempty }

-- | Resets an element in 'charges'.
recharge :: ID -> Ninja -> Ninja
recharge ID{name, owner} n = n { N.charges = deleteMap key n.charges }
  where
    key = Skill.Key name owner

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
spendCharge :: Skill -> Ninja -> Ninja
spendCharge skill n =
    n { N.charges = insertWith (+) (Skill.key skill) 1 n.charges }
