
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

  , addTrap
  , addStatus
  , addBarrier
  , addDefense
  , increaseDefense
  , removeDefense
  , clearBarrier
  , clearDefense

  , clear
  , clearTrap
  , clearTraps
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

import           Class.Classed (Classed)
import qualified Class.Parity as Parity
import           Class.Stackable ((.:))
import           Class.TurnBased (TurnBased)
import qualified Class.TurnBased as TurnBased
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Skills as Skills
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Channel as Channel
import           Game.Model.Character (Character(Character))
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
import           Game.Model.ID (HasID, ID)
import qualified Game.Model.ID as ID
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Requirement (Requirement(..))
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
processSkills n@Ninja{copies, slot, character = Character{skills}}
    = n { N.skills = zipWith getSkill (toNullable skills) copies }
  where
    getSkill (base:|alts) mcopy = Requirement.usable True n
        . Skills.change n
        $ fromMaybe (own base) $ copied <|> (own <$> alternate)
      where
        own x     = x { Skill.owner = slot }
        copied    = Copy.skill <$> mcopy
        alternate = do
            alt <- Effects.alternate (Skill.name base) n
            find ((== alt) . Skill.name) alts

-- | Cycles a skill through its list of alternates.
nextAlternate :: Text -> Ninja -> Maybe Text
nextAlternate baseName n@Ninja{character = Character{skills}} = do
    _:|alts <- find ((== baseName) . Skill.name . head) skills
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
    flattenStatusEffects Status{effects, amount} = replicate amount =<< effects
    allEffects = (flattenStatusEffects =<< statuses)
              ++ (Destructible.effects =<< barrier ++ defense)

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
modifyStatuses f n = processEffects n { N.statuses = f $ N.statuses n }

modifyAll :: (∀ a. (Classed a, HasID a, TurnBased a) => [a] -> [a])
          -> Ninja -> Ninja
modifyAll f n = processEffects n { N.defense  = f $ N.defense n
                                 , N.barrier  = f $ N.barrier n
                                 , N.statuses = f $ N.statuses n
                                 , N.traps    = f $ N.traps n
                                 }

-- | Factory resets a @Ninja@ to its starting values.
factory :: Ninja -> Ninja
factory n = N.new (N.slot n) $ N.character n

-- | Modifies 'health', restricting the value within [0, 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth f n =
    n { N.health = min 100 . max minHealth . f $ N.health n }
  where
    minHealth
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
    n { N.defense   = mapMaybe TurnBased.decrement $ N.defense n
      , N.statuses  = mapMaybe TurnBased.decrement $ N.statuses n
      , N.barrier   = mapMaybe TurnBased.decrement $ N.barrier n
      , N.channels  = mapMaybe (TurnBased.decrement . setNotNew) $ N.channels n
      , N.traps     = mapMaybe TurnBased.decrement $ N.traps n
      , N.copies    = (TurnBased.decrement =<<) <$> N.copies n
      , N.cooldowns = (max 0 . subtract 1) <$> N.cooldowns n
      , N.acted     = False
      }
  where
    setNotNew chan = chan { Channel.new = False }

addTrap :: Trap -> Ninja -> Ninja
addTrap trap n@Ninja{traps}
  | any (conflicts trap) traps = n
  | otherwise = n { N.traps = trap : traps }
  where
    conflicts = (==) `on` \Trap{user, direction, trigger, classes, dur, name} ->
        (user, direction, trigger, classes, dur, name)

checkEffects :: [Effect] -> Ninja -> Ninja
checkEffects [] n = n
checkEffects _  n = processEffects n

addStatus :: Status -> Ninja -> Ninja
addStatus st@Status{effects} n = checkEffects effects
    $ n { N.statuses = st .: N.statuses n }

checkDestructibleEffects :: [Destructible] -> Ninja -> Ninja
checkDestructibleEffects xs n
  | any hasEffects xs = processEffects n
  | otherwise         = n
  where
   hasEffects Destructible{effects = []} = False
   hasEffects _                          = True

addBarrier :: Destructible -> Ninja -> Ninja
addBarrier b@Destructible{amount, effects} n = case amount `compare` 0 of
    LT -> n { N.defense = Destructible.negate b .: N.defense n }
    EQ -> n
    GT -> checkEffects effects n { N.barrier = b .: N.barrier n }

addDefense :: Destructible -> Ninja -> Ninja
addDefense b@Destructible{amount, effects} n = case amount `compare` 0 of
    LT -> n { N.barrier = Destructible.negate b .: N.barrier n }
    EQ -> n
    GT -> checkEffects effects n { N.defense = b .: N.defense n }

increaseDefense :: Int -- ^ 'Destructible.amount'.
                -> ID -- ^ 'Destructible.name'.
                -> Ninja -> Ninja
increaseDefense amount defenseID n = n { N.defense = addFirst $ N.defense n }
  where
    addFirst [] = []
    addFirst (x:xs)
      | ID.from x == defenseID = addAmount x : xs
      | otherwise              = x : addFirst xs
    addAmount x = x { Destructible.amount = amount + Destructible.amount x }

removeDefense :: ID -- ^ 'Destructible.name'.
              -> Ninja -> Ninja
removeDefense defenseID n = processEffects
    n { N.defense = filter ((/= defenseID) . ID.from) $ N.defense n }

clearBarrier :: Ninja -> Ninja
clearBarrier n@Ninja{barrier} = checkDestructibleEffects barrier
    $ n { N.barrier = [] }

clearDefense :: Ninja -> Ninja
clearDefense n@Ninja{defense} = checkDestructibleEffects defense
    $ n { N.defense = [] }

-- | Deletes matching 'statuses'.
clear :: ID -- ^ 'Status.name'.
      -> Ninja -> Ninja
clear statusID = modifyStatuses . filter $ (/= statusID) . ID.from

-- | Deletes matching 'traps'.
clearTrap :: ID -- ^ 'Trap.name'.
          -> Ninja -> Ninja
clearTrap trapID n =
    n { N.traps = filter ((/= trapID) . ID.from) $ N.traps n }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearTraps :: Trigger -> Ninja -> Ninja
clearTraps tr n = n { N.traps = filter ((/= tr) . Trap.trigger) $ N.traps n }

-- | Adds channels with a specific target.
addChannels :: Skill -> Slot -> Ninja -> Ninja
addChannels Skill{dur = Instant} _ n = n
addChannels skill@Skill{dur} target n = n { N.channels = chan : N.channels n }
  where
    chan = Channel
        { target
        , skill = skill { Skill.require = Usable }
        , dur   = TurnBased.increment dur
        , new   = True
        }

-- | Deletes matching 'channels'.
cancelChannel :: ID -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel(ID.fromOwner -> channelID) n =
    n { N.channels = filter ((/= channelID) . ID.from) $ N.channels n }

-- | Deletes matching 'channels' if they are not 'Channel.new'.
cancelOldChannel :: ID -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelOldChannel (ID.fromOwner -> channelID) n =
    n { N.channels = filter retain $ N.channels n }
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
    seqCopies = fromList . toList $ N.copies n
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
cure match n@Ninja{slot}
  | n `is` Plague = n
  | otherwise     = filterEffects keep n
  where
    keep user effect = user == slot
                    || Effect.helpful effect
                    || Effect.sticky effect
                    || not (match effect)

-- | Cures 'Bane' 'statuses'.
cureBane :: Ninja -> Ninja
cureBane n@Ninja{slot}
  | n `is` Plague = n
  | otherwise     = modifyStatuses (filter keep) n
  where
    keep Status{classes, user} = slot == user
                                 || Bane ∉ classes
                                 || Unremovable ∈ classes

kill :: Bool -- ^ Can be prevented by 'Endure'.
     -> Ninja -> Ninja
kill endurable n
  | endurable = setHealth 0 n
  | otherwise = clearTraps OnRes $ n { N.health = 0 }

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
    matches st@Status{classes} = Controlled ∈ classes && ID.from st == statusID

prolongIf :: (Status -> Bool) -> Duration -> Ninja -> Ninja
prolongIf condition dur n
  | dur < 0 = processEffects n'
  | otherwise = n'
  where
    n' = n { N.statuses = mapMaybe doProlong $ N.statuses n }
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
           , Status.maxDur = max (Status.maxDur st) statusDur'
           }
    where
      statusDur' = Status.dur st + Duration dur'
      dur'
        | odd $ sync (Status.dur st) + dur = dur
        | dur < 0                          = dur + 1
        | otherwise                        = dur - 1

prolongChannel :: Duration -> ID -> Ninja -> Ninja
prolongChannel dur (ID.fromOwner -> channelID) n =
    n { N.channels = f <$> N.channels n }
  where
    f chan@Channel{dur = dur'}
      | prolongs dur' && ID.from chan == channelID = TurnBased.addDur dur chan
      | otherwise                                  = chan
    prolongs (Ongoing (Duration i)) = i >= 0
    prolongs (Action  (Duration i)) = i >= 0
    prolongs _                      = False

renameChannels :: (Text -> Text) -> Ninja -> Ninja
renameChannels rename n = n { N.channels = f <$> N.channels n }
  where
    f chan@Channel{new = True} = chan
    f chan@Channel{skill} = chan
        { Channel.skill = skill { Skill.name = rename $ Skill.name skill } }

-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge = filterEffects keep
  where
    keep _ effect = Effect.sticky effect || not (Effect.helpful effect)

-- | Resets the duration of matching 'statuses' to their 'Status.maxDur'.
refresh :: ID -- ^ 'Status.name'.
        -> Ninja -> Ninja
refresh statusID n = n { N.statuses = f <$> N.statuses n }
  where
    f st
      | ID.from st == statusID = st { Status.dur = Status.maxDur st }
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
recharge :: Text -> Slot -> Ninja -> Ninja
recharge name owner n = n { N.charges = deleteMap key $ N.charges n }
  where
    key = Skill.Key name owner

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
spendCharge :: Skill -> Ninja -> Ninja
spendCharge skill n =
    n { N.charges = insertWith (+) (Skill.key skill) 1 $ N.charges n }
