-- | 'Ninja' processing.
module Game.Engine.Ninjas
  ( skills, getSkill, hasSkill
  , modifyStatuses
  , apply
  , processEffects

  , nextAlternate

  , decr

  , factory

  , adjustHealth
  , sacrifice
  , setHealth
  , kill

  , addStatus
  , addOwnStacks
  , addBarrier
  , addDefense
  , increaseDefense
  , removeDefense

  , clear
  , clearTrap
  , clearTraps
  , cure
  , cureBane
  , purge
  , removeStack
  , removeStacks

  , addChannels
  , cancelChannel

  , copy, copyAll
  , recharge, rechargeAll

  , prolong
  , prolong'
  , prolongChannel
  , renameChannels
  , refresh
  ) where

import ClassyPrelude

import           Data.List (findIndex)
import           Data.List.NonEmpty ((!!))
import qualified Data.Sequence as Seq

import qualified Class.Classed as Classed
import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
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
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import           Util ((!?), (∈), (∉), intersects)

headOr :: ∀ a. a -> [a] -> a
headOr x []    = x
headOr _ (x:_) = x

alternate :: Ninja -> [Int]
alternate Ninja{character = Character{skills = sk}, effects} =
    findAlt <$> toList sk
  where
    findAlt (base:|alts) = headOr 0
        [i + 1 | Alternate name alt <- effects
               , name == Skill.name base
               , i <- maybeToList $ findIndex ((== alt) . Skill.name) alts
               ]

processAlternates :: Ninja -> Ninja
processAlternates n = n { N.alternates = fromList $ alternate n }

-- | Cycles a skill through its list of alternates.
nextAlternate :: Text -> Ninja -> Maybe Text
nextAlternate baseName Ninja{character = Character{skills = sk}, effects} = do
    alts <- find ((== baseName) . Skill.name . head) $ toList sk
    alt  <- filterAlt $ tail alts
    return $ Skill.name alt
  where
    filterAlt = headOr headMay
        [ headMay . drop 1 . dropWhile ((/= alt) . Skill.name)
            | Alternate name alt <- effects
            , name == baseName
            ]

-- | Applies 'skill' to a @Skill@ and further modifies it due to 'N.copies'
-- and 'Skill.require'ments.
getSkill :: Int -> Ninja -> Maybe Skill
getSkill s n
  | n `is` Swap = Skills.swap <$> base
  | otherwise   = base
  where
    base = Requirement.usable True n . Skills.change n
         <$> ((Copy.skill <$> join (N.copies n !? s)) <|> N.baseSkill s n)

-- | Searches 'skills'.
hasSkill :: Text -- ^ `Skill.name`.
         -> Ninja -> Bool
hasSkill name n = any ((== name) . Skill.name) $ skills n

-- | All four skill slots of a @Ninja@ modified by 'skill'.
skills :: Ninja -> [Skill]
skills n = catMaybes $ flip getSkill n <$> [0..N.numSkills n - 1]

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
processEffects n@Ninja{barrier, defense, statuses} = n { N.effects = processed }
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

-- | Factory resets a @Ninja@ to its starting values.
factory :: Ninja -> Ninja
factory n = N.new (N.slot n) $ N.character n

-- | Modifies 'health', restricting the value within ['N.minHealth', 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth f n =
    n { N.health = min 100 . max (N.minHealth n) . f $ N.health n }

-- | Sets 'health', restricting the value within ['N.minHealth', 100].
setHealth :: Int -> Ninja -> Ninja
setHealth = adjustHealth . const

-- | Sacrifices some amount of the target's 'N.health' down to a minimum.
sacrifice :: Int -> Int -> Ninja -> Ninja
sacrifice minhp hp = adjustHealth $ max minhp . (- hp)

-- | Applies 'Class.TurnBased.decr' to all of a @Ninja@'s 'Class.TurnBased'
-- types.
decr :: Ninja -> Ninja
decr n = processAlternates $ processEffects
    n { N.defense   = mapMaybe TurnBased.decr $ N.defense n
      , N.statuses  = mapMaybe TurnBased.decr $ N.statuses n
      , N.barrier   = mapMaybe TurnBased.decr $ N.barrier n
      , N.channels  = mapMaybe (TurnBased.decr . setNotNew) $ N.channels n
      , N.traps     = mapMaybe TurnBased.decr $ N.traps n
      , N.delays    = mapMaybe TurnBased.decr $ N.delays n
      , N.copies    = (TurnBased.decr =<<) <$> N.copies n
      , N.cooldowns = (max 0 . subtract 1) `omap` N.cooldowns n
      , N.acted     = False
      }
  where
    setNotNew chan = chan { Channel.new = False }

addStatus :: Status -> Ninja -> Ninja
addStatus st = modifyStatuses $ Classed.nonStack st

addOwnStacks :: Duration -- ^ 'Status.dur'.
             -> Text -- ^ 'Status.name'.
             -> Int -- ^ Skill index in 'Character.skills'.
             -> Int -- ^ Index in skill in 'Character.skills'.
             -> Int -- ^ 'Status.amount'.
             -> Ninja -> Ninja
addOwnStacks dur name s alt i n@Ninja{slot, character = Character{skills = sk}}
    = addStatus st n
  where
    skill = sk !! s !! alt
    st = (Status.new slot dur skill)
            { Status.name    = name
            , Status.classes = insertSet Unremovable $ Status.classes st
            , Status.amount  = i
            }

checkEffects :: [Effect] -> Ninja -> Ninja
checkEffects [] n = n
checkEffects _ n = processEffects n

addBarrier :: Destructible -> Ninja -> Ninja
addBarrier b@Destructible{amount, effects} n = case amount `compare` 0 of
    LT -> n { N.defense = Classed.nonStack (Destructible.negate b) $ N.defense n }
    EQ -> n
    GT -> checkEffects effects n { N.barrier = Classed.nonStack b $ N.barrier n }

addDefense :: Destructible -> Ninja -> Ninja
addDefense b@Destructible{amount, effects} n = case amount `compare` 0 of
    LT -> n { N.barrier = Classed.nonStack (Destructible.negate b) $ N.barrier n }
    EQ -> n
    GT -> checkEffects effects n { N.defense = Classed.nonStack b $ N.defense n }

increaseDefense :: Int -- ^ 'Destructible.amount'.
           -> Text -- ^ 'Destructible.name'.
           -> Slot -- ^ 'Destructible.user'.
           -> Ninja -> Ninja
increaseDefense amount name user n =
    n { N.defense = Labeled.mapFirst addAmount name user $ N.defense n }
  where
    addAmount x = x { Destructible.amount = amount + Destructible.amount x }

removeDefense :: Text -- ^ 'Destructible.name'.
              -> Slot -- ^ 'Destructible.user'.
              -> Ninja -> Ninja
removeDefense name user n = processEffects
    n { N.defense = filter (not . Labeled.match name user) $ N.defense n }

-- | Deletes matching 'statuses'.
clear :: Text -- ^ 'Status.name'.
      -> Slot -- ^ 'Status.user'.
      -> Ninja -> Ninja
clear name user = modifyStatuses . filter $ not . Labeled.match name user

-- | Deletes matching 'traps'.
clearTrap :: Text -- ^ 'Trap.name'.
          -> Slot -- ^ 'Trap.user'.
          -> Ninja -> Ninja
clearTrap name user n =
    n { N.traps = filter (not . Labeled.match name user) $ N.traps n }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearTraps :: Trigger -> Ninja -> Ninja
clearTraps tr n = n { N.traps = filter ((/= tr) . Trap.trigger) $ N.traps n }

-- | Adds channels with a specific target.
addChannels :: Skill -> Slot -> Ninja -> Ninja
addChannels skill target n
  | chan == Instant || dur == 1                     = n
  | Effects.stun n `intersects` Skill.classes skill = n
  | otherwise = n { N.channels = chan' : N.channels n }
  where
    chan  = Skill.dur skill
    dur   = succ $ TurnBased.getDur chan
    chan' = Channel
        { target
        , skill = skill { Skill.require = Usable }
        , dur   = TurnBased.setDur dur chan
        , new   = True
        }

-- | Deletes matching 'channels'.
cancelChannel :: Text -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel name n = n { N.channels = f $ N.channels n }
  where
    f = filter $ (/= name) . Skill.name . Channel.skill

-- | Copies all 'Skill's from a source into 'N.copies'.
copyAll :: Duration -- ^ 'Copy.dur'.
        -> Ninja -- ^ Person whose skills are being copied.
        -> Ninja -> Ninja
copyAll dur source n = n { N.copies = fromList $ cop <$> skills source }
  where
    dur'
      | Parity.even dur = dur
      | otherwise       = succ dur
    cop skill = Just Copy { skill, dur = dur' }

-- | Copies a matching 'Skill' from a source into 'N.copies'.
copy :: Duration -- ^ 'Copy.dur'.
      -> [Int] -- ^ Skill slots, in the range @[0, 'N.numSkills')@.
      -> Skill -- ^ 'Copy.skill'.
      -> Ninja -> Ninja
copy dur slots skill n = n { N.copies = foldl' go (N.copies n) slots }
  where
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
        -> Text -- ^ 'Status.name'.
        -> Slot -- ^ 'Status.user'.
        -> Ninja -> Ninja
prolong dur name src n =
    n { N.statuses = mapMaybe (prolong' dur name src) $ N.statuses n }

-- | Extends the duration of a single 'Status'.
prolong' :: Duration -- ^ Added to 'Status.dur'.
         -> Text -- ^ 'Status.name'.
         -> Slot -- ^ 'Status.user'.
         -> Status -> Maybe Status
prolong' Permanent _ _ st = Just st { Status.dur = Permanent }
prolong' (Duration dur) name user st
  | Status.dur st == Permanent       = Just st
  | not $ Labeled.match name user st = Just st
  | statusDur' < 0                   = Nothing
  | otherwise                        = Just
        st { Status.dur    = statusDur'
           , Status.maxDur = max (Status.maxDur st) statusDur'
           }
    where
      statusDur' = Status.dur st + Duration dur'
      dur'
        | odd $ sync (Status.dur st) + dur = dur
        | dur < 0                          = dur + 1
        | otherwise                        = dur - 1

prolongChannel :: Duration -> Text -> Ninja -> Ninja
prolongChannel dur name n = n { N.channels = f <$> N.channels n }
  where
    dur' chan = TurnBased.getDur chan + dur
    f chan
      | TurnBased.getDur chan <= 0              = chan
      | name /= Skill.name (Channel.skill chan) = chan
      | otherwise = TurnBased.setDur (dur' chan) chan

renameChannels :: (Text -> Text) -> Ninja -> Ninja
renameChannels rename n = n { N.channels = f <$> N.channels n }
  where
    f chan@Channel{skill} = chan
        { Channel.skill = skill { Skill.name = rename $ Skill.name skill } }

-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge = filterEffects keep
  where
    keep _ effect = Effect.sticky effect || not (Effect.helpful effect)

-- | Resets the duration of matching 'statuses' to their 'Status.maxDur'.
refresh :: Text -- ^ 'Status.name'.
        -> Slot -- ^ 'Status.user'.
        -> Ninja -> Ninja
refresh name user n = n { N.statuses = f <$> N.statuses n }
  where
    f st
      | Labeled.match name user st = st { Status.dur = Status.maxDur st }
      | otherwise                    = st

-- | Deletes one matching 'Status'.
removeStack :: Text -- ^ 'Status.name'.
            -> Ninja -> Ninja
removeStack name n = modifyStatuses (Status.remove 1 name $ N.slot n) n

-- | Replicates 'removeStack'.
removeStacks :: Text -- ^ 'Status.name'.
             -> Int -- ^ Subtracted from 'Status.amount'.
             -> Slot -- ^ 'Status.user'.
             -> Ninja -> Ninja
removeStacks name i user = modifyStatuses $ Status.remove i name user

-- | Resets 'charges' to @mempty@s.
rechargeAll :: Ninja -> Ninja
rechargeAll n = n { N.charges = mempty }

-- | Resets an element in 'charges'.
recharge :: Text -> Slot -> Ninja -> Ninja
recharge name owner n = n { N.charges = deleteMap key $ N.charges n }
  where
    key = Skill.Key name owner
