-- | 'Ninja' processing.
module Game.Engine.Ninjas
  ( skills, getSkill
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
  , addOwnDefense
  , addDefense
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
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Copy (Copy(Copy))
import qualified Game.Model.Copy as Copy
import           Game.Model.Defense (Defense(Defense))
import qualified Game.Model.Defense as Defense
import           Game.Model.Duration (Duration(..), sync)
import           Game.Model.Effect (Amount(..), Effect(..))
import qualified Game.Model.Effect as Effect
import           Game.Model.Ninja (Ninja(..), is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Requirement (Requirement(..))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status
import qualified Game.Model.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import           Util ((<$>.), (—), (!?), (∈), (∉), intersects)

headOr :: ∀ a. a -> [a] -> a
headOr x []    = x
headOr _ (x:_) = x

alternate :: Ninja -> [Int]
alternate n = findAlt <$> toList (Character.skills $ character n)
  where
    findAlt (base:|alts) = headOr 0 do
        Alternate name alt <- effects n
        guard $ name == Skill.name base
        maybe empty (return . (+ 1)) $ findIndex ((== alt) . Skill.name) alts

processAlternates :: Ninja -> Ninja
processAlternates n = n { alternates = fromList $ alternate n }

-- | Cycles a skill through its list of alternates.
nextAlternate :: Text -> Ninja -> Maybe Text
nextAlternate baseName n = do
    alts <- find ((== baseName) . Skill.name . head) .
            toList . Character.skills $ character n
    alt  <- filterAlt $ tail alts
    return $ Skill.name alt
  where
    filterAlt = headOr headMay do
        Alternate name alt <- effects n
        guard $ name == baseName
        return $ headMay . drop 1 . dropWhile ((/= alt) . Skill.name)

-- | Applies 'skill' to a @Skill@ and further modifies it due to 'Ninja.copies'
-- and 'Skill.require'ments.
getSkill :: Int -> Ninja -> Maybe Skill
getSkill s n
  | n `is` Swap = Skills.swap <$> base
  | otherwise   = base
  where
    base = Skills.change n . Requirement.usable True n
           <$> ((Copy.skill <$> join (copies n !? s)) <|> Ninja.baseSkill s n)

-- | All four skill slots of a @Ninja@ modified by 'skill'.
skills :: Ninja -> [Skill]
skills n = catMaybes $ flip getSkill n <$> [0..Ninja.numSkills n - 1]

-- | Modifies @Effect@s when they are first added to a @Ninja@ due to @Effect@s
-- already added.
apply :: Ninja -> Ninja -> [Effect] -> [Effect]
apply n nt = adjustEffect <$>. filter keepEffects
  where
    adjustEffect (Reduce cla Flat x) = Reduce cla Flat $ x - Effects.unreduce n
    adjustEffect f                   = f
    keepEffects Invulnerable{}       = not $ nt `is` Expose
    keepEffects _                    = True

-- | Fills 'Ninja.effects' with the effects of 'Ninja.statuses', modified by
-- 'NoIgnore', 'Seal', 'Boost', and so on.
processEffects :: Ninja -> Ninja
processEffects n = n { effects = process =<< baseStatuses }
  where
    nSlot         = slot n
    baseStatuses  = statuses n
    unmodEffects  = Status.effects =<< baseStatuses
    noIgnore      = NoIgnore ∈ unmodEffects
    baseEffects
      | noIgnore  = filter (not . Effect.isIgnore) unmodEffects
      | otherwise = unmodEffects
    enraged       = Enrage ∈ baseEffects
    sealed        = not enraged && Seal ∈ baseEffects
    boostAmount
      | sealed    = 1
      | otherwise = product $ 1 : [x | Boost x <- baseEffects]
    process Status{user, effects, amount} =
        replicate (boost * amount) =<< filter allow efs
      where
        boost
          | user == nSlot            = 1
          | Parity.allied nSlot user = boostAmount
          | otherwise                = 1
        efs
          | sealed        = filter (not . Effect.helpful) effects
          | user == nSlot = effects
          | enraged       = filter Effect.bypassEnrage effects
          | otherwise     = effects
        allow (Reduce _ _ x) = x <= 0 || enraged || not (Expose ∈ baseEffects)
        allow (Bleed _ _ x)  = x >= 0 || enraged || not (Expose ∈ baseEffects)
        allow (Effect.isDisable -> True) = sealed || not (Focus ∈ baseEffects)
        allow (Effect.isIgnore -> True) = not noIgnore
        allow _ = True

-- | Alters 'statuses' and then calls 'processEffects'.
modifyStatuses :: ([Status] -> [Status]) -> Ninja -> Ninja
modifyStatuses f n = processEffects n { statuses = f $ statuses n }

-- | Factory resets a @Ninja@ to its starting values.
factory :: Ninja -> Ninja
factory n = Ninja.new (slot n) $ character n

-- | Modifies 'health', restricting the value within ['Ninja.minHealth', 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth f n =
    n { health = min 100 . max (Ninja.minHealth n) . f $ health n }

-- | Sets 'health', restricting the value within ['Ninja.minHealth', 100].
setHealth :: Int -> Ninja -> Ninja
setHealth = adjustHealth . const

-- | Sacrifices some amount of the target's 'Ninja.health' down to a minimum.
sacrifice :: Int -> Int -> Ninja -> Ninja
sacrifice minhp hp = adjustHealth $ max minhp . (— hp)

-- | Applies 'Class.TurnBased.decr' to all of a @Ninja@'s 'Class.TurnBased'
-- types.
decr :: Ninja -> Ninja
decr n@Ninja{..} = processAlternates $ processEffects n
    { defense   = mapMaybe TurnBased.decr defense
    , statuses  = mapMaybe TurnBased.decr statuses
    , barrier   = mapMaybe TurnBased.decr barrier
    , channels  = decrChannels
    , traps     = mapMaybe TurnBased.decr traps
    , delays    = mapMaybe TurnBased.decr delays
    , newChans  = mempty
    , copies    = (TurnBased.decr =<<) <$> copies
    , cooldowns = (max 0 . subtract 1) `omap` cooldowns
    , acted     = False
    }
  where
    decrChannels       = mapMaybe TurnBased.decr $ newChans ++ channels

addStatus :: Status -> Ninja -> Ninja
addStatus st = modifyStatuses $ Classed.nonStack st st

addOwnStacks :: Duration -- ^ 'Status.dur'.
             -> Text -- ^ 'Status.name'.
             -> Int -- ^ Skill index in 'Character.skills'.
             -> Int -- ^ Index in skill in 'Character.skills'.
             -> Int -- ^ 'Status.amount'.
             -> Ninja -> Ninja
addOwnStacks dur name s alt i n =
    addStatus st { Status.name    = name
                 , Status.classes = insertSet Unremovable $ Status.classes st
                 , Status.amount  = i
                 } n
  where
    st = Status.new (slot n) dur $ Character.skills (character n) !! s !! alt

addOwnDefense :: Duration -- ^ 'Defense.dur'.
              -> Text -- ^ 'Defense.name'.
              -> Int -- ^ 'Defense.amount'.
              -> Ninja -> Ninja
addOwnDefense dur name amount n = n { defense = d : defense n }
  where
    d = Defense { amount, name, user = slot n, dur = succ dur }

addDefense :: Int -- ^ 'Defense.amount'.
           -> Text -- ^ 'Defense.name'.
           -> Slot -- ^ 'Defense.user'.
           -> Ninja -> Ninja
addDefense amount name user n =
    n { defense = Labeled.mapFirst addAmount name user $ defense n }
  where
    addAmount x = x { Defense.amount = amount + Defense.amount x }

removeDefense :: Text -- ^ 'Defense.name'.
              -> Slot -- ^ 'Defense.user'.
              -> Ninja -> Ninja
removeDefense name user n =
    n { defense = filter (not . Labeled.match name user) $ defense n }

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
    n { traps = filter (not . Labeled.match name user) $ traps n }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearTraps :: Trigger -> Ninja -> Ninja
clearTraps tr n = n { traps = filter ((/= tr) . Trap.trigger) $ traps n }

-- | Adds channels with a specific target.
addChannels :: Skill -> Slot -> Ninja -> Ninja
addChannels skill target n
  | chan == Instant || dur == 1                     = n
  | Effects.stun n `intersects` Skill.classes skill = n
  | otherwise = n { newChans = chan' : newChans n }
  where
    chan  = Skill.dur skill
    dur   = succ $ TurnBased.getDur chan
    chan' = Channel { target
                    , skill = skill { Skill.require = Usable }
                    , dur   = TurnBased.setDur dur chan
                    }

-- | Deletes matching 'channels'.
cancelChannel :: Text -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel name n = n { channels = f $ channels n
                         , newChans = f $ newChans n
                         }
  where
    f = filter $ (/= name) . Skill.name . Channel.skill

-- | Copies all 'Skill's from a source into 'Ninja.copies'.
copyAll :: Duration -- ^ 'Copy.dur'.
        -> Ninja -- ^ Person whose skills are being copied.
        -> Ninja -> Ninja
copyAll dur source n = n { copies = fromList $ cop <$> skills source }
  where
    dur'
      | Parity.even dur = dur
      | otherwise       = succ dur
    cop skill = Just Copy { skill, dur = dur' }

-- | Copies a matching 'Skill' from a source into 'Ninja.copies'.
copy :: Duration -- ^ 'Copy.dur'.
      -> [Int] -- ^ Skill slots, in the range @[0, 'Ninja.numSkills')@.
      -> Skill -- ^ 'Copy.skill'.
      -> Ninja -> Ninja
copy dur slots skill n = n { copies = foldl' go (copies n) slots }
  where
    go acc slot = Seq.update slot (Just Copy { skill, dur }) acc

-- | Removes harmful effects. Does not work if the target has 'Plague'.
cure :: (Effect -> Bool) -> Ninja -> Ninja
cure match n
  | n `is` Plague = n
  | otherwise     = modifyStatuses (mapMaybe cure') n
  where
    keep a = Effect.helpful a || not (match a)
    cure' st
      | Status.user st == slot n           = Just st
      | null $ Status.effects st           = Just st
      | Unremovable ∈ Status.classes st    = Just st
      | not $ any keep $ Status.effects st = Nothing
      | otherwise = Just st { Status.effects = filter keep $ Status.effects st }

-- | Cures 'Bane' 'statuses'.
cureBane :: Ninja -> Ninja
cureBane n
  | n `is` Plague = n
  | otherwise     = modifyStatuses (filter keep) n
  where
    keep st = Bane ∉ Status.classes st || slot n == Status.user st

kill :: Bool -- ^ Can be prevented by 'Endure'.
     -> Ninja -> Ninja
kill endurable n
  | endurable = setHealth 0 n
  | otherwise = clearTraps OnRes $ n { health = 0 }

-- | Extends the duration of matching 'statuses'.
prolong :: Duration -- ^ Added to 'Status.dur'.
        -> Text -- ^ 'Status.name'.
        -> Slot -- ^ 'Status.user'.
        -> Ninja -> Ninja
prolong dur name src n =
    n { statuses = mapMaybe (prolong' dur name src) $ statuses n }

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
  | otherwise                        = Just st
      { Status.dur    = statusDur'
      , Status.maxDur = max (Status.maxDur st) statusDur'
      }
    where
      statusDur' = Status.dur st + Duration dur'
      dur'
        | odd $ sync (Status.dur st) + dur = dur
        | dur < 0                          = dur + 1
        | otherwise                        = dur - 1

prolongChannel :: Duration -> Text -> Ninja -> Ninja
prolongChannel dur name n = n { channels = f <$> channels n }
  where
    dur' chan = TurnBased.getDur chan + dur
    f chan
      | TurnBased.getDur chan <= 0              = chan
      | name /= Skill.name (Channel.skill chan) = chan
      | otherwise = TurnBased.setDur (dur' chan) chan

renameChannels :: (Text -> Text) -> Ninja -> Ninja
renameChannels rename n = n { channels = f <$> channels n }
  where
    f chan = chan
        { Channel.skill = skill { Skill.name = rename $ Skill.name skill } }
      where
        skill = Channel.skill chan

-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge = modifyStatuses (doPurge <$>)
  where
    keep ef = Effect.sticky ef || not (Effect.helpful ef)
    doPurge st
      | Unremovable ∈ Status.classes st = st
      | otherwise = st { Status.effects = filter keep $ Status.effects st }

-- | Resets the duration of matching 'statuses' to their 'Status.maxDur'.
refresh :: Text -- ^ 'Status.name'.
        -> Slot -- ^ 'Status.user'.
        -> Ninja -> Ninja
refresh name user n = n { statuses = f <$> statuses n }
  where
    f st
      | Labeled.match name user st = st { Status.dur = Status.maxDur st }
      | otherwise                    = st

-- | Deletes one matching 'Status'.
removeStack :: Text -- ^ 'Status.name'.
            -> Ninja -> Ninja
removeStack name n = modifyStatuses (Status.remove 1 name $ slot n) n

-- | Replicates 'removeStack'.
removeStacks :: Text -- ^ 'Status.name'.
             -> Int -- ^ Subtracted from 'Status.amount'.
             -> Slot -- ^ 'Status.user'.
             -> Ninja -> Ninja
removeStacks name i user = modifyStatuses $ Status.remove i name user

-- | Resets 'charges' to @mempty@s.
rechargeAll :: Ninja -> Ninja
rechargeAll n = n { charges = mempty }

-- | Resets an element in 'charges'.
recharge :: Text -> Slot -> Ninja -> Ninja
recharge name owner n = n { charges = deleteMap key $ charges n }
  where
    key = Skill.Key name owner
