-- | 'Ninja' processing.
module Game.Engine.Ninjas
  ( skill, skills
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

  , copy, copyAll, copyAlternates
  , recharge, rechargeAll

  , prolong
  , prolong'
  , prolongChannel
  , refresh

  , kabuto
  ) where

import ClassyPrelude

import           Data.List (findIndex)
import           Data.List.NonEmpty ((!!))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import qualified Class.Classed as Classed
import qualified Class.Labeled as Labeled
import qualified Class.Parity as Parity
import qualified Class.TurnBased as TurnBased
import qualified Game.Engine.Effects as Effects
import           Game.Model.Channel (Channel(Channel), Channeling(..))
import qualified Game.Model.Channel as Channel
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Copy (Copy(Copy))
import qualified Game.Model.Copy as Copy
import           Game.Model.Defense (Defense(Defense))
import qualified Game.Model.Defense as Defense
import           Game.Model.Duration (Duration, incr, sync)
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
import           Util ((—), (!?), (∈), (∉))

headOr :: ∀ a. a -> [a] -> a
headOr x []    = x
headOr _ (x:_) = x

alternate :: Ninja -> [Int]
alternate n = findAlt <$> toList (Character.skills $ character n)
  where
    findAlt (base:|alts) = headOr 0 do
        Alternate name alt <- effects n
        guard $ name == Skill.name base
        maybeToList $ (+1) <$> findIndex ((alt ==) . Skill.name) alts

processAlternates :: Ninja -> Ninja
processAlternates n = n { alternates = fromList $ alternate n }

nextAlternate :: Text -> Ninja -> Maybe Text
nextAlternate baseName n = do
    alts <- find ((baseName ==) . Skill.name . head) .
            toList . Character.skills $ character n
    alt  <- filterAlt $ tail alts
    return $ Skill.name alt
  where
    filterAlt = headOr headMay do
        Alternate name alt <- effects n
        guard $ name == baseName
        return $ headMay . drop 1 . dropWhile ((alt /=) . Skill.name)

-- | Applies 'skill'' to a @Skill@ and further modifies it due to 'Ninja.copies'
-- and 'Skill.require'ments.
-- Invariant: With @Left x@, @x < 'Ninja.skillSize'@.
skill :: Either Int Skill -> Ninja -> Skill
skill (Right sk) n = Requirement.usable False n sk
skill (Left s)   n = Requirement.usable True n .
                     maybe (Ninja.baseSkill s n) Copy.skill .
                     join . (!? s) $ copies n

-- | All four skill slots of a @Ninja@ modified by 'skill'.
skills :: Ninja -> [Skill]
skills n = flip skill n . Left <$> [0 .. Ninja.skillSize - 1]

-- | Modifies @Effect@s when they are first added to a @Ninja@ due to @Effect@s
-- already added.
apply :: Ninja -> [Effect] -> [Effect]
apply n = map adjustEffect . filter keepEffects
  where
    adjustEffect (Reduce cla Flat x) = Reduce cla Flat $ x - Effects.unreduce n
    adjustEffect f                   = f
    keepEffects Invulnerable{}       = not $ n `is` Expose
    keepEffects _                    = True

-- | Fills 'Ninja.effects' with the effects of 'Ninja.statuses', modified by
-- 'Ignore', 'Seal', 'Boost', and so on.
processEffects :: Ninja -> Ninja
processEffects n = n { effects = baseStatuses >>= process }
  where
    nSlot         = slot n
    baseStatuses  = statuses n
    unmodEffects  = baseStatuses >>= Status.effects
    baseEffects
      | NoIgnore ∈ unmodEffects = filter (not . Effect.isIgnore) unmodEffects
      | otherwise               = unmodEffects
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
        allow _ = True

modifyStatuses :: ([Status] -> [Status]) -> Ninja -> Ninja
modifyStatuses f n = processEffects n { statuses = f $ statuses n }

-- | Factory resets a @Ninja@ to its starting values.
factory :: Ninja -> Ninja
factory n = Ninja.new (slot n) $ character n

-- | Modifies 'health', restricting the value within ['minHealth', 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth f n =
    n { health = min 100 . max (Ninja.minHealth n) . f $ health n }

-- | Sets 'health', restricting the value within ['minHealth', 100].
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
    , statuses  = foldStats $ mapMaybe TurnBased.decr statuses
    , barrier   = mapMaybe TurnBased.decr barrier
    , channels  = decrChannels
    , traps     = mapMaybe TurnBased.decr traps
    , delays    = mapMaybe TurnBased.decr delays
    , newChans  = mempty
    , copies    = (>>= TurnBased.decr) <$> copies
    , cooldowns = (max 0 . subtract 1) `omap` cooldowns
    , acted     = False
    }
  where
    decrChannels       = mapMaybe TurnBased.decr $ newChans ++ channels
    foldStats xs       = foldStat <$> group (sort xs)
    foldStat   (x:|[]) = x
    foldStat xs@(x:|_) = x { Status.amount = sum $ Status.amount <$> xs }

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
                 , Status.classes = Unremovable `insertSet` Status.classes st
                 , Status.amount  = i
                 } n
  where
    st = Status.new (slot n) dur $ Character.skills (character n) !! s !! alt

addOwnDefense :: Duration -- ^ 'Defense.dur'.
              -> Text -- ^ 'Defense.name'.
              -> Int -- ^ 'Defense.amount'.
              -> Ninja -> Ninja
addOwnDefense dur name i n = n { defense = d : defense n }
  where
    d = Defense { Defense.amount = i
                , Defense.user   = slot n
                , Defense.name   = name
                , Defense.dur    = incr $ sync dur
                }

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
clearTraps tr n = n { traps = filter ((tr /=) . Trap.trigger) $ traps n }

-- | Adds channels with a specific target.
addChannels :: Skill -> Slot -> Ninja -> Ninja
addChannels sk target n
  | chan == Instant || dur == 1 || dur == 2 = n
  | otherwise = n { newChans = chan' : newChans n }
  where
    chan  = Skill.dur sk
    dur   = incr $ TurnBased.getDur chan
    chan' = Channel { Channel.skill  = sk { Skill.require = Usable }
                    , Channel.target = target
                    , Channel.dur    = TurnBased.setDur dur chan
                    }

-- | Deletes matching 'channels'.
cancelChannel :: Text -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel name n = n { channels = f $ channels n
                         , newChans = f $ newChans n
                         }
  where
    f = filter $ (name /=) . Skill.name . Channel.skill

-- | Acquires 'Alternate' effects when copying a target's skills.
copyAlternates :: Duration -- ^ 'Copy.dur'.
               -> Skill -- ^ Skill being copied.
               -> Ninja -- ^ Person whose skills are being copied.
               -> Ninja -> Ninja
copyAlternates dur sk source n = n { statuses = alts ++ statuses n }
  where
    alts = filter (not . null . Status.effects) $ alt <$> statuses source
    dur' = sync if dur < -1 then dur + 1 else dur
    isAlt (Alternate _ name) = Skill.name sk == name
    isAlt _                  = False
    alt st = st { Status.dur     = min dur' $ Status.dur st
                , Status.user    = slot n
                , Status.effects = filter isAlt $ Status.effects st
                }

-- | Adds a 'Copy.Copy' to copies'.
copy :: Duration -- ^ 'Copy.dur'.
     -> Text -- ^ Replacing 'Skill.name'.
     -> Skill -- ^ Skill.
     -> Ninja -> Ninja
copy dur name sk n = fromMaybe n do
      s <- findIndex (any $ (name ==) . Skill.name) . toList .
           Character.skills $ character n
      return n { copies = copier s $ copies n }
  where
    copier s = Seq.update s . Just . Copy sk . incr $ sync dur

-- | Copies all @Skill@s from a source into 'Ninja.copies'.
copyAll :: Duration -- ^ 'Copy.dur'.
        -> Ninja -- ^ Person whose skills are being copied.
        -> Ninja -> Ninja
copyAll dur source n = n { copies = fromList $ cop <$> skills source }
  where
    synced = sync dur
    cop sk = Just $ Copy
        { Copy.dur   = synced + synced `rem` 2
        , Copy.skill = sk
        }

-- | Removes harmful effects. Does not work if the target has 'Plague'.
cure :: (Effect -> Bool) -> Ninja -> Ninja
cure match n
  | n `is` Plague = n
  | otherwise     = modifyStatuses (mapMaybe cure') n
  where
    keep Reveal = True
    keep a      = Effect.helpful a || not (match a)
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
prolong :: Int -- ^ Added to 'Status.dur'.
        -> Text -- ^ 'Status.name'.
        -> Slot -- ^ 'Status.user'.
        -> Ninja -> Ninja
prolong dur name src n =
    n { statuses = mapMaybe (prolong' dur name src) $ statuses n }

-- | Extends the duration of a single 'Status'.
prolong' :: Int -- ^ Added to 'Status.dur'.
         -> Text -- ^ 'Status.name'.
         -> Slot -- ^ 'Status.user'.
         -> Status -> Maybe Status
prolong' dur name user st
  | Status.dur st == 0               = Just st
  | not $ Labeled.match name user st = Just st
  | statusDur' <= 0                  = Nothing
  | otherwise                        = Just st
      { Status.dur    = statusDur'
      , Status.maxDur = max (Status.maxDur st) statusDur'
      }
    where
      statusDur' = Status.dur st + dur'
      dur'
        | odd (Status.dur st + dur) = dur
        | dur < 0                   = dur + 1
        | otherwise                 = dur - 1

prolongChannel :: Duration -> Text -> Ninja -> Ninja
prolongChannel dur name n = n { channels = f <$> channels n }
  where
    dur' chan = TurnBased.getDur chan + sync dur
    f chan
      | TurnBased.getDur chan <= 0              = chan
      | Skill.name (Channel.skill chan) /= name = chan
      | otherwise = TurnBased.setDur (dur' chan) chan

-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge = modifyStatuses (doPurge <$>)
  where
    canPurge ef = Effect.helpful ef || not (Effect.sticky ef)
    doPurge st
      | Unremovable ∈ Status.classes st = st
      | otherwise = st { Status.effects = filter canPurge $ Status.effects st }

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
recharge name owner n = n { charges = key `deleteMap` charges n }
  where
    key = Skill.Key name owner

-- With my... ninja info cards
kabuto :: Skill -> Ninja -> Ninja
kabuto sk n =
    n { statuses   = newmode : filter (not . getMode) (statuses n)
      , alternates = fromList [m + 1, m, m, m]
      , channels   = toList (init nChannels') ++ [swaps (last nChannels')]
      }
  where
    nSlot      = slot n
    nChannels' = case channels n of
                    x:xs -> x :| xs
                    []   -> Channel
                                { Channel.skill  = sk
                                , Channel.target = nSlot
                                , Channel.dur    = Skill.dur sk
                                } :| []
    sage       = " Sage"
    sLen       = length sage
    (mode, m)  = advance . maybe "" (dropEnd sLen . Status.name) .
                 find getMode $ statuses n
    ml         = mode ++ sage
    newmode    = Status { Status.amount  = 1
                        , Status.name    = ml
                        , Status.user    = nSlot
                        , Status.skill   = sk
                        , Status.effects = []
                        , Status.classes = setFromList [Hidden, Unremovable]
                        , Status.bombs   = []
                        , Status.maxDur  = 0
                        , Status.dur     = 0
                        }
    getMode st = Status.user st == nSlot
                 && sage == Text.takeEnd sLen (Status.name st)
    advance "Bloodline" = ("Genjutsu" , 2)
    advance "Genjutsu"  = ("Ninjutsu" , 3)
    advance "Ninjutsu"  = ("Taijutsu" , 4)
    advance _           = ("Bloodline", 1)
    swaps ch = ch { Channel.skill = (Channel.skill ch) { Skill.name = ml } }
