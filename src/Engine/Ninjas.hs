-- | 'Ninja' processing.
module Engine.Ninjas
  ( skills, skill, skill'
  , apply
  , processEffects

  , decrStats
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

  , clear
  , clearReplaces
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
  , resetCharges
  , vary

  , prolong
  , prolong'
  , prolongChannel
  , refresh

  , take, drop

  , kabuto
  ) where

import ClassyPrelude hiding (drop, group, head, init, last, take)

import           Data.List (findIndex)
import           Data.List.NonEmpty (NonEmpty(..), (!!), group, head, init, last)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import           Core.Util ((—), (!?), (∈), (∉))
import qualified Class.Classed as Classed
import qualified Class.Parity as Parity
import qualified Class.Labeled as Labeled
import qualified Class.TurnBased as TurnBased
import qualified Model.Channel as Channel
import           Model.Channel (Channel(Channel), Channeling(..))
import qualified Model.Character as Character
import           Model.Class (Class(..))
import qualified Model.Copy as Copy
import           Model.Copy (Copy(Copy), Copying)
import qualified Model.Defense as Defense
import           Model.Defense (Defense(Defense))
import           Model.Duration (Duration, incr, sync)
import qualified Model.Effect as Effect
import           Model.Effect (Amount(..), Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja(..), is)
import qualified Model.Requirement as Requirement
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import qualified Model.Status as Status
import qualified Model.Variant as Variant
import           Model.Variant (Variant(Variant), Varying)
import qualified Engine.Effects as Effects
import qualified Engine.Skills as Skills

import           Model.Slot (Slot)
import           Model.Status (Status(Status))
import qualified Model.Trap as Trap
import           Model.Trap (Trigger)

-- | Adjusts the @Skill@ slot of a @Ninja@ due to 'Ninja.variants', 'Effect's
-- that modify skills, and the 'Skill.changes' of the @Skill@.
skill' :: Ninja -> Int -> Int -> Skill
skill' n s v = Skills.change n $ cSkills !! s !! v
  where
    cSkills = Character.skills $ Ninja.character n

-- | Applies 'skill'' to a @Skill@ and further modifies it due to 'Ninja.copies'
-- and 'Skill.require'ments.
skill :: Either Int Skill -> Ninja -> Skill
skill (Right sk) n = Requirement.usable n Nothing sk
skill (Left s)   n = Requirement.usable n (Just s) .
                     maybe (skill' n s v) Copy.skill . join . (!? s) $
                     Ninja.copies n
    where
      v = maybe 0 (Variant.variant . head) . (!? s) $ Ninja.variants n

-- | All four skill slots of a @Ninja@ modified by 'skill'.
skills :: Ninja -> [Skill]
skills n = flip skill n . Left <$> [0..3]

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
processEffects n = n { Ninja.effects = baseStatuses >>= replicates >>= process }
  where
    nSlot         = Ninja.slot n
    baseStatuses  = Ninja.statuses n
    baseEffects   = concatMap Status.effects baseStatuses
    enraged       = Enrage ∈ baseEffects
    sealed        = not enraged && Seal ∈ baseEffects
    ignores
      | sealed    = [ef | status <- baseStatuses
                        , Status.user status == nSlot
                        , Ignore con <- Status.effects status
                        , ef <- Effect.construct con
                        ]
      | otherwise = [ef | Ignore con <- baseEffects, ef <- Effect.construct con]
    boostAmount
      | sealed    = 1
      | otherwise = product $ 1 : [x | Boost x <- baseEffects]
    filtered filt = filter (\ef -> filt ef && ef ∉ ignores) . Status.effects
    replicates st = replicate (Status.amount st) st { Status.amount = 1 }
    process st
      | Status.user st == Ninja.slot n = Status.effects st
      | enraged   = boost <$> filtered Effect.bypassEnrage st
      | sealed    = boost <$> filtered (not . Effect.helpful) st
      | otherwise = boost <$> filtered (const True) st
      where
        boost
          | Parity.allied nSlot $ Status.user st = Effect.boosted boostAmount
          | otherwise                            = id

-- | Adds a 'Variant.Variant' to 'Ninja.variants' by skill and variant index
-- within 'Character.skills'.
vary :: Varying -> Int -> Int -> Ninja -> Ninja
vary dur s v n = n { Ninja.variants = adjust $ Ninja.variants n }
  where
    variant = Variant
        { Variant.variant = v
        , Variant.ownCd   = Skill.varicd $ skill' n s v
        , Variant.dur     = dur
        }
    adjust
      | TurnBased.getDur dur <= 0 = Seq.update s $ variant :| []
      | otherwise                 = Seq.adjust' (cons variant) s


-- | Factory resets a @Ninja@ to its starting values.
factory :: Ninja -> Ninja
factory n = Ninja.new (slot n) $ character n

-- | Modifies 'health', restricting the value within ['minHealth', 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth f n = n { Ninja.health = min 100 . max (Ninja.minHealth n) . f $ health n }

-- | Sets 'health', restricting the value within ['minHealth', 100].
setHealth :: Int -> Ninja -> Ninja
setHealth = adjustHealth . const

-- | Sacrifices some amount of the target's 'Ninja.health' down to a minimum.
sacrifice :: Int -> Int -> Ninja -> Ninja
sacrifice minhp hp = adjustHealth $ max minhp . (— hp)

-- | Obtains an 'Effect' and removes its 'Status' from its owner.
take :: (Effect -> Bool) -> Ninja -> Maybe (Ninja, Effect, Status)
take matcher n = do
    match <- find (any matcher . Status.effects) $ statuses n
    a     <- case Status.effects match of
        [x] -> Just x
        xs  -> find matcher xs
    return (n { statuses = Status.remove match $ statuses n }, a, match)

-- | Removes a 'Status' with a matching 'Effect'. Uses 'take' internally.
drop :: (Effect -> Bool) -> Ninja -> Maybe Ninja
drop = map fst3 . take
  where
    fst3 (Just (n, _, _)) = Just n
    fst3 Nothing          = Nothing

-- | While concluding 'Engine.Turn.run', prevents refreshed 'Status'es from
-- having doubled effects due to there being both an old and new version.
decrStats :: Ninja -> Ninja
decrStats n = n { statuses = expire <$> statuses n }
  where
    expire st
      | Status.dur st == 1 = st { Status.effects = mempty }
      | otherwise          = st

-- | Applies 'Class.TurnBased.decr' to all of a @Ninja@'s 'Class.TurnBased'
-- types.
decr :: Ninja -> Ninja
decr n = n { defense   = mapMaybe TurnBased.decr $ defense n
           , statuses  = foldStats . mapMaybe TurnBased.decr $ statuses n
           , barrier   = mapMaybe TurnBased.decr $ barrier n
           , face      = mapMaybe TurnBased.decr $ face n
           , channels  = decrChannels
           , traps     = mapMaybe TurnBased.decr $ traps n
           , newChans  = mempty
           , variants  = variantsDecr <$> variants n
           , copies    = (>>= TurnBased.decr) <$> copies n
           , cooldowns = ((max 0 . subtract 1) <$>) <$> cooldowns n
           , acted     = False
           }
  where
    decrChannels       = mapMaybe TurnBased.decr $ newChans n ++ channels n
    foldStats          = (foldStat <$>) . group . sort
    foldStat   (x:|[]) = x
    foldStat xs@(x:|_) = x { Status.amount = sum $ Status.amount <$> xs }
    variantsDecr xs    = case mapMaybe decrVariant $ toList xs of
        x:xs' -> x :| xs'
        []    -> Variant.none :| []
    decrVariant x = case Variant.dur x of
        Variant.FromSkill name
          | any ((name ==) . Skill.name . Channel.skill) decrChannels -> Just x
          | otherwise -> Nothing
        _ -> TurnBased.decr x

addStatus :: Status -> Ninja -> Ninja
addStatus st n = n { statuses = Classed.nonStack st' st' $ statuses n }
  where
    st' = st { Status.classes = deleteSet InvisibleTraps $ Status.classes st }

addOwnStacks :: Duration -- ^ 'Status.dur'.
             -> Text -- ^ 'Status.name'.
             -> Int -- ^ Skill index in 'Character.skills'.
             -> Int -- ^ Variant index in skill in 'Character.skills'.
             -> Int -- ^ 'Status.amount'.
             -> Ninja -> Ninja
addOwnStacks dur name s v i n =
    addStatus st { Status.name    = name
                 , Status.classes = insertSet Unremovable $ Status.classes st
                 , Status.amount  = i
                 } n
  where
    st = Status.new (slot n) dur $ Character.skills (character n) !! s !! v

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

addDefense :: Int -> Defense -> Ninja -> Ninja
addDefense amount d n =
    n { defense = added : deleteBy Labeled.eq added (defense n) }
  where
    added = d { Defense.amount = amount + Defense.amount d }

-- | Deletes matching 'statuses'.
clear :: Text -- ^ 'Status.name'.
      -> Slot -- ^ 'Status.user'.
      -> Ninja -> Ninja
clear name user n = n { statuses = filter keep $ statuses n }
  where
    keep = not . Labeled.match name user

-- | Deletes 'Replace' effects.
clearReplaces :: Ninja -> Ninja
clearReplaces n =
    n { statuses = filter (any un . Status.effects) $ statuses n }
  where
    un Replace{} = False
    un _         = True

-- | Deletes matching 'traps'.
clearTrap :: Text -- ^ 'Trap.name'.
          -> Slot -- ^ 'Trap.user'.
          -> Ninja -> Ninja
clearTrap name user n =
    n { traps = filter (not . Labeled.match name user) $ traps n }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearTraps :: Trigger -> Ninja -> Ninja
clearTraps tr n = n { traps = filter ((tr /=) . Trap.trigger) $ traps n }

-- | Resets matching 'variants'.
clearVariants :: Text -- ^ 'Variant.name'.
              -> Ninja -> Ninja
clearVariants name n = n { variants = f <$> variants n }
  where
    keep Variant{ dur = Variant.FromSkill x } = x /= name
    keep _ = True
    f = ensure . filter keep . toList -- TODO
    ensure []     = Variant.none :| []
    ensure (x:xs) = x :| xs

-- | Adds channels with a specific target.
addChannels :: Skill -> Slot -> Ninja -> Ninja
addChannels sk target n
  | chan == Instant || dur == 1 || dur == 2 = n
  | otherwise = n { newChans = chan' : newChans n }
  where
    chan  = Skill.dur sk
    dur   = Copy.maxDur (Skill.copying sk) . incr $ TurnBased.getDur chan
    chan' = Channel { Channel.source = Copy.source sk $ slot n
                    , Channel.skill  = sk
                    , Channel.target = target
                    , Channel.dur    = TurnBased.setDur dur chan
                    }


-- | Deletes matching 'channels'.
cancelChannel :: Text -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel name n = clearVariants name n { channels = f $ channels n }
  where
    f = filter ((name /=) . Skill.name . Channel.skill)

-- | Adds a 'Copy.Copy' to copies'.
copy :: Duration
     -> Text -- ^ Replacing 'Skill.name'.
     -> (Slot -> Int -> Copying) -- ^ Either 'Copy.Deep' or 'Copy.Shallow'.
     -> Slot -- ^ Target.
     -> Skill -- ^ Skill.
     -> Ninja -> Ninja
copy dur name constructor copyFrom sk n = fromMaybe n do
      s <- findIndex (any $ (name ==) . Skill.name) . toList .
           Character.skills $ character n
      return n { copies = copier s $ copies n }
  where
    copier s = Seq.update s . Just . Copy sk' $
               sync if dur < -1 then dur + 1 else dur
    sk'      = sk { Skill.cost     = 0
                  , Skill.cooldown = 0
                  , Skill.copying  = constructor copyFrom $ sync dur - 1
                  }

-- | Copies all @Skill@s from a source into 'Ninja.copies'.
copyAll :: Duration -- ^ 'Copy.dur'.
        -> Ninja -- ^ Person whose skills are being copied.
        -> Ninja -> Ninja
copyAll dur source n = n { copies = fromList $ cop <$> skills source }
  where
    synced = sync dur
    src    = slot source
    cop sk = Just $ Copy
        { Copy.dur   = synced + synced `rem` 2
        , Copy.skill =
            sk { Skill.copying = Copy.Deep (Copy.source sk src) $ sync dur - 1 }
        }

-- | Removes harmful effects. Does not work if the target has 'Plague'.
cure :: (Effect -> Bool) -> Ninja -> Ninja
cure match n = n { statuses = mapMaybe cure' $ statuses n }
  where
    keep Reveal = True
    keep a      = Effect.helpful a || not (match a)
    cure' st
      | Status.user st == slot n           = Just st
      | null $ Status.effects st           = Just st
      | Unremovable ∈ Status.classes st    = Just st
      | n `is` Plague                      = Just st
      | not $ any keep $ Status.effects st = Nothing
      | otherwise = Just st { Status.effects = filter keep $ Status.effects st }

-- | Cures 'Bane' 'statuses'.
cureBane :: Ninja -> Ninja
cureBane n
  | n `is` Plague = n
  | otherwise     = n { statuses = filter keep $ statuses n }
  where
    keep st = Bane ∉ Status.classes st || slot n == Status.user st

kill :: Bool -- ^ Can be prevented by 'Endure'.
     -> Ninja -> Ninja
kill endurable n
  | endurable = setHealth 0 n
  | otherwise = n { statuses = dead : statuses n
                  , health = 0
                  }
  where
    dead = Status.dead $ slot n

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
    where -- TODO figure out why the fuck this works
      statusDur' = Status.dur st + dur'
      dur'
        | odd (Status.dur st) == even dur = dur
        | dur < 0                         = dur + 1
        | otherwise                       = dur - 1

prolongChannel :: Duration -> Text -> Ninja -> Ninja
prolongChannel dur name n = n { channels = f <$> channels n }
  where
    f chan
      | TurnBased.getDur chan <= 0              = chan
      | Skill.name (Channel.skill chan) /= name = chan
      | otherwise = flip TurnBased.setDur chan .
                    Copy.maxDur (Skill.copying $ Channel.skill chan) $
                    TurnBased.getDur chan + sync dur


-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge n = n { statuses = doPurge <$> statuses n }
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
removeStack name n = n { statuses = f $ statuses n }
  where
    f = Status.removeMatch 1 . Labeled.match name $ slot n

-- | Replicates 'removeStack'.
removeStacks :: Text -- ^ 'Status.name'.
             -> Int -- ^ Subtracted from 'Status.amount'.
             -> Slot -- ^ 'Status.user'.
             -> Ninja -> Ninja
removeStacks name i user n = n { statuses = f $ statuses n }
  where
    f = Status.removeMatch i $ Labeled.match name user

-- | Resets 'charges' to four @0@s.
resetCharges :: Ninja -> Ninja
resetCharges n = n { charges = replicate 4 0 }

-- With my... ninja info cards
kabuto :: Skill -> Ninja -> Ninja
kabuto sk n =
    n { statuses = newmode : filter (not . getMode) (statuses n)
      , variants = fromList $ (:|[]) <$> [var', var, var, var]
      , channels = toList (init nChannels') ++ [swaps (last nChannels')]
      }
  where
    nSlot      = slot n
    nChannels' = case channels n of
                    x:xs -> x :| xs
                    []   -> Channel
                                { Channel.source = nSlot
                                , Channel.skill  = sk
                                , Channel.target = nSlot
                                , Channel.dur    = Skill.dur sk
                                } :| []
    sage       = " Sage"
    sLen       = length sage
    (mode, m)  = advance . maybe "" (dropEnd sLen . Status.name) .
                 find getMode $ statuses n
    var        = Variant { Variant.variant = m
                         , Variant.ownCd   = False
                         , Variant.dur     = Variant.Duration 0
                         }
    var'       = var { Variant.variant = m + 1 }
    ml         = mode ++ sage
    newmode    = Status { Status.amount  = 1
                        , Status.name    = ml
                        , Status.source  = nSlot
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
