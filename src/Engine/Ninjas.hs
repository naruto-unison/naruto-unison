-- | 'Ninja' processing.
module Engine.Ninjas
  ( skill, skills
  , apply
  , processEffects

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
  , clearFace
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

  , kabuto
  ) where

import ClassyPrelude hiding (drop, take)

import           Data.List (findIndex)
import           Data.List.NonEmpty ((!!))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import           Core.Util ((—), (!?), (∈), (∉))
import qualified Class.Classed as Classed
import qualified Class.Parity as Parity
import qualified Class.Labeled as Labeled
import qualified Class.TurnBased as TurnBased
import           Class.TurnBased (TurnBased)
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
import qualified Model.Face as Face
import           Model.Face (Face(Face))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja(..), is)
import qualified Model.Requirement as Requirement
import           Model.Requirement (Requirement(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)
import qualified Model.Status as Status
import           Model.Status (Status(Status))
import qualified Model.Trap as Trap
import           Model.Trap (Trigger(..))
import qualified Model.Variant as Variant
import           Model.Variant (Variant(Variant), Varying)
import qualified Engine.Effects as Effects
import qualified Engine.Skills as Skills

-- | Adjusts the @Skill@ slot of a @Ninja@ due to 'Ninja.variants', 'Effect's
-- that modify skills, and the 'Skill.changes' of the @Skill@.
skill' :: Ninja -> Int -> Int -> Skill
skill' n s v = Skills.change n $ Character.skills (character n) !! s !! v

-- | Applies 'skill'' to a @Skill@ and further modifies it due to 'Ninja.copies'
-- and 'Skill.require'ments.
skill :: Either Int Skill -> Ninja -> Skill
skill (Right sk) n = Requirement.usable n Nothing sk
skill (Left s)   n = Requirement.usable n (Just s) .
                     maybe (skill' n s v) Copy.skill . join . (!? s) $ copies n
  where
    v = maybe 0 (Variant.variant . head) . (!? s) $ variants n

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
processEffects n = n { effects = baseStatuses >>= replicates >>= process }
  where
    nSlot         = slot n
    baseStatuses  = statuses n
    baseEffects   = baseStatuses >>= Status.effects
    enraged       = Enrage ∈ baseEffects
    sealed        = not enraged && Seal ∈ baseEffects
    boostAmount
      | sealed    = 1
      | otherwise = product $ 1 : [x | Boost x <- baseEffects]
    replicates st = replicate (boost * Status.amount st)
                    st { Status.amount = 1 }
      where
        user = Status.user st
        boost
          | user == nSlot            = 1
          | Parity.allied nSlot user = boostAmount
          | otherwise                = 1
    process Status{user, effects}
      | sealed        = filter (not . Effect.helpful) effects
      | user == nSlot = effects
      | enraged       = filter Effect.bypassEnrage effects
      | otherwise     = effects

-- | Adds a 'Variant.Variant' to 'Ninja.variants' by skill and variant index
-- within 'Character.skills'.
vary :: Varying -> Int -> Int -> Ninja -> Ninja
vary dur s v n = n { variants = adjust $ variants n }
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
decr n@Ninja{..} = n
    { defense   = mapMaybe TurnBased.decr defense
    , statuses  = foldStats $ mapMaybe TurnBased.decr statuses
    , barrier   = mapMaybe TurnBased.decr barrier
    , face      = mapMaybe faceDecr face
    , channels  = decrChannels
    , traps     = mapMaybe TurnBased.decr traps
    , delays    = mapMaybe TurnBased.decr delays
    , newChans  = mempty
    , variants  = variantsDecr <$> variants
    , copies    = (>>= TurnBased.decr) <$> copies
    , cooldowns = ((max 0 . subtract 1) <$>) <$> cooldowns
    , acted     = False
    }
  where
    decrChannels       = mapMaybe TurnBased.decr $ newChans ++ channels
    foldStats xs       = foldStat <$> group (sort xs)
    foldStat   (x:|[]) = x
    foldStat xs@(x:|_) = x { Status.amount = sum $ Status.amount <$> xs }
    faceDecr x         = decrVarying (Face.dur x) x
    variantsDecr xs    = case mapMaybe decrVariant $ toList xs of
        x:xs' -> x :| xs'
        []    -> Variant.none :| []
    decrVariant x = decrVarying (Variant.dur x) x
    decrVarying :: ∀ a. TurnBased a => Varying -> a -> Maybe a
    decrVarying (Variant.FromSkill name) x
      | any ((name ==) . Skill.name . Channel.skill) decrChannels = Just x
      | otherwise = Nothing
    decrVarying (Variant.Duration _) x = TurnBased.decr x

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

addDefense :: Int -- ^ 'Defense.amount'.
           -> Text -- ^ 'Defense.name'.
           -> Slot -- ^ 'Defense.user'.
           -> Ninja -> Ninja
addDefense amount name user n =
    n { defense = Labeled.mapFirst addAmount name user $ defense n }
  where
    addAmount x = x { Defense.amount = amount + Defense.amount x }

-- | Deletes matching 'statuses'.
clear :: Text -- ^ 'Status.name'.
      -> Slot -- ^ 'Status.user'.
      -> Ninja -> Ninja
clear name user n =
    n { statuses = filter (not . Labeled.match name user) $ statuses n }

-- | Deletes matching 'traps'.
clearTrap :: Text -- ^ 'Trap.name'.
          -> Slot -- ^ 'Trap.user'.
          -> Ninja -> Ninja
clearTrap name user n =
    n { traps = filter (not . Labeled.match name user) $ traps n }

-- | Deletes 'traps' with matching 'Trap.trigger'.
clearTraps :: Trigger -> Ninja -> Ninja
clearTraps tr n = n { traps = filter ((tr /=) . Trap.trigger) $ traps n }

-- | Resets matching 'face's.
clearFace :: Text -- ^ 'Face.name'.
          -> [Face] -> [Face]
clearFace name = filter \case
    Face{ dur = Variant.FromSkill x } -> x /= name
    _                                 -> True

-- | Resets matching 'variants'.
clearVariants :: Text -- ^ 'Variant.name'.
              -> Seq (NonEmpty Variant) -> Seq (NonEmpty Variant)
clearVariants name variants = ensure . filter keep . toList <$> variants
  where
    keep Variant{ dur = Variant.FromSkill x } = x /= name
    keep _                                    = True
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
    chan' = Channel { Channel.skill  = sk { Skill.require = Usable }
                    , Channel.target = target
                    , Channel.dur    = TurnBased.setDur dur chan
                    }

-- | Deletes matching 'channels'.
cancelChannel :: Text -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel name n = n { channels = f $ channels n
                         , newChans = f $ newChans n
                         , variants = clearVariants name $ variants n
                         , face     = clearFace name $ face n
                         }
  where
    f = filter $ (name /=) . Skill.name . Channel.skill

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
cure match n
  | n `is` Plague = n
  | otherwise     = n { statuses = mapMaybe cure' $ statuses n }
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
  | otherwise     = n { statuses = filter keep $ statuses n }
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
    dur' chan = Copy.maxDur (Skill.copying $ Channel.skill chan) $
                TurnBased.getDur chan + sync dur
    f chan
      | TurnBased.getDur chan <= 0              = chan
      | Skill.name (Channel.skill chan) /= name = chan
      | otherwise = TurnBased.setDur (dur' chan) chan

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
removeStack name n = n { statuses = Status.remove 1 name (slot n) $ statuses n }

-- | Replicates 'removeStack'.
removeStacks :: Text -- ^ 'Status.name'.
             -> Int -- ^ Subtracted from 'Status.amount'.
             -> Slot -- ^ 'Status.user'.
             -> Ninja -> Ninja
removeStacks name i user n =
    n { statuses = Status.remove i name user $ statuses n }

-- | Resets 'charges' to four @0@s.
resetCharges :: Ninja -> Ninja
resetCharges n = n { charges = replicate Ninja.skillSize 0 }

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
                                { Channel.skill  = sk
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
