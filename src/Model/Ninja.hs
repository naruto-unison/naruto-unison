module Model.Ninja
  ( Ninja(..), new, factory
  , Flag(..)
  , playing
  , alive, minHealth, healthLost
  , adjustHealth, setHealth, sacrifice
  , is, isAny, isChanneling
  , has, hasOwn, hasDefense, hasStatus, hasTrap
  , numActive, numStacks, numHelpful, defenseAmount
  , take, drop
  , decr
  , decrStats

  , cancelChannel
  , clear, clearCounters, clearTrap, clearVariants
  , cure
  , cureBane
  , kill
  , prolong, prolong'
  , purge
  , refresh
  , addStatus, addOwnStacks, addOwnDefense
  , removeStack, removeStacks
  , resetCharges

  , kabuto
  ) where

import ClassyPrelude hiding (drop, group, head, init, last, take, mapMaybe)

import qualified Data.List as List
import           Data.List.NonEmpty ((!!), NonEmpty(..), group, init, last)
import qualified Data.Text as Text

import           Core.Util ((—), (∈), (∉), enumerate, intersects, mapMaybe)
import qualified Class.Classed as Classed
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled as Labeled
import qualified Class.TurnBased as TurnBased
import           Model.Internal (Ninja(..), Flag(..))
import qualified Model.Channel as Channel
import qualified Model.Character as Character
import qualified Model.Defense as Defense
import           Model.Duration (Duration, incr, sync)
import qualified Model.Effect as Effect
import           Model.Effect (Effect(..))
import           Model.Class (Class(..))
import           Model.Character (Character)
import qualified Model.Trap as Trap
import           Model.Trap (Trigger(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)
import qualified Model.Status as Status
import           Model.Status (Status)
import qualified Model.Variant as Variant

-- | Constructs a 'Ninja' with starting values from a character and an index.
new :: Character -> Slot -> Ninja
new c slot = Ninja { slot      = slot
                   , health    = 100
                   , character = c
                   , defense   = []
                   , barrier   = []
                   , statuses  = []
                   , charges   = replicate 4 0
                   , cooldowns = mempty
                   , variants  = replicate 4 $ Variant.none :| []
                   , copies    = replicate 4 Nothing
                   , channels  = []
                   , newChans   = []
                   , traps     = mempty
                   , face      = []
                   , parrying  = []
                   , tags      = []
                   , lastSkill = Nothing
                   , effects   = mempty
                   , flags     = mempty
                   }

-- | Factory resets a 'Ninja' to its starting values.
factory :: Ninja -> Ninja
factory n = new (character n) $ slot n

alive :: Ninja -> Bool
alive = (> 0) . health

-- | Whether a 'Ninja' belongs to the currently playing 'Model.Player.Player'.
playing :: ∀ a. Parity a => a -> Ninja -> Bool
playing p n = alive n && Parity.allied p n

is :: Effect -> Ninja -> Bool
is ef = (ef ∈) . effects

isAny :: (Class -> Effect) -> Ninja -> Bool
isAny efs = ((efs <$> enumerate) `intersects`) . effects

isChanneling :: Text -- ^ 'Skill.name'.
             -> Ninja -> Bool
isChanneling name = any ((name ==) . Skill.name . Channel.skill) . channels

has :: Text -- ^ 'Status.name'.
    -> Slot -- ^ 'Status.user'.
    -> Ninja -> Bool
has name user = any (Labeled.match name user) . statuses

hasStatus :: Status -> Ninja -> Bool
hasStatus st = any (Labeled.eq st) . statuses

hasDefense :: Text -- ^ 'Defense.name'.
           -> Slot -- ^ 'Defense.user'.
           -> Ninja -> Bool
hasDefense name user = any (Labeled.match name user) . defense

defenseAmount :: Text -- ^ 'Defense.name'.
              -> Slot -- ^ 'Defense.user'.
              -> Ninja -> Int
defenseAmount name user n =
    sum [ Defense.amount d | d <- defense n
                           , Defense.user d == user
                           , Defense.name d == name
                           ]

hasTrap :: Text -- ^ 'Trap.name'.
        -> Slot -- ^ 'Trap.user'.
        -> Ninja -- ^ 'traps' owner.
        -> Bool
hasTrap name user = any (Labeled.match name user) . traps

hasOwn :: Text -> Ninja -> Bool
hasOwn name n = has name (slot n) n
                || isChanneling name n
                || hasDefense name (slot n) n

-- | Number of stacks of matching self-applied 'statuses'.
numActive :: Text -- ^ 'Status.name'.
          -> Ninja -> Int
numActive name n
  | stacks > 0         = stacks
  | hasOwn name n      = 1
  | otherwise          = 0
  where
    stacks = numStacks name (slot n) n

-- | Number of stacks of matching 'statuses'.
numStacks :: Text -- ^ 'Status.name'.
          -> Slot -- ^ 'Status.user'.
          -> Ninja -> Int
numStacks name user n = sum . (Status.amount <$>) .
                        filter (Labeled.match name user) $
                        statuses n

-- | Counts all 'Effect.helpful' 'statuses' from allies.
-- Does not include self-applied 'Status'es.
numHelpful :: Ninja -> Int
numHelpful n = length stats + length defs
  where
    stats = List.nubBy Labeled.eq [ st | st <- statuses n
                                  , any Effect.helpful $ Status.effects st
                                  , slot n /= Status.user st
                                  , Parity.allied (slot n) $ Status.user st
                                  , Hidden ∉ Status.classes st
                                  ]
    defs  = List.nubBy Labeled.eq [ d | d <- defense n
                                  , slot n /= Defense.user d
                                  , Parity.allied (slot n) $ Defense.user d
                                  ]

-- | @1@ if affected by 'Endure', otherwise @0@.
minHealth :: Ninja -> Int
minHealth n
  | is Endure n = 1
  | otherwise   = 0

-- | Modifies 'health', restricting the value within ['minHealth', 100].
adjustHealth :: (Int -> Int) -> Ninja -> Ninja
adjustHealth f n = n { health = min 100 . max (minHealth n) . f $ health n }

-- | Sets 'health', restricting the value within ['minHealth', 100].
setHealth :: Int -> Ninja -> Ninja
setHealth = adjustHealth . const

-- | Sacrifices some amount of the target's 'Ninja.health' down to a minimum.
sacrifice :: Int -> Int -> Ninja -> Ninja
sacrifice minhp hp = adjustHealth $ max minhp . (— hp)

healthLost :: Ninja -- ^ Old.
           -> Ninja -- ^ New.
           -> Int
healthLost n n' = max 0 $ health n' - health n

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

-- | Applies 'Class.TurnBased.decr' to all of a 'Ninja's 'Class.TurnBased' types.
decr :: Ninja -> Ninja
decr n = case findMatch $ statuses n of
    Just (Snapshot n') -> decr n' -- TODO
    _ -> n { defense   = mapMaybe TurnBased.decr $ defense n
           , statuses  = foldStats . mapMaybe TurnBased.decr $ statuses n
           , barrier   = mapMaybe TurnBased.decr $ barrier n
           , face      = mapMaybe TurnBased.decr $ face n
           , channels  = mapMaybe TurnBased.decr $ newChans n ++ channels n
           , tags      = mapMaybe TurnBased.decr $ tags n
           , traps     = mapMaybe TurnBased.decr $ traps n
           , newChans  = mempty
           , variants  = turnDecr' <$> variants n
           , copies    = (>>= TurnBased.decr) <$> copies n
           , cooldowns = ((max 0 . subtract 1) <$>) <$> cooldowns n
           , parrying  = mempty
           , flags     = mempty
           }
  where
    findMatch          = find match . reverse . toList .
                         concatMap Status.effects . filter ((<= 2) . Status.dur)
    match Snapshot{}   = True
    match _            = False
    foldStats          = (foldStat <$>) . group . sort
    foldStat   (x:|[]) = x
    foldStat xs@(x:|_) = x { Status.amount = sum $ Status.amount <$> xs }
    turnDecr' xs       = case mapMaybe TurnBased.decr $ toList xs of
        x:xs' -> x :| xs'
        []    -> Variant.none :| []

addStatus :: Status -> Ninja -> Ninja
addStatus st n = n { statuses = Classed.nonStack st' st' $ statuses n }
  where
    st' = st { Status.classes = filter (InvisibleTraps /=) $ Status.classes st }

addOwnStacks :: Duration -- ^ 'Status.dur'.
             -> Text -- ^ 'Status.name'.
             -> Int -- ^ Skill index in 'Character.skills'.
             -> Int -- ^ Variant index in skill in 'Character.skills'.
             -> Int -- ^ 'Status.amount'.
             -> Ninja -> Ninja
addOwnStacks dur name s v i n =
    addStatus st { Status.name    = name
                 , Status.classes = Unremovable : Status.classes st
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
    d = Defense.Defense { Defense.amount = i
                        , Defense.user   = slot n
                        , Defense.name   = name
                        , Defense.dur    = incr $ sync dur
                        }

-- | Deletes matching 'statuses'.
clear :: Text -- ^ 'Status.name'.
      -> Slot -- ^ 'Status.user'.
      -> Ninja -> Ninja
clear name user n = n { statuses = filter keep $ statuses n }
  where
    keep = not . Labeled.match name user

-- | Deletes matching 'traps'.
clearTrap :: Text -- ^ 'Trap.name'.
          -> Slot -- ^ 'Trap.user'.
          -> Ninja -> Ninja
clearTrap name user n =
    n { traps = filter (not . Labeled.match name user) $ traps n }

-- | Resets matching 'variants'.
clearVariants :: Text -- ^ 'Variant.name'.
              -> Ninja -> Ninja
clearVariants name n = n { variants = f <$> variants n }
  where
    keep v = not (Variant.fromSkill v) || Variant.name v /= name
    f = ensure . filter keep . toList -- TODO
    ensure []     = Variant.none :| []
    ensure (x:xs) = x :| xs

-- | Deletes matching 'channels'.
cancelChannel :: Text -- ^ 'Skill.name'.
              -> Ninja -> Ninja
cancelChannel name n = clearVariants name n { channels = f $ channels n }
  where
    f = filter ((name /=) . Skill.name . Channel.skill)

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
      | is Plague n                        = Just st
      | not $ any keep $ Status.effects st = Nothing
      | otherwise = Just st { Status.effects = filter keep $ Status.effects st }

-- | Cures 'Bane' 'statuses'.
cureBane :: Ninja -> Ninja
cureBane n
  | is Plague n = n
  | otherwise = cure cured n { statuses = filter keep $ statuses n }
  where
    cured Afflict{} = True
    cured _         = False
    keep st         = Bane ∉ Status.classes st
                      || slot n == Status.user st

kill :: Bool -- ^ Can be prevented by 'Endure'
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
  | Status.dur st == 0                 = Just st
  | not $ Labeled.match name user st = Just st
  | statusDur' <= 0                    = Nothing
  | otherwise                          = Just st
      { Status.dur    = statusDur'
      , Status.maxDur = max (Status.maxDur st) statusDur'
      }
    where -- TODO figure out why the fuck this works
      statusDur' = Status.dur st + dur'
      dur'
        | odd (Status.dur st) == even dur = dur
        | dur < 0                         = dur + 1
        | otherwise                       = dur - 1

-- | Removes all helpful effects.
purge :: Ninja -> Ninja
purge n
  | is Enrage n = n
  | otherwise         = n { statuses = doPurge <$> statuses n }
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

-- | Removes 'OnCounter' 'traps'.
clearCounters :: Ninja -> Ninja
clearCounters n = n { traps = [trap | trap <- traps n
                                    , keep $ Trap.trigger trap] }
  where
    keep (OnCounter _) = False
    keep _             = True

-- With my... ninja info cards
kabuto :: Skill -> Ninja -> Ninja
kabuto skill n =
    n { statuses = newmode : filter (not . getMode) (statuses n)
      , variants = fromList $ (:|[]) <$> [var', var, var, var]
      , channels = toList (init nChannels') ++ [swaps (last nChannels')]
      }
  where
    nSlot      = slot n
    nChannels' = case channels n of
                    x:xs -> x :| xs
                    []   -> Channel.Channel
                                { Channel.source = nSlot
                                , Channel.skill  = skill
                                , Channel.target = nSlot
                                , Channel.dur    = Skill.channel skill
                                } :| []
    sage       = " Sage"
    sLen       = length sage
    (mode, m)  = advance . maybe "" (dropEnd sLen . Status.name) .
                 find getMode $ statuses n
    var        = Variant.Variant { Variant.variant   = m
                                 , Variant.ownCd     = False
                                 , Variant.name      = ""
                                 , Variant.fromSkill = False
                                 , Variant.dur       = 0
                                 }
    var'       = var { Variant.variant = m + 1 }
    ml         = mode ++ sage
    newmode    = Status.Status { Status.amount  = 1
                               , Status.name    = ml
                               , Status.source  = nSlot
                               , Status.user    = nSlot
                               , Status.skill   = skill
                               , Status.effects = []
                               , Status.classes = [Hidden, Unremovable]
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
