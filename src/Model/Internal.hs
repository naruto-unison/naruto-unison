{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-} -- Used once at the bottom of the file.
module Model.Internal where

import ClassyPrelude.Yesod hiding (Status, Vector, get)
import           Control.Monad.Reader (local, mapReaderT)
import           Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import           Control.Monad.Trans.State.Strict (StateT, get, modify')
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Vector.Generic (Vector)
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as Random
import           Text.Blaze (ToMarkup(..))
import           Yesod.WebSockets (WebSocketsT)

import           Core.Util (enumerate, equaling)
import qualified Class.Classed as Classed
import           Class.Classed (Classed)
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled as Labeled
import           Class.Labeled (Labeled)
import           Class.TurnBased (TurnBased(..))
import           Model.Class (Class(..))
import           Model.Chakra (Chakras(..))
import           Model.Face (Face(..))
import           Model.Player (Player)
import           Model.Slot (Slot)

data Amount = Flat | Percent deriving (Eq)

data Context = Context { skill   :: Skill
                       , source  :: Slot
                       , user    :: Slot
                       , target  :: Slot
                       } deriving (Eq)

type PlayConstraint a = ∀ m. (RandomT m, PlayT m) => m a
newtype Play a = Play (PlayConstraint a)
instance Eq (Play a) where
    (==) = const $ const True

class Monad m => RandomT m where
    random  :: ∀ a. Random.Variate a => a -> a -> m a
    shuffle :: ∀ v a. Vector v a => v a -> m (v a)

class Monad m => GameT m where
    game        :: m Game
    modify      :: (Game -> Game) -> m ()

class GameT m => PlayT m where
    context :: m Context
    with    :: ∀ a. (Context -> Context) -> m a -> m a

instance GameT m => GameT (ReaderT Context m) where
    game   = lift game
    modify = lift . modify
instance GameT m => PlayT (ReaderT Context m) where
    context = ask
    with    = local

instance RandomT m => RandomT (MaybeT m) where
    random a = lift . random a
    shuffle  = lift . shuffle
instance GameT m => GameT (MaybeT m) where
    game    = lift game
    modify  = lift . modify
instance PlayT m => PlayT (MaybeT m) where
    context = lift context
    with f  = mapMaybeT $ with f
instance RandomT m => RandomT (ReaderT Context m) where
    random a = lift . random a
    shuffle  = lift . shuffle
instance RandomT m => RandomT (WebSocketsT m) where
    random a = lift . random a
    shuffle  = lift . shuffle
instance GameT m => GameT (WebSocketsT m) where
    game    = lift game
    modify  = lift . modify
instance PlayT m => PlayT (WebSocketsT m) where
    context = lift context
    with f  = mapReaderT $ with f

instance GameT (StateT Game Identity) where
    game   = get
    modify = modify'

data Wrapper = Wrapper { gameRef :: IORef Game
                       , rand    :: Random.GenIO
                       }

instance MonadIO m => RandomT (ReaderT Wrapper m) where
    random a b = asks (rand :: Wrapper -> Random.GenIO)
                 >>= liftIO . Random.uniformR (a, b)
    shuffle xs = asks (rand :: Wrapper -> Random.GenIO)
                 >>= liftIO . Random.uniformShuffle xs

instance MonadIO m => GameT (ReaderT Wrapper m) where
    game     = asks gameRef >>= readIORef
    modify f = asks gameRef >>= flip modifyIORef' f

-- | Effects of 'Status'es.
data Effect
    = Afflict      Int               -- ^ Deals damage every turn
    | AntiCounter                    -- ^ Cannot be countered or reflected
    | Bleed        Class Amount Int  -- ^ Adds to damage received
    | Bless        Int               -- ^ Adds to healing 'Skill's
    | Block                          -- ^ Treats source as 'Invulnerable'
    | Boost        Int               -- ^ Scales effects from allies
    | Build        Int               -- ^ Adds to destructible defense 'Skill'
    | Counter      Class             -- ^ Counters the first 'Skill's
    | CounterAll   Class             -- ^ 'Counter's without being removed
    | Duel                           -- ^ 'Invulnerable' to everyone but source
    | Endure                         -- ^ Health cannot go below 1
    | Enrage                         -- ^ Ignore all harmful status effects
    | Exhaust      Class             -- ^ 'Skill's cost 1 additional random chakra
    | Expose                         -- ^ Cannot reduce damage or be 'Invulnerable'
    | Heal         Int               -- ^ Heals every turn
    | Ignore       (Class -> Effect) -- ^ Invulnerable to certain effects
    | Invulnerable Class             -- ^ Invulnerable to enemy 'Skill's
    | ImmuneSelf                     -- ^ Invulnerable to self-caused damage
    | Invincible   Class             -- ^ Like 'Invulnerable', but targetable
    | Isolate                        -- ^ Unable to affect others
    | Link         Int               -- ^ Increases damage and healing from source
    | Parry        Class (Play ())   -- ^ 'Counter' and trigger an effect
    | ParryAll     Class (Play ())   -- ^ 'Parry' repeatedly
    | Pierce                         -- ^ Damage skills turn into piercing
    | Plague                         -- ^ Invulnerable to healing and curing
    | Reduce       Class Amount Int  -- ^ Reduces damage by an amount
    | Reapply                        -- ^ Shares harmful skills with source
    | Redirect     Class             -- ^ Transfers harmful 'Skill's
    | Reflect                        -- ^ Reflects the first 'Skill'
    | ReflectAll                     -- ^ 'Reflect' repeatedly
    | Restrict                       -- ^ Forces AoE attacks to be single-target
    | Reveal                         -- ^ Makes 'Invisible' effects visible
    | Seal                           -- ^ Ignore all friendly 'Skill's
    | Share                          -- ^ Shares all harmful non-damage effects
    | Silence                        -- ^ Unable to cause non-damage effects
    | Snapshot     Ninja             -- ^ Saves a snapshot of the current state
    | Snare        Int               -- ^ Increases cooldowns
    | SnareTrap    Class Int         -- ^ Negates next skill and increases cooldown
    | Strengthen   Class Amount Int  -- ^ Adds to all damage dealt
    | Stun         Class             -- ^ Unable to use 'Skill's
    | Swap         Class             -- ^ Target swaps enemies and allies
    | Taunt                          -- ^ Forced to attack the source
    | Taunting     Int               -- ^ Forced to attack their next target
    | Threshold    Int               -- ^ Invulnerable to baseline damage below a threhold
    | Throttle (Class -> Effect) Int -- ^ Applying an effect lasts fewer turns
    | Undefend                       -- ^ Does not benefit from destructible defense
    | Uncounter                      -- ^ Cannot counter or reflect
    | Unexhaust                      -- ^ Decreases chakra costs by 1 random
    | Unreduce     Int               -- ^ Reduces damage reduction 'Skill's
    | Weaken       Class Amount Int  -- ^ Lessens damage dealt
    -- | Copies a skill into source's skill slot
    | Replace { replaceDuration :: Int
              , replaceClass    :: Class
              , replaceTo       :: Int   -- ^ Skill index of source to copy into
              , replaceNonHarm  :: Bool  -- ^ Include non-harmful 'Skill's
              } deriving (Eq)

low :: ∀ a. Show a => a -> String
low = toLower . show

showAmt :: Amount -> Int -> String
showAmt Flat    = show
showAmt Percent = (++ "%") . show

instance Show Effect where
    show (Afflict x) = "Receives " ++ show x ++ " affliction damage each turn."
    show AntiCounter = "Cannot be countered or reflected."
    show (Bleed classes amt x)
      | x >= 0    =  showAmt amt x ++ " additional damage taken from " ++ low classes ++ " skills."
      | otherwise = "Reduces all " ++ low classes ++  " damage received by " ++ showAmt amt (-x) ++ "."
    show (Bless x) = "Healing skills heal 1 additional " ++ show x ++ " health."
    show Block = "Unable to affect the source of this effect."
    show (Boost x) = "Active effects from allies are " ++ show x ++ " times as powerful."
    show (Build x)
      | x >= 0    = "Destructible skills provide " ++ show x ++ " additional points of defense."
      | otherwise =  "Destructible skills provide " ++ show (-x) ++ " fewer points of defense."
    show (Counter All)  = "Counters the first skill."
    show (Counter classes) = "Counters the first " ++ low classes ++ "skill."
    show (CounterAll All) = "Counters all skills."
    show (CounterAll classes) = "Counters all " ++ low classes ++ "skills."
    show Undefend = "Unable to benefit from destructible defense"
    show Duel = "Invulnerable to everyone but the source of this effect."
    show Endure = "Health cannot go below 1."
    show Enrage = "Ignores harmful status effects other than chakra cost changes."
    show (Exhaust classes) = show classes ++ " skills cost 1 additional random chakra."
    show Expose = "Unable to reduce damage or become invulnerable."
    show (Heal x) = "Gains " ++ show x ++ " health each turn."
    show (Ignore _) = "Ignores certain effects."
    show (Invulnerable classes) = "Invulnerable to " ++ low classes ++ " skills."
    show ImmuneSelf = "Invulnerable to self-damage."
    show (Invincible classes) = "Harmful " ++ low classes ++ " skills have no effect."
    show Isolate = "Unable to affect others."
    show (Link x) = "Receives " ++ show x ++ " additional damage from the source of this effect."
    show (Parry All _) = "Counters the first skill."
    show (Parry classes _) = "Counters the first " ++ low classes ++ " skill."
    show (ParryAll All _) = "Counters all skill."
    show (ParryAll classes _) = "Counters all " ++ low classes ++ " skills."
    show Pierce = "Non-affliction skills deal piercing damage."
    show Plague = "Cannot be healed or cured."
    show Reapply = "Harmful skills received are also reflected to the source of this effect."
    show (Reduce Affliction amt x)
      | x >= 0    = "Reduces all damage received—including piercing and affliction—by " ++ showAmt amt x ++ "."
      | otherwise = "Increases all damage received—including piercing and affliction—by " ++ showAmt amt x ++ "."
    show (Reduce classes amt x)
      | x >= 0    = "Reduces " ++ low classes ++ " damage received by " ++ showAmt amt x ++ ". Does not affect piercing or affliction damage."
      | otherwise = "Increases " ++ low classes ++ " damage received by " ++ showAmt amt (-x) ++ ". Does not affect piercing or affliction damage."
    show (Redirect classes) = "Redirects " ++ low classes  ++ " harmful skills to the source of this effect."
    show Reflect = "Reflects the first harmful non-mental skill."
    show ReflectAll = "Reflects all non-mental skills."
    show (Replace _ classes _ _) = show classes ++ " skills will be temporarily acquired by the source of this effect."
    show Reveal = "Reveals invisible skills to the enemy team. This effect cannot be removed."
    show Restrict = "SkillTransform that normally affect all opponents must be targeted."
    show Seal = "Invulnerable to effects from allies."
    show Share = "If a harmful non-damage effect is received, it is also applied to the source of this effect."
    show Silence = "Unable to cause non-damage effects."
    show (Snare x)
      | x >= 0    = "Cooldowns increased by " ++ show x ++ "."
      | otherwise = "Cooldowns decreased by " ++ show (-x) ++ "."
    show (SnareTrap _ _) = "Next skill used will be negated and go on a longer cooldown."
    show (Snapshot _) = "Will be restored to an earlier state when this effect ends."
    show (Strengthen classes amt x) = show classes ++ " damaging skills deal " ++ showAmt amt x ++ " additional damage."
    show (Stun Affliction) = "Unable to deal affliction damage."
    show (Stun NonAffliction) = "Unable to deal non-affliction damage."
    show (Stun classes) = "Unable to use " ++ low classes ++ " skills."
    show (Swap classes) = "Next " ++ low classes ++ " skill will target allies instead of enemies and enemies instead of allies."
    show Taunt = "Forced to target the source of this effect."
    show (Taunting x) = "Will be forced to target the next enemy they use a skill on for " ++ show x ++ " turns."
    show (Threshold x) = "Uninjured by attacks that deal " ++ show x ++ " baseline damage or lower."
    show (Throttle _ x) = "SkillTransform will apply " ++ show x ++ " fewer turns of certain effects."
    show Uncounter = "Unable to benefit from counters or reflects."
    show Unexhaust = "All skills cost 1 fewer random chakra."
    show (Unreduce x) = "Damage reduction skills reduce " ++ show x ++ " fewer damage."
    show (Weaken classes amt x) = show classes ++ " skills deal " ++ showAmt amt x ++ " fewer damage. Does not affect affliction damage."

data Flag
    = Acted
    | Harmed
    | Targeted
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Hashable Flag where
    hashWithSalt x = hashWithSalt x . fromEnum

-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja { slot      :: Slot           -- ^ 'gameNinjas' index (0-5)
                   , character :: Character
                   , health    :: Int                    -- ^ `Starts at` @100@
                   , cooldowns :: Seq (Seq Int)          -- ^ Starts empty
                   , charges   :: Seq Int                -- ^ Starts at 4 @0@s
                   , variants  :: Seq (NonEmpty Variant) -- ^ Starts at 4 @0@s
                   , copies    :: Seq (Maybe Copy)     -- ^ Starts at 4 'Nothing's
                   , defense   :: [Defense]              -- ^ Starts empty
                   , barrier   :: [Barrier]              -- ^ Starts empty
                   , statuses  :: [Status]               -- ^ Starts empty
                   , channels  :: [Channel]              -- ^ Starts empty
                   , newChans  :: [Channel]              -- ^ Starts empty
                   , traps     :: Seq Trap               -- ^ Starts empty
                   , face      :: [Face]                 -- ^ Starts empty
                   , parrying  :: [Skill]                -- ^ Starts empty
                   , tags      :: [ChannelTag]           -- ^ Starts empty
                   , lastSkill :: Maybe Skill            -- ^ Starts at 'Nothing'
                   , flags     :: HashSet Flag           -- ^ Empty each turn
                   }
instance Eq Ninja where
    (==) = equaling \Ninja{..} -> (slot, health, cooldowns, charges)
instance Parity Ninja where
    even = Parity.even . slot

-- | 'Ignore's.
ignore :: Ninja -> [Effect]
ignore n = [f cla | cla      <- enumerate
                  , st       <- statuses n
                  , Ignore f <- (effects :: Status -> [Effect]) st]

-- | Game state.
data Game = Game { ninjas  :: Seq Ninja
                 , chakra  :: (Chakras, Chakras)
                 -- ^ Starts at @('Chakras' 0 0 0 0 0, 'Chakras' 0 0 0 0 0)@
                 , delays  :: [Delay]
                 -- ^ Starts at @(0, 0)@. Resets every turn to @(0, 0)@
                 , traps   :: Seq (Context, Play ())
                 -- ^ Starts empty
                 , playing :: Player
                 -- ^ Starts at 'Player.A'
                 , victor  :: [Player]
                 -- ^ Starts empty
                 } deriving (Eq)

data Requirement
    = Usable
    | Unusable
    | HasI Int Text
    | HasU Text
    deriving (Eq)

-- | Target destinations of 'Skill's.
data Target
    = Self          -- ^ User of 'Skill'
    | Ally          -- ^ Specific ally
    | Allies        -- ^ All allies
    | RAlly         -- ^ Random ally
    | XAlly         -- ^ Specific ally excluding 'Self'
    | XAllies       -- ^ 'Allies' excluding 'Self'
    | Enemy         -- ^ Specific enemy
    | Enemies       -- ^ All enemies
    | REnemy        -- ^ Random enemy
    | XEnemies      -- ^ Enemies excluding 'Enemy'
    | Everyone      -- ^ All 'Ninja's
    | Specific Slot -- ^ Specific ninja index in 'gameNinjas' (0-5)
    deriving (Eq)

-- | A move that a 'Character' can perform.
data Skill = Skill { name     :: Text -- ^ Name
                   , desc     :: Text -- ^ Description
                   , require  :: Requirement   -- ^ Defaults to 'Usable'
                   , classes  :: [Class]       -- ^ Defaults to @[]@
                   , cost     :: Chakras       -- ^ Defaults to 'S.empty'
                   , cooldown :: Int           -- ^ Defaults to @0@
                   , varicd   :: Bool          -- ^ Defaults to @False@
                   , charges  :: Int           -- ^ Defaults to @0@
                   , channel  :: Channeling    -- ^ Defaults to 'Instant'
                   , start    :: [(Target, Play ())] -- ^ Defaults to @[]@
                   , effects  :: [(Target, Play ())] -- ^ Defaults to @[]@
                   , disrupt  :: [(Target, Play ())] -- ^ Defaults to @[]@
                   , copying  :: Copying       -- ^ Defaults to 'NotCopied'
                   , pic      :: Bool          -- ^ Defaults to @False@
                   , changes  :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
                   }
instance Eq Skill where
    (==) = equaling \Skill{..} -> (name, desc)
instance Classed Skill where
    classes = classes

-- | Destructible barrier.
data Barrier = Barrier { amount :: Int
                       , source :: Slot
                       , name   :: Text
                       , while  :: (Context, Play ())
                       , finish :: Int -> (Context, Play ())
                       , dur    :: Int
                       } deriving (Eq)
instance TurnBased Barrier where
    getDur     = dur
    setDur d x = x { dur = d }
instance Ord Barrier where
    compare = comparing (name :: Barrier -> Text)
instance Labeled Barrier where
    name   = name
    source = source

-- | Destructible defense.
data Defense = Defense { amount :: Int
                       , source :: Slot
                       , name   :: Text
                       , dur    :: Int
                       } deriving (Eq)
instance TurnBased Defense where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Defense where
    name   = name
    source = source

-- | An 'Act' channeled over multiple turns.
data Channel = Channel { root   :: Slot
                       , skill  :: Skill
                       , target :: Slot
                       , dur    :: Channeling
                       } deriving (Eq)
instance TurnBased Channel where
    getDur     = getDur . (dur :: Channel -> Channeling)
    setDur d x = x { dur = setDur d $ (dur :: Channel -> Channeling) x }

-- | Types of channeling for 'Skill's.
data Channeling = Instant
                | Passive
                | Action Int
                | Control Int
                | Ongoing Int
                deriving (Eq, Show)
instance TurnBased Channeling where
    getDur Instant     = 0
    getDur Passive     = 0
    getDur (Action d)  = d
    getDur (Control d) = d
    getDur (Ongoing d) = d
    setDur _ Instant   = Instant
    setDur _ Passive   = Passive
    setDur d Action{}  = Action d
    setDur d Control{} = Control d
    setDur d Ongoing{} = Ongoing d

-- | Indicates that a channeled 'Skill' will affect a 'Ninja' next turn.
data ChannelTag = ChannelTag { root    :: Slot
                             , source  :: Slot
                             , skill   :: Skill
                             , ghost   :: Bool
                             , dur     :: Int
                             } deriving (Eq)
instance TurnBased ChannelTag where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled ChannelTag where
    name   = (name :: Skill -> Text) . (skill :: ChannelTag -> Skill)
    source = root

-- | 'Original', 'Shippuden', or 'Reanimated'.
data Category
    = Original
    | Shippuden
    | Reanimated
    deriving (Show, Eq, Ord, Bounded, Enum)
instance ToMarkup Category where
    toMarkup = toMarkup . show

-- | An out-of-game character.
data Character = Character { name     :: Text
                           , bio      :: Text
                           , skills   :: NonEmpty (NonEmpty Skill)
                           , hooks    :: Seq (Trigger, Int -> Ninja -> Ninja)
                           , category :: Category
                           }
instance Eq Character where
    (==) = equaling \Character{..} -> (name, category)

instance Show Character where
    show (Character name _ _ _ Original)   = unpack name
    show (Character name _ _ _ Shippuden)  = unpack name ++ " (S)"
    show (Character name _ _ _ Reanimated) = unpack name ++ " (R)"

-- | Conditions to activate a 'Trap'.
data Trigger
    = OnAction Class
    | OnNoAction
    | OnBreak Text
    | OnChakra
    | OnCounter Class
    | OnCounterAll
    | OnDamage
    | OnDamaged Class
    | OnDeath
    | OnHarm
    | OnNoHarm
    | OnHarmed Class
    | OnHealed
    | PerHealed
    | OnHelped
    | OnImmune
    | OnReflectAll
    | OnRes
    | OnStun
    | OnStunned
    | PerDamage
    | PerDamaged
    | TrackDamage
    | TrackDamaged
    deriving (Eq)
instance Show Trigger where
    show (OnAction  All) = "Trigger: Use any skill"
    show (OnAction  cla) = "Trigger: Use " ++ low cla ++ " skills"
    show (OnBreak   name)   = "Trigger: Lose all destructible defense from '" ++ unpack name ++ "'"
    show OnChakra        = "Trigger: Steal or remove chakra"
    show (OnCounter All) = "Next harmful skill is countered."
    show (OnCounter Uncounterable) = "Next skill is negated."
    show (OnCounter cla) = "Next harmful " ++ low cla ++ " skill is countered."
    show OnCounterAll    = "All skills are countered."
    show OnDamage        = "Trigger: Deal damage"
    show (OnDamaged All) = "Trigger: Receive damage"
    show (OnDamaged cla) = "Trigger: Receive " ++ low cla ++ " damage"
    show OnDeath         = "Trigger: Die"
    show OnHarm          = "Trigger: Use harmful skill"
    show (OnHarmed All)  = "Trigger: Be affected by a new harmful skill"
    show (OnHarmed cla)  = "Trigger: Be affected by a new " ++ low cla ++ " harmful skill"
    show OnHealed        = "Trigger: Receive healing"
    show OnHelped        = "Trigger: Be affected by a new skill from an ally"
    show OnImmune        = "Trigger: Become invulnerable"
    show OnNoAction      = "Trigger: Do not use a new skill"
    show OnNoHarm        = "Trigger: Do not use a new harmful skill"
    show OnReflectAll    = "All skills are reflected."
    show OnRes           = "Trigger: Reach 0 health"
    show OnStun          = "Trigger: Apply a stun"
    show OnStunned       = "Trigger: Stunned"
    show PerDamage       = show OnDamage
    show PerDamaged      = show (OnDamaged All)
    show PerHealed       = show OnHealed
    show TrackDamage     = show OnDamage
    show TrackDamaged    = show PerDamaged

data Variant = Variant { variant   :: Int -- ^ Index in 'characterSkills'
                       , ownCd     :: Bool -- ^ Uses a different cooldown than the baseline 'Skill'
                       , name      :: Text
                       , fromSkill :: Bool -- ^ Duration is based on a 'Skill'
                       , dur       :: Int
                       } deriving (Eq, Show)
instance TurnBased Variant where
    getDur        = dur
    setDur x vari = vari { dur = x }

-- | A 'Skill' copied from a different character.
data Copy = Copy { skill :: Skill
                 , dur   :: Int
                 } deriving (Eq)
instance TurnBased Copy where
    getDur = dur
    setDur d x@Copy{skill} = x { dur   = d
                                 , skill = f $ copying skill
                                 }
        where
          f (Shallow b _) = skill { copying = Shallow b d }
          f (Deep    b _) = skill { copying = Deep    b d }
          f NotCopied     = skill

data Copying
    = Shallow Slot Int -- ^ No cooldown or chakra cost.
    | Deep Slot Int    -- ^ Cooldown and chakra cost.
    | NotCopied
    deriving (Eq)

-- | Applies an effect after several turns.
data Delay = Delay { user   :: Slot
                   , skill  :: Skill
                   , effect :: (Context, Play ())
                   , dur    :: Int
                   } deriving (Eq)
instance TurnBased Delay where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Delay where
    name   = (name :: Skill -> Text) . (skill :: Delay -> Skill)
    source = user

-- | Applies 'Transform's when a 'Status' ends.
data Bomb
    = Done   -- ^ Applied with both 'Expire' and 'Remove'
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    | Remove -- ^ Applied when a 'Status' is removed prematurely
    deriving (Enum, Eq, Show)

-- | A status effect affecting a 'Ninja'.
data Status = Status { amount  :: Int  -- ^ Starts at 1
                     , name    :: Text -- ^ Label
                     , root    :: Slot -- ^ Owner of the 'statusSkill'
                     , source  :: Slot -- ^ Original user
                     , user    :: Slot -- ^ Direct user (e.g. if reflected)
                     , skill   :: Skill
                     , effects :: [Effect]
                     , classes :: [Class]
                     , bombs   :: [(Bomb, Play ())]
                     , maxDur  :: Int
                     , dur     :: Int
                     }
instance Eq Status where
    (==) = equaling \Status{..} -> (name, source, maxDur, classes)
instance TurnBased Status where
    getDur     = dur
    setDur d x = x { dur = d }
instance Ord Status where
    compare = comparing (name :: Status -> Text)
instance Labeled Status where
    name   = name
    source = source
instance Classed Status where
    classes = classes

data Direction
    = To
    | From
    | Per
    deriving (Enum, Eq, Show)

-- | A trap which gets triggered when a 'Ninja' meets the conditions of a 'Trigger'.
data Trap = Trap { direction :: Direction
                 , trigger   :: Trigger
                 , name      :: Text
                 , desc      :: Text
                 , source    :: Slot
                 , effect    :: Int -> (Context, Play ())
                 , classes   :: [Class]
                 , tracker   :: Int
                 , dur       :: Int
                 }
instance Eq Trap where
    (==) = equaling \Trap{..} -> (direction, trigger, name, source, dur)
instance TurnBased Trap where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Trap where
    name   = name
    source = source


-- Black magic
instance Eq (a -> b) where
    (==) = const $ const True
