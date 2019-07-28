{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
module Model.Internal where

import ClassyPrelude

import           Control.Monad.Reader (local, mapReaderT)
import           Control.Monad.Trans.Accum (AccumT, mapAccumT)
import           Control.Monad.Trans.Except (ExceptT, mapExceptT)
import           Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import           Control.Monad.Trans.Select (SelectT, mapSelectT)
import           Control.Monad.Trans.State.Strict (StateT, mapStateT)
import           Control.Monad.Trans.Writer (WriterT, mapWriterT)
import           Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import           Data.Aeson ((.=), ToJSON(..), object)
import           Data.List.NonEmpty (NonEmpty(..))
import           Text.Blaze (ToMarkup(..))
import           Yesod.WebSockets (WebSocketsT)

import           Core.Util ((∈), Lift, enumerate)
import qualified Class.Classed as Classed
import           Class.Classed (Classed)
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled
import           Class.Labeled (Labeled)
import           Class.Random (MonadRandom)
import           Class.TurnBased (TurnBased(..))
import           Model.Class (Class(..), ClassSet)
import           Model.Chakra (Chakras(..))
import           Model.Defense (Defense(..))
import           Model.Duration (Duration, sync, unsync)
import           Model.Face (Face(..))
import           Model.Player (Player)
import qualified Model.Slot as Slot
import           Model.Slot (Slot(..))
import           Model.Variant (Variant(..))

data Amount = Flat | Percent deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Constructor
    = Only Effect
    | Any (Class -> Effect)

instance Eq Constructor where
    Only x == Only y = x == y
    Any  x == Any  y = x All == y All
    Any  x == Only y = y ∈ enumerate x
    Only x == Any  y = x ∈ enumerate y

-- | Effects of 'Status'es.
data Effect
    = Afflict      Int               -- ^ Deals damage every turn
    | AntiCounter                    -- ^ Cannot be countered or reflected
    | Bleed        Class Amount Int  -- ^ Adds to damage received
    | Bless        Int               -- ^ Adds to healing 'Skill's
    | Block                          -- ^ Treats user as 'Invulnerable'
    | Boost        Int               -- ^ Scales effects from allies
    | Build        Int               -- ^ Adds to destructible defense 'Skill'
    | Counter      Class             -- ^ Counters the first 'Skill's
    | CounterAll   Class             -- ^ 'Counter's without being removed
    | Duel         Slot              -- ^ 'Invulnerable' to everyone but user
    | Endure                         -- ^ Health cannot go below 1
    | Enrage                         -- ^ Ignore all harmful status effects
    | Exhaust      Class             -- ^ 'Skill's cost 1 additional random chakra
    | Expose                         -- ^ Cannot reduce damage or be 'Invulnerable'
    | Heal         Int               -- ^ Heals every turn
    | Ignore       Constructor       -- ^ Invulnerable to certain effects
    | Invulnerable Class             -- ^ Invulnerable to enemy 'Skill's
    | ImmuneSelf                     -- ^ Invulnerable to self-caused damage
    | Invincible   Class             -- ^ Like 'Invulnerable', but targetable
    | Parry        Class (Play ())   -- ^ 'Counter' and trigger an effect
    | ParryAll     Class (Play ())   -- ^ 'Parry' repeatedly
    | Pierce                         -- ^ Damage attacks become piercing
    | Plague                         -- ^ Invulnerable to healing and curing
    | Reduce       Class Amount Int  -- ^ Reduces damage by an amount
    | Redirect     Class Slot        -- ^ Transfers harmful 'Skill's
    | Reflect                        -- ^ Reflects the first 'Skill'
    | ReflectAll                     -- ^ 'Reflect' repeatedly
    | Restrict                       -- ^ Forces AoE attacks to be single-target
    | Reveal                         -- ^ Makes 'Invisible' effects visible
    | Seal                           -- ^ Ignore all friendly 'Skill's
    | Share        Slot              -- ^ Harmful skills are also applied to a target
    | Silence                        -- ^ Unable to cause non-damage effects
    | Snapshot     Ninja             -- ^ Saves a snapshot of the current state
    | Snare        Int               -- ^ Increases cooldowns
    | SnareTrap    Class Int         -- ^ Negates next skill and increases cooldown
    | Strengthen   Class Amount Int  -- ^ Adds to all damage dealt
    | Stun         Class             -- ^ Unable to use 'Skill's
    | Swap         Class             -- ^ Target's skills swap enemies and allies
    | Taunt        Slot              -- ^ Forced to attack a target
    | Threshold    Int               -- ^ Invulnerable to baseline damage below a threhold
    | Throttle     Int Constructor   -- ^ Applying an effect lasts fewer turns
    | Undefend                       -- ^ Does not benefit from destructible defense
    | Uncounter                      -- ^ Cannot counter or reflect
    | Unexhaust                      -- ^ Decreases chakra costs by 1 random
    | Unreduce     Int               -- ^ Reduces damage reduction 'Skill's
    | Weaken       Class Amount Int  -- ^ Lessens damage dealt
    -- | Copies a skill into user's skill slot
    | Replace Duration
              Class
              Int   -- ^ Skill index of user to copy into
              Bool  -- ^ Include non-harmful 'Skill's
              deriving (Eq)
instance Classed Effect where
    classes (Bleed cla _ _)      = singletonSet cla
    classes (Counter cla)        = singletonSet cla
    classes (CounterAll cla)     = singletonSet cla
    classes (Exhaust cla)        = singletonSet cla
    classes (Invulnerable cla)   = singletonSet cla
    classes (Invincible cla)     = singletonSet cla
    classes (Parry cla _)        = singletonSet cla
    classes (ParryAll cla _)     = singletonSet cla
    classes (Reduce cla _ _)     = singletonSet cla
    classes (Redirect cla _)     = singletonSet cla
    classes (SnareTrap cla _)    = singletonSet cla
    classes (Strengthen cla _ _) = singletonSet cla
    classes (Stun cla)           = singletonSet cla
    classes (Swap cla)           = singletonSet cla
    classes (Weaken cla _ _)     = singletonSet cla
    classes (Replace _ cla _ _)  = singletonSet cla
    classes _                    = mempty

instance ToJSON Effect where
    toJSON x = object
      [ "desc"    .= tshow x
      , "helpful" .= helpful x
      , "sticky"  .= sticky x
      , "trap"    .= False
      ]

helpful :: Effect -> Bool
helpful Afflict{}      = False
helpful AntiCounter    = True
helpful (Bleed _ _ x)  = x < 0
helpful Bless{}        = True
helpful Block{}        = False
helpful Boost{}        = True
helpful (Build x)      = x >= 0
helpful Counter{}      = True
helpful CounterAll{}   = True
helpful Duel{}         = True
helpful Endure         = True
helpful Enrage         = True
helpful Exhaust{}      = False
helpful Expose         = False
helpful Heal{}         = True
helpful Invulnerable{} = True
helpful ImmuneSelf     = True
helpful Ignore{}       = True
helpful Invincible{}   = True
helpful Parry{}        = True
helpful ParryAll {}    = True
helpful Pierce         = True
helpful Plague         = False
helpful (Reduce _ _ x) = x >= 0
helpful Redirect{}     = True
helpful Reflect        = True
helpful ReflectAll     = True
helpful Replace{}      = False
helpful Restrict       = False
helpful Reveal         = False
helpful Seal           = False
helpful Share{}        = False
helpful Silence        = False
helpful Snapshot{}     = True
helpful (Snare x)      = x < 0
helpful SnareTrap{}    = False
helpful Strengthen{}   = True
helpful Stun{}         = False
helpful Swap{}         = False
helpful Taunt{}        = False
helpful Threshold{}    = True
helpful Throttle{}     = False
helpful Uncounter      = False
helpful Undefend       = False
helpful Unexhaust      = True
helpful Unreduce{}     = False
helpful Weaken{}       = False

-- | Effect cannot be removed.
sticky :: Effect -> Bool
sticky Block{}        = True
sticky Counter{}      = True
sticky CounterAll{}   = True
sticky Enrage         = True
sticky Invulnerable{} = True
sticky Invincible{}   = True
sticky Parry{}        = True
sticky ParryAll{}     = True
sticky Redirect{}     = True
sticky Replace{}      = True
sticky Reflect        = True
sticky ReflectAll     = True
sticky Restrict       = True
sticky Reveal         = True
sticky Share{}        = True
sticky Snapshot{}     = True
sticky Swap{}         = True
sticky _              = False

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
    show (Duel _) = "Invulnerable to everyone but a specific target."
    show Endure = "Health cannot go below 1."
    show Enrage = "Ignores status effects from enemies except chakra cost changes."
    show (Exhaust classes) = show classes ++ " skills cost 1 additional random chakra."
    show Expose = "Unable to reduce damage or become invulnerable."
    show (Heal x) = "Gains " ++ show x ++ " health each turn."
    show (Ignore _) = "Ignores certain effects."
    show (Invulnerable classes) = "Invulnerable to " ++ low classes ++ " skills."
    show ImmuneSelf = "Invulnerable to self-damage."
    show (Invincible classes) = "Harmful " ++ low classes ++ " skills have no effect."
    show (Parry All _) = "Counters the first skill."
    show (Parry classes _) = "Counters the first " ++ low classes ++ " skill."
    show (ParryAll All _) = "Counters all skill."
    show (ParryAll classes _) = "Counters all " ++ low classes ++ " skills."
    show Pierce = "Non-affliction skills deal piercing damage."
    show Plague = "Cannot be healed or cured."
    show (Reduce Affliction amt x)
      | x >= 0    = "Reduces all damage received—including piercing and affliction—by " ++ showAmt amt x ++ "."
      | otherwise = "Increases all damage received—including piercing and affliction—by " ++ showAmt amt x ++ "."
    show (Reduce classes amt x)
      | x >= 0    = "Reduces " ++ low classes ++ " damage received by " ++ showAmt amt x ++ ". Does not affect piercing or affliction damage."
      | otherwise = "Increases " ++ low classes ++ " damage received by " ++ showAmt amt (-x) ++ ". Does not affect piercing or affliction damage."
    show (Redirect classes _) = "Redirects " ++ low classes  ++ " harmful skills to a different target."
    show Reflect = "Reflects the first harmful non-mental skill."
    show ReflectAll = "Reflects all non-mental skills."
    show (Replace _ classes _ _) = show classes ++ " skills will be temporarily acquired by the user of this effect."
    show Reveal = "Reveals invisible skills to the enemy team. This effect cannot be removed."
    show Restrict = "Skills that normally affect all opponents must be targeted."
    show Seal = "Invulnerable to effects from allies."
    show (Share _) = "Harmful skills received are also reflected to another target."
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
    show (Taunt _) = "Can only affect a specific target."
    show (Threshold x) = "Uninjured by attacks that deal " ++ show x ++ " baseline damage or lower."
    show (Throttle x _) = "Skills will apply " ++ show x ++ " fewer turns of certain effects."
    show Uncounter = "Unable to benefit from counters or reflects."
    show Unexhaust = "All skills cost 1 fewer random chakra."
    show (Unreduce x) = "Damage reduction skills reduce " ++ show x ++ " fewer damage."
    show (Weaken classes amt x) = show classes ++ " skills deal " ++ showAmt amt x ++ " fewer damage. Does not affect affliction damage."

-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja { slot      :: Slot           -- ^ 'Model.Game.Ninjas' index (0-5)
                   , character :: Character
                   , health    :: Int                    -- ^ Starts at @100@
                   , cooldowns :: Vector (Seq Int)       -- ^ Starts empty
                   , charges   :: Vector Int             -- ^ Starts at @0@s
                   , variants  :: Vector (NonEmpty Variant) -- ^ Starts at @0@s
                   , copies    :: Vector (Maybe Copy)     -- ^ Starts at 'Nothing's
                   , defense   :: [Defense]              -- ^ Starts empty
                   , barrier   :: [Barrier]              -- ^ Starts empty
                   , statuses  :: [Status]               -- ^ Starts empty
                   , channels  :: [Channel]              -- ^ Starts empty
                   , newChans  :: [Channel]              -- ^ Starts empty
                   , traps     :: Seq Trap               -- ^ Starts empty
                   , face      :: [Face]                 -- ^ Starts empty
                   , lastSkill :: Maybe Skill            -- ^ Starts at 'Nothing'
                   , triggers  :: Set Trigger            -- ^ Empty at the start of each turn
                   , counters  :: [SavedPlay]            -- ^ Empty at the start of each turn
                   , effects   :: [Effect]               -- ^ Empty at the start of each turn
                   }
instance Eq Ninja where
    (==) = (==) `on` \Ninja{..} -> (slot, health, cooldowns, charges)
instance Parity Ninja where
    even = Parity.even . slot

-- | Game state.
data Game = Game { chakra  :: (Chakras, Chakras)
                 -- ^ Starts at @('Chakras' 0 0 0 0 0, 'Chakras' 0 0 0 0 0)@
                 , delays  :: [Delay]
                 -- ^ Starts at @(0, 0)@. Resets every turn to @(0, 0)@
                 , playing :: Player
                 -- ^ Starts at 'Player.A'
                 , victor  :: [Player]
                 -- ^ Starts empty
                 }

data Requirement
    = Usable
    | Unusable
    | HasI Int Text
    | HasU Text
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)

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
    | Specific Slot -- ^ Specific ninja index in 'ninjas' (0-5)
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)

-- | A move that a 'Character' can perform.
data Skill = Skill { name      :: Text -- ^ Name
                   , desc      :: Text -- ^ Description
                   , require   :: Requirement   -- ^ Defaults to 'Usable'
                   , classes   :: ClassSet      -- ^ Defaults to @mempty@
                   , cost      :: Chakras       -- ^ Defaults to '[]'
                   , cooldown  :: Duration      -- ^ Defaults to @0@
                   , varicd    :: Bool          -- ^ Defaults to @False@
                   , charges   :: Int           -- ^ Defaults to @0@
                   , channel   :: Channeling    -- ^ Defaults to 'Instant'
                   , start     :: [(Target, Play ())] -- ^ Defaults to @[]@
                   , effects   :: [(Target, Play ())] -- ^ Defaults to @[]@
                   , interrupt :: [(Target, Play ())] -- ^ Defaults to @[]@
                   , copying   :: Copying       -- ^ Defaults to 'NotCopied'
                   , pic       :: Bool          -- ^ Defaults to @False@
                   , changes   :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
                   } deriving (Generic)
instance ToJSON Skill where
    toJSON Skill{..} = object
        [ "name"      .= name
        , "desc"      .= desc
        , "require"   .= require
        , "classes"   .= classes
        , "cost"      .= cost
        , "cooldown"  .= cooldown
        , "varicd"    .= varicd
        , "charges"   .= charges
        , "channel"   .= channel
        , "start"     .= start
        , "effects"   .= effects
        , "interrupt" .= interrupt
        , "copying"   .= copying
        , "pic"       .= pic
        ]
instance Classed Skill where
    classes = classes

-- | Destructible barrier.
data Barrier = Barrier { amount :: Int
                       , user   :: Slot
                       , name   :: Text
                       , while  :: SavedPlay
                       , finish :: Int -> SavedPlay
                       , dur    :: Int
                       } deriving (Generic)
instance ToJSON Barrier where
    toJSON Barrier{..} = object
        [ "amount" .= amount
        , "user"   .= user
        , "name"   .= name
        , "dur"    .= dur
        ]
instance TurnBased Barrier where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Barrier where
    name   = name
    user = user

-- | An 'Model.Act.Act' channeled over multiple turns.
data Channel = Channel { source :: Slot
                       , skill  :: Skill
                       , target :: Slot
                       , dur    :: Channeling
                       } deriving (Generic, ToJSON)

instance Classed Channel where
    classes = Classed.classes . (skill :: Channel -> Skill)

instance TurnBased Channel where
    getDur     = getDur . (dur :: Channel -> Channeling)
    setDur d x = x { dur = setDur d $ (dur :: Channel -> Channeling) x }

-- | Types of channeling for 'Skill's.
data Channeling
    = Instant
    | Passive
    | Action  Duration
    | Control Duration
    | Ongoing Duration
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)
instance TurnBased Channeling where
    getDur Instant     = 0
    getDur Passive     = 0
    getDur (Action d)  = sync d
    getDur (Control d) = sync d
    getDur (Ongoing d) = sync d
    setDur _ Instant   = Instant
    setDur _ Passive   = Passive
    setDur d Action{}  = Action $ unsync d
    setDur d Control{} = Control $ unsync d
    setDur d Ongoing{} = Ongoing $ unsync d

-- | 'Original', 'Shippuden', or 'Reanimated'.
data Category
    = Original
    | Shippuden
    | Reanimated
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)
instance ToMarkup Category where
    toMarkup = toMarkup . show

-- | An out-of-game character.
data Character = Character { name     :: Text
                           , bio      :: Text
                           , skills   :: NonEmpty (NonEmpty Skill)
                           , hooks    :: Seq (Trigger, Int -> Ninja -> Ninja)
                           , category :: Category
                           } deriving (Generic)
instance ToJSON Character where
    toJSON Character{..} = object
        [ "name"     .= name
        , "bio"      .= bio
        , "skills"   .= skills
        , "category" .= category
        ]

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
    | OnHarmed Class
    | OnHealed
    | OnHelped
    | OnImmune
    | OnReflectAll
    | OnRes
    | OnStun
    | OnStunned
    | PerDamage
    | PerHealed
    | PerDamaged
    deriving (Eq, Ord)

instance ToJSON Trigger where
    toJSON = toJSON . show

instance Classed Trigger where
    classes (OnAction cla)  = singletonSet cla
    classes (OnCounter cla) = singletonSet cla
    classes (OnDamaged cla) = singletonSet cla
    classes (OnHarmed cla)  = singletonSet cla
    classes _               = mempty

instance Show Trigger where
    show (OnAction  All)  = "Trigger: Use any skill"
    show (OnAction  cla)  = "Trigger: Use " ++ low cla ++ " skills"
    show (OnBreak   name) = "Trigger: Lose all destructible defense from '" ++ unpack name ++ "'"
    show OnChakra         = "Trigger: Steal or remove chakra"
    show (OnCounter All)  = "Next harmful skill is countered."
    show (OnCounter Uncounterable) = "Next skill is negated."
    show (OnCounter cla)  = "Next harmful " ++ low cla ++ " skill is countered."
    show OnCounterAll     = "All skills are countered."
    show OnDamage         = "Trigger: Deal damage"
    show (OnDamaged All)  = "Trigger: Receive damage"
    show (OnDamaged cla)  = "Trigger: Receive " ++ low cla ++ " damage"
    show OnDeath          = "Trigger: Die"
    show OnHarm           = "Trigger: Use harmful skill"
    show (OnHarmed All)   = "Trigger: Be affected by a new harmful skill"
    show (OnHarmed cla)   = "Trigger: Be affected by a new " ++ low cla ++ " harmful skill"
    show OnHealed         = "Trigger: Receive healing"
    show OnHelped         = "Trigger: Be affected by a new skill from an ally"
    show OnImmune         = "Trigger: Become invulnerable"
    show OnNoAction       = "Trigger: Do not use a new skill"
    show OnReflectAll     = "All skills are reflected."
    show OnRes            = "Trigger: Reach 0 health"
    show OnStun           = "Trigger: Apply a stun"
    show OnStunned        = "Trigger: Stunned"
    show PerDamage        = show OnDamage
    show PerDamaged       = show (OnDamaged All)
    show PerHealed        = show OnHealed

-- | A 'Skill' copied from a different character.
data Copy = Copy { skill :: Skill
                 , dur   :: Int
                 } deriving (Generic, ToJSON)

instance Classed Copy where
    classes = Classed.classes . (skill :: Copy -> Skill)

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
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)

-- | Applies an effect after several turns.
data Delay = Delay { user   :: Slot
                   , skill  :: Skill
                   , effect :: () -> SavedPlay
                   , dur    :: Int
                   }

instance Classed Delay where
    classes = Classed.classes . (skill :: Delay -> Skill)

instance TurnBased Delay where
    getDur     = dur
    setDur d x = x { dur = d }

instance Labeled Delay where
    name   = (name :: Skill -> Text) . (skill :: Delay -> Skill)
    user = user

-- | Applies actions when a 'Status' ends.
data Bomb
    = Done   -- ^ Applied with both 'Expire' and 'Remove'
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    | Remove -- ^ Applied when a 'Status' is removed prematurely
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

-- | A status effect affecting a 'Ninja'.
data Status = Status { amount  :: Int  -- ^ Starts at 1
                     , name    :: Text -- ^ Label
                     , source  :: Slot -- ^ Owner of the 'Status.skill'
                     , user    :: Slot -- ^ User
                     , skill   :: Skill
                     , effects :: [Effect]
                     , classes :: ClassSet
                     , bombs   :: [(Bomb, Play ())]
                     , maxDur  :: Int
                     , dur     :: Int
                     } deriving (Generic, ToJSON)
instance Eq Status where
    (==) = (==) `on` \Status{..} -> (name, user, maxDur, classes)
instance Ord Status where
    compare = compare `on` (name :: Status -> Text)
instance TurnBased Status where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Status where
    name   = name
    user = user
instance Classed Status where
    classes = classes

data Direction
    = To
    | From
    | Per
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

-- | A trap which gets triggered when a 'Ninja' meets the conditions of a 'Trigger'.
data Trap = Trap { direction :: Direction
                 , trigger   :: Trigger
                 , name      :: Text
                 , desc      :: Text
                 , user      :: Slot
                 , effect    :: Int -> SavedPlay
                 , classes   :: ClassSet
                 , tracker   :: Int
                 , dur       :: Int
                 } deriving (Generic)
instance ToJSON Trap where
    toJSON Trap{..} = object
        [ "direction" .= direction
        , "trigger"   .= trigger
        , "name"      .= name
        , "desc"      .= desc
        , "user"      .= user
        , "classes"   .= classes
        , "tracker"   .= tracker
        , "dur"       .= dur
        ]
instance Eq Trap where
    (==) = (==) `on` \Trap{..} -> (direction, trigger, name, user, dur)
instance TurnBased Trap where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Trap where
    name   = name
    user = user
instance Classed Trap where
    classes = classes


data Context = Context { skill   :: Skill
                       , user    :: Slot
                       , target  :: Slot
                       , new     :: Bool
                       } deriving (Generic, ToJSON)

instance MonadRandom m => MonadRandom (ReaderT Context m)

class Monad m => MonadGame m where
    game      :: m Game
    alter     :: (Game -> Game) -> m ()
    ninjas    :: m (Vector Ninja)
    ninja     :: Slot -> m Ninja
    write     :: Slot -> Ninja -> m ()
    modify    :: Slot -> (Ninja -> Ninja) -> m ()
    modifyAll :: (Ninja -> Ninja) -> m ()
    modifyAll f = traverse_ (flip modify f) Slot.all

    default game   :: Lift MonadGame m => m Game
    game     = lift game
    default alter  :: Lift MonadGame m => (Game -> Game) -> m ()
    alter    = lift . alter
    default ninjas :: Lift MonadGame m => m (Vector Ninja)
    ninjas   = lift ninjas
    default ninja  :: Lift MonadGame m => Slot -> m Ninja
    ninja    = lift . ninja
    default write  :: Lift MonadGame m => Slot -> Ninja -> m ()
    write i  = lift . write i
    default modify :: Lift MonadGame m => Slot -> (Ninja -> Ninja) -> m ()
    modify i = lift . modify i

class MonadGame m => MonadPlay m where
    context :: m Context
    with    :: ∀ a. (Context -> Context) -> m a -> m a

    default context :: Lift MonadPlay m => m Context
    context = lift context

instance MonadGame m => MonadPlay (ReaderT Context m) where
    context = ask
    with    = local

type PlayConstraint a = ∀ m. (MonadRandom m, MonadPlay m) => m a

newtype Play a = Play (PlayConstraint a)
instance Eq (Play a) where
    (==) = const $ const True
instance ToJSON (Play a) where
    toJSON = const $ toJSON (Nothing :: Maybe ())

type SavedPlay = (Context, Play ())

instance MonadGame m => MonadGame (ExceptT e m)
instance MonadGame m => MonadGame (IdentityT m)
instance MonadGame m => MonadGame (MaybeT m)
instance MonadGame m => MonadGame (SelectT r m)
instance MonadGame m => MonadGame (StateT r m)
instance MonadGame m => MonadGame (ReaderT Context m)
instance MonadGame m => MonadGame (WebSocketsT m)
instance (MonadGame m, Monoid w) => MonadGame (WriterT w m)
instance (MonadGame m, Monoid w) => MonadGame (AccumT w m)

instance MonadPlay m => MonadPlay (ExceptT e m) where
    with f = mapExceptT $ with f
instance MonadPlay m => MonadPlay (IdentityT m) where
    with f = mapIdentityT $ with f
instance MonadPlay m => MonadPlay (MaybeT m) where
    with f = mapMaybeT $ with f
instance MonadPlay m => MonadPlay (SelectT r m) where
    with f = mapSelectT $ with f
instance MonadPlay m => MonadPlay (StateT r m) where
    with f = mapStateT $ with f
instance MonadPlay m => MonadPlay (WebSocketsT m) where
    with f = mapReaderT $ with f
instance (MonadPlay m, Monoid w) => MonadPlay (WriterT w m) where
    with f = mapWriterT $ with f
instance (MonadPlay m, Monoid w) => MonadPlay (AccumT w m) where
    with f = mapAccumT $ with f
