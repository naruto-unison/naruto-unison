{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_HADDOCK hide, not-home #-}
module Model.Internal where

import ClassyPrelude

import Control.Monad.Reader (local, mapReaderT)
import Control.Monad.Trans.Accum (AccumT, mapAccumT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.Select (SelectT, mapSelectT)
import Control.Monad.Trans.Writer (WriterT, mapWriterT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Data.Aeson ((.=), ToJSON(..), object)
import Data.Enum.Set.Class (AsEnumSet(..), EnumSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word16)
import Text.Blaze (ToMarkup(..))
import Yesod.WebSockets (WebSocketsT)

import           Core.Util (Lift)
import qualified Class.Classed as Classed
import           Class.Classed (Classed)
import           Class.Display (Display(..))
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled
import           Class.Labeled (Labeled)
import           Class.Random (MonadRandom)
import           Class.TurnBased (TurnBased(..))
import           Model.Class (Class(..), lower)
import           Model.Chakra (Chakras(..))
import           Model.Defense (Defense(..))
import           Model.Duration (Duration, sync, unsync)
import           Model.Effect (Effect(..))
import           Model.Face (Face(..))
import           Model.Player (Player)
import qualified Model.Slot as Slot
import           Model.Slot (Slot(..))
import           Model.Variant (Variant(..))

-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja { slot      :: Slot                   -- ^ 'Model.Game.Ninjas' index (0-5)
                   , character :: Character
                   , health    :: Int                    -- ^ Starts at @100@
                   , cooldowns :: Seq (Seq Int)          -- ^ Starts empty
                   , charges   :: Seq Int                -- ^ Starts at @0@s
                   , variants  :: Seq (NonEmpty Variant) -- ^ Starts at @0@s
                   , copies    :: Seq (Maybe Copy)       -- ^ Starts at @Nothing@s
                   , defense   :: [Defense]              -- ^ Starts empty
                   , barrier   :: [Barrier]              -- ^ Starts empty
                   , statuses  :: [Status]               -- ^ Starts empty
                   , channels  :: [Channel]              -- ^ Starts empty
                   , newChans  :: [Channel]              -- ^ Starts empty
                   , traps     :: [Trap]                 -- ^ Starts empty
                   , face      :: [Face]                 -- ^ Starts empty
                   , lastSkill :: Maybe Skill            -- ^ Starts at @Nothing@
                   , triggers  :: Set Trigger            -- ^ Empty at the start of each turn
                   , effects   :: [Effect]               -- ^ Empty at the start of each turn
                   , acted     :: Bool                   -- ^ False at the start of each turn
                   }
instance Parity Ninja where
    even = Parity.even . slot
    {-# INLINE even #-}

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
    | HasU Int Text
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)

-- | Target destinations of 'Skill's.
data Target
    = Self    -- ^ User of 'Skill'
    | Ally    -- ^ Specific ally
    | Allies  -- ^ All allies
    | RAlly   -- ^ Random ally
    | XAlly   -- ^ Specific ally excluding 'Self'
    | XAllies -- ^ 'Allies' excluding 'Self'
    | Enemy   -- ^ Specific enemy
    | Enemies -- ^ All enemies
    | REnemy  -- ^ Random enemy
    | XEnemies -- ^ Enemies excluding 'Enemy'
    | Everyone -- ^ All 'Ninja's
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

instance AsEnumSet Target where
    type EnumSetRep Target = Word16

-- | A move that a 'Character' can perform.
data Skill = Skill { name      :: Text              -- ^ Name
                   , desc      :: Text              -- ^ Description
                   , require   :: Requirement       -- ^ Defaults to 'Usable'
                   , classes   :: EnumSet Class     -- ^ Defaults to empty
                   , cost      :: Chakras           -- ^ Defaults to empty
                   , cooldown  :: Duration          -- ^ Defaults to @0@
                   , varicd    :: Bool              -- ^ Defaults to @False@
                   , charges   :: Int               -- ^ Defaults to @0@
                   , dur       :: Channeling        -- ^ Defaults to 'Instant'
                   , start     :: [Runnable Target] -- ^ Defaults to empty
                   , effects   :: [Runnable Target] -- ^ Defaults to empty
                   , interrupt :: [Runnable Target] -- ^ Defaults to empty
                   , copying   :: Copying           -- ^ Defaults to 'NotCopied'
                   , pic       :: Bool              -- ^ Defaults to @False@
                   , changes   :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
                   }
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
        , "dur"       .= dur
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
                       , while  :: Runnable Context
                       , finish :: Int -> Runnable Context
                       , dur    :: Int
                       }
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
                           , hooks    :: [(Trigger, Int -> Ninja -> Ninja)]
                           , category :: Category
                           }
instance ToJSON Character where
    toJSON Character{..} = object
        [ "name"     .= name
        , "bio"      .= bio
        , "skills"   .= skills
        , "category" .= category
        ]

-- | Conditions to activate a 'Trap'.
data Trigger
    = Counter Class
    | CounterAll Class
    | Countered Class
    | OnAction Class
    | OnNoAction
    | OnBreak Text
    | OnChakra
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
    deriving (Eq, Ord, Show, Read)

instance ToJSON Trigger where
    toJSON = toJSON . display'

instance Classed Trigger where
    classes (Counter cla)      = singletonSet cla
    classes (CounterAll cla)   = singletonSet cla
    classes (Countered cla)    = singletonSet cla
    classes (OnAction cla)     = singletonSet cla
    classes (OnDamaged cla)    = singletonSet cla
    classes (OnHarmed cla)     = singletonSet cla
    classes _                  = mempty

instance Display Trigger where
    display (Counter Uncounterable)    = "Next harmful skill received will be negated."
    display (CounterAll Uncounterable) = "All harmful skills received will be negated."
    display (Countered Uncounterable)  = "Next harmful skill used will be negated."
    display (Counter All)      = "Next harmful skill received will be countered."
    display (CounterAll All)   = "All harmful skills received will be countered."
    display (Countered All)    = "Next harmful skill used will be countered."
    display (Counter cla)      = "Next harmful " ++ lower cla ++ " skill received will be countered."
    display (CounterAll cla)   = "All harmful " ++ lower cla ++ " skills received will be countered."
    display (Countered cla)    = "Next harmful " ++ lower cla ++ " skill used will be countered."
    display (OnAction  All)    = "Trigger: Use any skill"
    display (OnAction  cla)    = "Trigger: Use " ++ lower cla ++ " skills"
    display (OnBreak   name)   = "Trigger: Lose all destructible defense from '" ++ display name ++ "'"
    display OnChakra           = "Trigger: Gain, deplete, or absorb chakra"
    display OnDamage           = "Trigger: Deal damage"
    display (OnDamaged All)    = "Trigger: Receive damage"
    display (OnDamaged cla)    = "Trigger: Receive " ++ lower cla ++ " damage"
    display OnDeath            = "Trigger: Die"
    display OnHarm             = "Trigger: Use harmful skill"
    display (OnHarmed All)     = "Trigger: Be affected by a new harmful skill"
    display (OnHarmed cla)     = "Trigger: Be affected by a new " ++ lower cla ++ " harmful skill"
    display OnHealed           = "Trigger: Receive healing"
    display OnHelped           = "Trigger: Be affected by a new skill from an ally"
    display OnImmune           = "Trigger: Become invulnerable"
    display OnNoAction         = "Trigger: Do not use a new skill"
    display OnReflectAll       = "All skills are reflected."
    display OnRes              = "Trigger: Reach 0 health"
    display OnStun             = "Trigger: Apply a stun"
    display OnStunned          = "Trigger: Stunned"
    display PerDamage          = "Trigger: Deal damage"
    display PerDamaged         = "Trigger: Receive damage"
    display PerHealed          = "Trigger: Receive healing"

-- | A 'Skill' copied from a different character.
data Copy = Copy { skill :: Skill
                 , dur   :: Int
                 } deriving (Generic, ToJSON)

instance Classed Copy where
    classes = Classed.classes . (skill :: Copy -> Skill)

instance TurnBased Copy where
    getDur = dur
    setDur d Copy{skill} = Copy { skill = skill', dur = d }
      where
        skill' = case copying skill of
            Shallow b _ -> skill { copying = Shallow b d }
            Deep    b _ -> skill { copying = Deep    b d }
            NotCopied   -> skill

data Copying
    = Shallow Slot Int -- ^ No cooldown or chakra cost.
    | Deep Slot Int    -- ^ Cooldown and chakra cost.
    | NotCopied
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)

-- | Applies an effect after several turns.
data Delay = Delay { user   :: Slot
                   , skill  :: Skill
                   , effect :: Runnable Context
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
                     , classes :: EnumSet Class
                     , bombs   :: [Runnable Bomb]
                     , maxDur  :: Int
                     , dur     :: Int
                     } deriving (Generic, ToJSON)
instance Eq Status where
    (==) = (==) `on` \Status{..} -> (name, user, classes, maxDur, dur)
instance Ord Status where
    compare = comparing (name :: Status -> Text)
instance TurnBased Status where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Status where
    name   = name
    user = user
instance Classed Status where
    classes = classes

data Direction
    = Toward
    | From
    | Per
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

-- | A trap which gets triggered when a 'Ninja' meets the conditions of a 'Trigger'.
data Trap = Trap { direction :: Direction
                 , trigger   :: Trigger
                 , name      :: Text
                 , desc      :: Text
                 , user      :: Slot
                 , effect    :: Int -> Runnable Context
                 , classes   :: EnumSet Class
                 , tracker   :: Int
                 , dur       :: Int
                 }
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
    (==) = (==) `on` \Trap{..} -> (direction, trigger, name, user, classes, dur)
instance TurnBased Trap where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Trap where
    name = name
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
    modifyAll f = traverse_ (`modify` f) Slot.all

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
    {-# INLINE context #-}

instance MonadGame m => MonadPlay (ReaderT Context m) where
    context = ask
    {-# INLINE context #-}
    with    = local
    {-# INLINE with #-}

type RunConstraint a = ∀ m. (MonadRandom m, MonadPlay m) => m a

data Runnable a = To { target :: a
                     , run    :: RunConstraint ()
                     }
instance Show a => Show (Runnable a) where
    show = show . (target :: Runnable a -> a)
instance ToJSON a => ToJSON (Runnable a) where
    toJSON = toJSON . (target :: Runnable a -> a)

instance MonadGame m => MonadGame (ExceptT e m)
instance MonadGame m => MonadGame (IdentityT m)
instance MonadGame m => MonadGame (MaybeT m)
instance MonadGame m => MonadGame (SelectT r m)
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
instance MonadPlay m => MonadPlay (WebSocketsT m) where
    with f = mapReaderT $ with f
instance (MonadPlay m, Monoid w) => MonadPlay (WriterT w m) where
    with f = mapWriterT $ with f
instance (MonadPlay m, Monoid w) => MonadPlay (AccumT w m) where
    with f = mapAccumT $ with f
