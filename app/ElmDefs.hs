{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-} -- Used once at the bottom of the file.
{-# OPTIONS_HADDOCK hide           #-}
module ElmDefs where
import           Elm.Derive

import ClassyPrelude.Yesod hiding (Status, Vector, get)
import           Control.Monad.Reader (local, mapReaderT)
import           Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import           Control.Monad.Trans.State.Strict (StateT, get, modify')
import qualified Data.Aeson as Aeson
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Vector.Generic (Vector)
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as Random
import           Text.Blaze (ToMarkup(..))
import           Yesod.WebSockets (WebSocketsT)

import           Core.Util (equaling)
import qualified Class.Classed as Classed
import           Class.Classed (Classed)
import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Labeled as Labeled
import           Class.Labeled (Labeled)
import           Class.TurnBased (TurnBased(..))
import           Model.Class (Class(..))
import           Model.Chakra (Chakras(..))
import           Model.Duration (Duration, sync, unsync)
import           Model.Face (Face(..))
import           Model.Player (Player)
import           Model.Slot (Slot)

opts :: Options
opts = defaultOptions { sumEncoding = Aeson.defaultTaggedObject }

data Effect = Effect
    { desc    :: Text
    , helpful :: Bool
    , sticky  :: Bool
    , trap    :: Bool
    }

data Ninja = Ninja
    { slot      :: Slot
    , health    :: Int
    , defense   :: [Defense]
    , barrier   :: [Barrier]
    , statuses  :: [Status]
    , charges   :: Seq Int
    , cooldowns :: Seq Int
    , variants  :: Seq (NonEmpty Variant)
    , copies    :: Seq (Maybe Copy)
    , channels  :: [Channel]
    , traps     :: Seq Trap
    , face      :: [Face]
    , parrying :: [Skill]
    , tags :: [ChannelTag]
    , lastSkill :: Maybe Skill
    , skills :: [Skill]
    }
instance Eq Ninja where
    (==) = equaling \Ninja{..} -> (slot, health, cooldowns, charges)
instance Parity Ninja where
    even = Parity.even . slot

data Game = Game { chakra  :: (Chakras, Chakras)
                 , ninjas  :: Seq Ninja
                 , playing :: Player
                 , victor  :: [Player]
                 , targets :: [[[Slot]]]
                 } deriving (Eq)

data Amount = Flat | Percent deriving (Eq, Ord)

data Context = Context { skill   :: Skill
                       , user    :: Slot
                       , target  :: Slot
                       } deriving (Eq)

type PlayConstraint a = ∀ m. (MonadRandom m, MonadPlay m) => m a
newtype Play a = Play (PlayConstraint a)
instance Eq (Play a) where
    (==) = const $ const True

type SavedPlay = (Context, Maybe Bool)

class Monad m => MonadRandom m where
    random  :: ∀ a. Random.Variate a => a -> a -> m a
    shuffle :: ∀ v a. Vector v a => v a -> m (v a)

class Monad m => MonadGame m where
    game        :: m Game
    modify      :: (Game -> Game) -> m ()

class MonadGame m => MonadPlay m where
    context :: m Context
    with    :: ∀ a. (Context -> Context) -> m a -> m a

instance MonadGame m => MonadGame (ReaderT Context m) where
    game   = lift game
    modify = lift . modify
instance MonadGame m => MonadPlay (ReaderT Context m) where
    context = ask
    with    = local

instance MonadRandom m => MonadRandom (MaybeT m) where
    random a = lift . random a
    shuffle  = lift . shuffle
instance MonadGame m => MonadGame (MaybeT m) where
    game    = lift game
    modify  = lift . modify
instance MonadPlay m => MonadPlay (MaybeT m) where
    context = lift context
    with f  = mapMaybeT $ with f
instance MonadRandom m => MonadRandom (ReaderT Context m) where
    random a = lift . random a
    shuffle  = lift . shuffle
instance MonadRandom m => MonadRandom (WebSocketsT m) where
    random a = lift . random a
    shuffle  = lift . shuffle
instance MonadGame m => MonadGame (WebSocketsT m) where
    game    = lift game
    modify  = lift . modify
instance MonadPlay m => MonadPlay (WebSocketsT m) where
    context = lift context
    with f  = mapReaderT $ with f

instance MonadGame (StateT Game Identity) where
    game   = get
    modify = modify'

data Wrapper = Wrapper { gameRef :: IORef Game
                       , rand    :: Random.GenIO
                       }

instance MonadIO m => MonadRandom (ReaderT Wrapper m) where
    random a b = asks (rand :: Wrapper -> Random.GenIO)
                 >>= liftIO . Random.uniformR (a, b)
    shuffle xs = asks (rand :: Wrapper -> Random.GenIO)
                 >>= liftIO . Random.uniformShuffle xs

instance MonadIO m => MonadGame (ReaderT Wrapper m) where
    game     = asks gameRef >>= readIORef
    modify f = asks gameRef >>= flip modifyIORef' f

data Flag
    = Acted
    | Harmed
    | Targeted
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Hashable Flag where
    hashWithSalt x = hashWithSalt x . fromEnum

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
data Skill = Skill { name      :: Text -- ^ Name
                   , desc      :: Text -- ^ Description
                   , require   :: Requirement   -- ^ Defaults to 'Usable'
                   , classes   :: [Class]       -- ^ Defaults to @[]@
                   , cost      :: Chakras       -- ^ Defaults to 'S.empty'
                   , cooldown  :: Duration      -- ^ Defaults to @0@
                   , varicd    :: Bool          -- ^ Defaults to @False@
                   , charges   :: Int           -- ^ Defaults to @0@
                   , channel   :: Channeling    -- ^ Defaults to 'Instant'
                   , start     :: [(Target, Maybe Bool)] -- ^ Defaults to @[]@
                   , effects   :: [(Target, Maybe Bool)] -- ^ Defaults to @[]@
                   , interrupt :: [(Target, Maybe Bool)] -- ^ Defaults to @[]@
                   , copying   :: Copying       -- ^ Defaults to 'NotCopied'
                   , pic       :: Bool          -- ^ Defaults to @False@
                   }
instance Eq Skill where
    (==) = equaling \Skill{..} -> (name, desc)
instance Classed Skill where
    classes = classes

-- | Destructible barrier.
data Barrier = Barrier { amount :: Int
                       , user   :: Slot
                       , name   :: Text
                       , dur    :: Int
                       } deriving (Eq)
instance TurnBased Barrier where
    getDur     = dur
    setDur d x = x { dur = d }
instance Ord Barrier where
    compare = comparing (name :: Barrier -> Text)
instance Labeled Barrier where
    name   = name
    user = user

-- | Destructible defense.
data Defense = Defense { amount :: Int
                       , user   :: Slot
                       , name   :: Text
                       , dur    :: Int
                       } deriving (Eq)
instance TurnBased Defense where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Defense where
    name   = name
    user = user

-- | An 'Act' channeled over multiple turns.
data Channel = Channel { source :: Slot
                       , skill  :: Skill
                       , target :: Slot
                       , dur    :: Channeling
                       } deriving (Eq)

instance Classed Channel where
    classes = Classed.classes . (skill :: Channel -> Skill)

instance TurnBased Channel where
    getDur     = getDur . (dur :: Channel -> Channeling)
    setDur d x = x { dur = setDur d $ (dur :: Channel -> Channeling) x }

-- | Types of channeling for 'Skill's.
data Channeling = Instant
                | Passive
                | Action  Duration
                | Control Duration
                | Ongoing Duration
                deriving (Eq, Show)
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

-- | Indicates that a channeled 'Skill' will affect a 'Ninja' next turn.
data ChannelTag = ChannelTag { source  :: Slot
                             , user    :: Slot
                             , skill   :: Skill
                             , ghost   :: Bool
                             , dur     :: Int
                             } deriving (Eq)

instance Classed ChannelTag where
    classes = Classed.classes . (skill :: ChannelTag -> Skill)

instance TurnBased ChannelTag where
    getDur     = dur
    setDur d x = x { dur = d }

instance Labeled ChannelTag where
    name = (name :: Skill -> Text) . (skill :: ChannelTag -> Skill)
    user = source

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
                           , category :: Category
                           }
instance Eq Character where
    (==) = equaling \Character{..} -> (name, category)

instance Show Character where
    show (Character name _ _ Original)   = unpack name
    show (Character name _ _ Shippuden)  = unpack name ++ " (S)"
    show (Character name _ _ Reanimated) = unpack name ++ " (R)"

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
    deriving (Eq, Show)

instance Classed Trigger where
    classes (OnAction cla)  = [cla]
    classes (OnCounter cla) = [cla]
    classes (OnDamaged cla) = [cla]
    classes (OnHarmed cla)  = [cla]
    classes _               = []

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
    deriving (Eq)

-- | Applies an effect after several turns.
data Delay = Delay { user   :: Slot
                   , skill  :: Skill
                   , effect :: SavedPlay
                   , dur    :: Int
                   } deriving (Eq)

instance Classed Delay where
    classes = Classed.classes . (skill :: Delay -> Skill)

instance TurnBased Delay where
    getDur     = dur
    setDur d x = x { dur = d }

instance Labeled Delay where
    name   = (name :: Skill -> Text) . (skill :: Delay -> Skill)
    user = user

-- | Applies 'Transform's when a 'Status' ends.
data Bomb
    = Done   -- ^ Applied with both 'Expire' and 'Remove'
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    | Remove -- ^ Applied when a 'Status' is removed prematurely
    deriving (Enum, Eq, Show)

-- | A status effect affecting a 'Ninja'.
data Status = Status { amount  :: Int  -- ^ Starts at 1
                     , name    :: Text -- ^ Label
                     , source  :: Slot -- ^ Owner of the 'Status.skill'
                     , user    :: Slot -- ^ User
                     , skill   :: Skill
                     , effects :: [Effect]
                     , classes :: [Class]
                     , bombs   :: [(Bomb, Maybe Bool)]
                     , maxDur  :: Int
                     , dur     :: Int
                     }
instance Eq Status where
    (==) = equaling \Status{..} -> (name, user, maxDur, classes)
instance TurnBased Status where
    getDur     = dur
    setDur d x = x { dur = d }
instance Ord Status where
    compare = comparing (name :: Status -> Text)
instance Labeled Status where
    name   = name
    user = user
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
                 , user      :: Slot
                 , classes   :: [Class]
                 , tracker   :: Int
                 , dur       :: Int
                 }
instance Eq Trap where
    (==) = equaling \Trap{..} -> (direction, trigger, name, user, dur)
instance TurnBased Trap where
    getDur     = dur
    setDur d x = x { dur = d }
instance Labeled Trap where
    name   = name
    user = user
instance Classed Trap where
    classes = classes

-- Black magic
instance Eq (a -> b) where
    (==) = const $ const True
