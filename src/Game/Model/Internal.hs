{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Game.Model.Internal where

import ClassyPrelude

import qualified Text.Blaze.Html5 as HTML

import Control.Monad.Reader (local, mapReaderT)
import Control.Monad.Trans.Accum (AccumT, mapAccumT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Identity (IdentityT, mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Select (SelectT, mapSelectT)
import Control.Monad.Trans.Writer (WriterT, mapWriterT)
import Data.Aeson.Types ((.=), ToJSON(..), ToJSONKey(..), object, toJSONKeyText)
import Data.Enum.Set (AsEnumSet(..), EnumSet)
import Text.Blaze (ToMarkup(..))
import Yesod.Core.Dispatch (PathPiece(..))
import Yesod.WebSockets (WebSocketsT)

import           Class.Classed (Classed)
import qualified Class.Classed as Classed
import qualified Class.Labeled
import           Class.Labeled (Labeled)
import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Class.TurnBased (TurnBased(..))
import           Game.Model.Chakra (Chakras(..))
import           Game.Model.Class (Class(..))
import           Game.Model.Defense (Defense(..))
import           Game.Model.Duration (Duration, sync, unsync)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game)
import           Game.Model.Group (Group)
import           Game.Model.Slot (Slot(..))
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger(..))
import           Util (Lift)

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

-- | Applies actions when a 'Status' ends.
data Bomb
    = Done   -- ^ Applied with both 'Expire' and 'Remove'
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    | Remove -- ^ Applied when a 'Status' is removed prematurely
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

-- | 'Original', 'Shippuden', or 'Reanimated'.
data Category
    = Original
    | Shippuden
    | Reanimated
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

instance ToMarkup Category where
    toMarkup Original   = mempty
    toMarkup Shippuden  = HTML.sup "𝕊"
    toMarkup Reanimated = HTML.sup "ℝ"

instance PathPiece Category where
    toPathPiece Original   = "original"
    toPathPiece Shippuden  = "shippuden"
    toPathPiece Reanimated = "reanimated"
    fromPathPiece "original"   = Just Original
    fromPathPiece "shippuden"  = Just Shippuden
    fromPathPiece "reanimated" = Just Reanimated
    fromPathPiece _            = Nothing

-- | An 'Model.Act.Act' channeled over multiple turns.
data Channel = Channel { skill  :: Skill
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

instance ToMarkup Channeling where
    toMarkup Instant     = "Instant"
    toMarkup Passive     = "Instant"
    toMarkup (Action 0)  = "Action"
    toMarkup (Control 0) = "Control"
    toMarkup (Ongoing 0) = "Ongoing"
    toMarkup (Action x)  = "Action " ++ toMarkup x
    toMarkup (Control x) = "Control " ++ toMarkup x
    toMarkup (Ongoing x) = "Ongoing " ++ toMarkup x

-- | An out-of-game character.
data Character = Character { name     :: Text
                           , bio      :: Text
                           , groups   :: EnumSet Group
                           , skills   :: NonEmpty (NonEmpty Skill)
                           , price    :: Int
                           , category :: Category
                           } deriving (Generic, ToJSON)

instance Eq Character where
    (==) = (==) `on` \Character{..} -> (name, category)

-- | A 'Skill' copied from a different character.
data Copy = Copy { skill :: Skill
                 , dur   :: Int
                 } deriving (Generic, ToJSON)

instance Classed Copy where
    classes = Classed.classes . (skill :: Copy -> Skill)

instance TurnBased Copy where
    getDur = dur
    setDur d x = x { dur = d }

-- | Applies an effect after several turns.
data Delay = Delay { effect :: Runnable Context
                   , dur    :: Int
                   }

instance Classed Delay where
    classes = Classed.classes . (skill :: Context -> Skill) .
              (target :: Runnable Context -> Context) .
              (effect :: Delay -> Runnable Context)

instance TurnBased Delay where
    getDur     = dur
    setDur d x = x { dur = d }

data Direction
    = Toward
    | From
    | Per
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

 -- Used for 'Game.Ninja.cooldowns' and 'Game.Ninja.charges'.
 -- Generated from a 'Skill'.
data Key = Key Text Slot
           deriving (Eq, Ord, Show, Read, Generic, Hashable)
toText :: Key -> Text
toText (Key x y) = Slot.toChar y `cons` x
{-# INLINE toText #-}
instance ToJSON Key where
  toJSON = toJSON . toText
  {-# INLINE toJSON #-}
instance ToJSONKey Key where
  toJSONKey = toJSONKeyText toText
  {-# INLINE toJSONKey #-}

-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja { slot       :: Slot             -- ^ 'Model.Game.Ninjas' index (0-5)
                   , character  :: Character
                   , health     :: Int              -- ^ Starts at @100@
                   , cooldowns  :: HashMap Key Int  -- ^ Starts empty
                   , charges    :: HashMap Key Int  -- ^ Starts at @0@s
                   , alternates :: Seq Int          -- ^ Starts at @0@s
                   , copies     :: Seq (Maybe Copy) -- ^ Starts at @Nothing@s
                   , defense    :: [Defense]        -- ^ Starts empty
                   , barrier    :: [Barrier]        -- ^ Starts empty
                   , statuses   :: [Status]         -- ^ Starts empty
                   , channels   :: [Channel]        -- ^ Starts empty
                   , newChans   :: [Channel]        -- ^ Starts empty
                   , traps      :: [Trap]           -- ^ Starts empty
                   , delays     :: [Delay]          -- ^ Starts empty
                   , lastSkill  :: Maybe Skill      -- ^ Starts at @Nothing@
                   , triggers   :: HashSet Trigger  -- ^ Empty at the start of each turn
                   , effects    :: ~[Effect]        -- ^ Processed automatically
                   , acted      :: Bool             -- ^ False at the start of each turn
                   }
instance Parity Ninja where
    even = Parity.even . slot
    {-# INLINE even #-}

data Requirement
    = Usable
    | Unusable
    | HasI Int Text
    | HasU Int Text
    | HealthI Int
    | HealthU Int
    | DefenseI Int Text
    deriving (Eq, Ord, Show, Read, Generic, ToJSON)

-- | A move that a 'Character' can perform.
data Skill = Skill { name      :: Text              -- ^ Name
                   , desc      :: Text              -- ^ Description
                   , require   :: Requirement       -- ^ Defaults to 'Usable'
                   , classes   :: EnumSet Class     -- ^ Defaults to empty
                   , cost      :: Chakras           -- ^ Defaults to empty
                   , cooldown  :: Duration          -- ^ Defaults to @0@
                   , charges   :: Int               -- ^ Defaults to @0@
                   , dur       :: Channeling        -- ^ Defaults to 'Instant'
                   , start     :: [Runnable Target] -- ^ Defaults to empty
                   , effects   :: [Runnable Target] -- ^ Defaults to empty
                   , stunned   :: [Runnable Target] -- ^ Defaults to empty
                   , interrupt :: [Runnable Target] -- ^ Defaults to empty
                   , changes   :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
                   , owner     :: Slot
                   }
instance ToJSON Skill where
    toJSON Skill{..} = object
        [ "name"      .= name
        , "desc"      .= desc
        , "require"   .= require
        , "classes"   .= classes
        , "cost"      .= cost
        , "cooldown"  .= cooldown
        , "charges"   .= charges
        , "dur"       .= dur
        , "start"     .= start
        , "effects"   .= effects
        , "stunned"   .= stunned
        , "interrupt" .= interrupt
        , "owner"     .= owner
        ]
instance Classed Skill where
    classes = classes

-- | A status effect affecting a 'Ninja'.
data Status = Status { amount  :: Int  -- ^ Starts at 1
                     , name    :: Text -- ^ Label
                     , user    :: Slot -- ^ User
                     , skill   :: Skill
                     , effects :: [Effect]
                     , classes :: EnumSet Class
                     , bombs   :: [Runnable Bomb]
                     , maxDur  :: Int
                     , dur     :: Int
                     } deriving (Generic, ToJSON)
instance Eq Status where
    (==) = (==) `on` \Status{..} -> (name, user, classes, dur)
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

-- | Target destinations of 'Skill's.
data Target
    = Self    -- ^ User of 'Skill'
    | Ally    -- ^ Specific ally
    | Allies  -- ^ All allies
    | XAlly   -- ^ Specific ally excluding 'Self'
    | XAllies -- ^ 'Allies' excluding 'Self'
    | RAlly   -- ^ Random ally
    | RXAlly -- ^ Random ally excluding 'Self'
    | Enemy   -- ^ Specific enemy
    | Enemies -- ^ All enemies
    | REnemy  -- ^ Random enemy
    | XEnemies -- ^ Enemies excluding 'Enemy'
    | Everyone -- ^ All 'Ninja's
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)

instance AsEnumSet Target

-- | A trap which gets triggered when a 'Ninja' meets the conditions of a 'Trigger'.
data Trap = Trap { direction :: Direction
                 , trigger   :: Trigger
                 , name      :: Text
                 , skill     :: Skill
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
        , "skill"     .= skill
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


-- | Gameplay context. This promotes a 'MonadGame' to 'MonadPlay'.
data Context = Context { skill     :: Skill
                       , user      :: Slot
                       , target    :: Slot
                       , new       :: Bool
                       , continues :: Bool -- ^ Cosmetic: continuous effect.
                       } deriving (Generic, ToJSON)

instance MonadRandom m => MonadRandom (ReaderT Context m)

-- | Basic game-handling. @MonadGame@ provides functionality for querying and
-- modifying 'Game' state and 'Ninja's.
class Monad m => MonadGame m where
    game      :: m Game
    alter     :: (Game -> Game) -> m ()
    ninjas    :: m [Ninja]
    ninja     :: Slot -> m Ninja
    write     :: Slot -> Ninja -> m ()
    modify    :: Slot -> (Ninja -> Ninja) -> m ()
    modifyAll :: (Ninja -> Ninja) -> m ()
    modifyAll f = traverse_ (`modify` f) Slot.all

    default game :: Lift MonadGame m
                 => m Game
    game = lift game
    {-# INLINE game #-}
    default alter :: Lift MonadGame m
                  => (Game -> Game) -> m ()
    alter = lift . alter
    {-# INLINE alter #-}
    default ninjas :: Lift MonadGame m
                   => m [Ninja]
    ninjas = lift ninjas
    {-# INLINE ninjas #-}
    default ninja :: Lift MonadGame m
                  => Slot -> m Ninja
    ninja = lift . ninja
    {-# INLINE ninja #-}
    default write :: Lift MonadGame m
                  => Slot -> Ninja -> m ()
    write i = lift . write i
    {-# INLINE write #-}
    default modify :: Lift MonadGame m
                   => Slot -> (Ninja -> Ninja) -> m ()
    modify i = lift . modify i
    {-# INLINE modify #-}

-- | The main typeclass of the game engine. @MonadPlay@ is built on top of
-- @MonadGame@, but it also provides a "view" into the game: a @Context@ that
-- defines which skill is being used, who is using it, and who they are using it
-- on. This context changes frequently, and temporary contexts may even be
-- pushed and popped (e.g. if a skill is reflected), but the underlying
-- @MonadGame@ instance stays the same.
class MonadGame m => MonadPlay m where
    context :: m Context
    with    :: ∀ a. (Context -> Context) -> m a -> m a

    default context :: Lift MonadPlay m
                    => m Context
    context = lift context
    {-# INLINE context #-}

instance MonadGame m => MonadPlay (ReaderT Context m) where
    context = ask
    {-# INLINE context #-}
    with    = local
    {-# INLINE with #-}

-- | Impredicatively-typed monad constraint.
type RunConstraint a = ∀ m. (MonadRandom m, MonadPlay m) => m a

-- | Hides 'RunConstraint' behind a constructor so that only RankNTypes is
-- needed.
data Runnable a = To { target :: a
                     , run    :: RunConstraint ()
                     }
instance Show a => Show (Runnable a) where
    showsPrec i = showsPrec i . (target :: Runnable a -> a)
instance ToJSON a => ToJSON (Runnable a) where
    toJSON = toJSON . (target :: Runnable a -> a)
    {-# INLINE toJSON #-}

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
