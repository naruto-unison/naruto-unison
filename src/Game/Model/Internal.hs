{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import qualified Class.Classed
import           Class.Labeled (Labeled)
import qualified Class.Labeled
import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Chakras (Chakras(..))
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Game (Game)
import           Game.Model.Group (Group)
import           Game.Model.Slot (Slot(..))
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger(..))
import           Util (Lift)

-- | Applies actions when a 'Status' ends.
data Bomb
    = Done   -- ^ Applied with both 'Expire' and 'Remove'
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    | Remove -- ^ Applied when a 'Status' is removed prematurely.
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance ToJSON Bomb


-- | 'Original', 'Shippuden', or 'Reanimated'.
data Category
    = Original
    | Shippuden
    | Reanimated
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance ToJSON Category

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
data Channel = Channel
    { skill  :: Skill
    , target :: Slot
    , new    :: Bool
    , dur    :: Channeling
    } deriving (Generic)

instance ToJSON Channel

instance Classed Channel where
    classes (Channel Skill{classes} _ _ _) = classes

instance Labeled Channel where
    name (Channel Skill{name} _ _ _)  = name
    user (Channel Skill{owner} _ _ _) = owner


-- | Types of channeling for 'Skill's.
data Channeling
    = Instant
    | Passive
    | Action  Duration
    | Control Duration
    | Ongoing Duration
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Channeling

instance ToMarkup Channeling where
    toMarkup (Action Permanent)  = "Action"
    toMarkup (Control Permanent) = "Control"
    toMarkup (Ongoing Permanent) = "Ongoing"
    toMarkup Instant     = "Instant"
    toMarkup Passive     = "Instant"
    toMarkup (Action x)  = "Action " ++ toMarkup x
    toMarkup (Control x) = "Control " ++ toMarkup x
    toMarkup (Ongoing x) = "Ongoing " ++ toMarkup x


-- | An out-of-game character.
data Character = Character
    { name     :: Text
    , bio      :: Text
    , groups   :: EnumSet Group
    , skills   :: NonEmpty (NonEmpty Skill)
    , price    :: Int
    , category :: Category
    } deriving (Generic)

instance ToJSON Character

instance Eq Character where
    (==) = (==) `on` \Character{name, category} -> (name, category)

instance Ord Character where
    compare = comparing \Character{category, name} -> (category, name)


-- | A 'Skill' copied from a different character.
data Copy = Copy
    { skill :: Skill
    , dur   :: Duration
    } deriving (Generic)

instance ToJSON Copy

instance Classed Copy where
    classes (Copy Skill{classes} _) = classes

instance Labeled Copy where
    name (Copy Skill{name} _)  = name
    user (Copy Skill{owner} _) = owner


-- | Destructible barrier or defense.
data Destructible = Destructible
    { amount  :: Int
    , user    :: Slot
    , skill   :: Skill
    , dur     :: Duration
    , effects :: [Effect]
    } deriving (Generic)

instance ToJSON Destructible

instance Classed Destructible where
    classes Destructible{skill = Skill{classes}} = classes

instance Labeled Destructible where
    name Destructible{skill = Skill{name}} = name
    user Destructible{user}                = user


data Direction
    = Toward
    | From
    | Per
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance ToJSON Direction


 -- Used for 'Game.Ninja.cooldowns' and 'Game.Ninja.charges'.
 -- Generated from a 'Skill'.
data Key = Key Text Slot
           deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Key

toText :: Key -> Text
toText (Key x y) = Slot.toChar y `cons` x
{-# INLINABLE toText #-}

instance ToJSON Key where
    toJSON = toJSON . toText

instance ToJSONKey Key where
    toJSONKey = toJSONKeyText toText


-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja
    { slot       :: Slot             -- ^ 'Model.Game.Ninjas' index (0-5)
    , character  :: Character
    , health     :: Int              -- ^ Starts at @100@
    , cooldowns  :: HashMap Key Int  -- ^ Starts empty
    , charges    :: HashMap Key Int  -- ^ Starts at @0@s
    , alternates :: Seq Int          -- ^ Starts at @0@s
    , copies     :: Seq (Maybe Copy) -- ^ Starts at @Nothing@s
    , defense    :: [Destructible]   -- ^ Starts empty
    , barrier    :: [Destructible]   -- ^ Starts empty
    , statuses   :: [Status]         -- ^ Starts empty
    , channels   :: [Channel]        -- ^ Starts empty
    , traps      :: [Trap]           -- ^ Starts empty
    , lastSkill  :: Maybe Skill      -- ^ Starts at @Nothing@
    , triggers   :: HashSet Trigger  -- ^ Empty at the start of each turn
    , effects    :: ~[Effect]        -- ^ Processed automatically
    , acted      :: Bool             -- ^ False at the start of each turn
    }

instance Parity Ninja where
    even = Parity.even . slot
    {-# INLINE even #-}

instance Labeled Ninja where
    name Ninja{character = Character{name}} = name
    user Ninja{slot}                        = slot


data Requirement
    = Usable
    | Unusable
    | UserHas Int Text
    | TargetHas Int Text
    | UserHealth Int
    | TargetHealth Int
    | UserDefense Int Text
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Requirement


-- | A move that a 'Character' can perform.
data Skill = Skill
    { name      :: Text              -- ^ Name
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
    , end       :: [Runnable Target] -- ^ Defaults to empty
    , changes   :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
    , owner     :: Slot
    }

instance ToJSON Skill where
    toJSON Skill
        { name
        , desc
        , require
        , classes
        , cost
        , cooldown
        , charges
        , dur
        , start
        , effects
        , stunned
        , end
        , owner
        } = object
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
        , "end"       .= end
        , "owner"     .= owner
        ]

instance Classed Skill where
    classes Skill{classes} = classes

instance Labeled Skill where
    name Skill{name}  = name
    user Skill{owner} = owner


-- | A status effect affecting a 'Ninja'.
data Status = Status
    { amount  :: Int  -- ^ Starts at 1
    , name    :: Text -- ^ Label
    , user    :: Slot -- ^ User
    , skill   :: Skill
    , effects :: [Effect]
    , classes :: EnumSet Class
    , bombs   :: [Runnable Bomb]
    , maxDur  :: Duration
    , dur     :: Duration
    } deriving (Generic)

instance ToJSON Status

instance Eq Status where
    (==) = (==) `on` \Status{name, user, classes, dur} ->
        (name, user, classes, dur)

instance Ord Status where
    compare = comparing \Status{name, user, classes, dur} ->
        (name, user, classes, dur)

instance Classed Status where
    classes Status{classes} = classes

instance Labeled Status where
    name Status{name} = name
    user Status{user} = user


-- | Target destinations of 'Skill's.
data Target
    = Self     -- ^ User of 'Skill'
    | Ally     -- ^ Specific ally
    | Allies   -- ^ All allies
    | XAlly    -- ^ Specific ally excluding 'Self'
    | XAllies  -- ^ 'Allies' excluding 'Self'
    | RAlly    -- ^ Random ally
    | RXAlly   -- ^ Random ally excluding 'Self'
    | Enemy    -- ^ Specific enemy
    | Enemies  -- ^ All enemies
    | REnemy   -- ^ Random enemy
    | XEnemies -- ^ Enemies excluding 'Enemy'
    | Everyone -- ^ All 'Ninja's
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

instance ToJSON Target

instance AsEnumSet Target


-- | A trap which gets triggered when a 'Ninja' meets the conditions of a 'Trigger'.
data Trap = Trap
    { direction :: Direction
    , trigger   :: Trigger
    , name      :: Text
    , skill     :: Skill
    , user      :: Slot
    , effect    :: Int -> Runnable Context
    , classes   :: EnumSet Class
    , tracker   :: Int
    , dur       :: Duration
    }

instance ToJSON Trap where
    toJSON Trap
        { direction
        , trigger
        , name
        , skill
        , user
        , classes
        , tracker
        , dur
        } = object
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
    (==) = (==) `on` \Trap{direction, trigger, name, user, classes, dur} ->
        (direction, trigger, name, user, classes, dur)

instance Classed Trap where
    classes Trap{classes} = classes

instance Labeled Trap where
    name Trap{name} = name
    user Trap{user} = user


-- | Gameplay context. This promotes a 'MonadGame' to 'MonadPlay'.
data Context = Context
    { skill     :: Skill
      -- ^ @Skill@ used to perform an action.
    , user      :: Slot
      -- ^ User of the action.
    , target    :: Slot
      -- ^ Target of the action. When an action affects  multiple 'Ninja's, the
      -- @target@ field is the only part of the 'Context' that changes.
    , new       :: Bool
      -- ^ When new actions are used, they can trigger traps and counters.
      -- All other actions, such as channeled actions past the first turn,
      -- delays, and effects of traps, cannot.
    , continues :: Bool
      -- ^ Cosmetic: continuous effect.
    } deriving (Generic)

instance ToJSON Context

instance Classed Context where
    classes Context{skill = Skill{classes}} = classes

instance Labeled Context where
    name Context{skill = Skill{name}} = name
    user Context{user}                = user


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
    default modifyAll :: Lift MonadGame m
                      => (Ninja -> Ninja) -> m ()
    modifyAll = lift . modifyAll
    {-# INLINE modifyAll #-}

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

-- | 'RunConstraint' with an argument.
type IntRunConstraint a = ∀ m. (MonadRandom m, MonadPlay m) => Int -> m a

-- | Hides 'RunConstraint' behind a constructor so that only RankNTypes is
-- needed.
data Runnable a = To
    { target :: a
    , run    :: RunConstraint ()
    }
instance Show a => Show (Runnable a) where
    showsPrec i (To target _) = showsPrec i target
instance ToJSON a => ToJSON (Runnable a) where
    toJSON (To target _) = toJSON target

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
