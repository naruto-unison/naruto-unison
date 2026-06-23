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

import           Class.Classed (Classed(..))
import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.ID (HasID, ID(ID))
import qualified Game.Model.ID
import           Game.Model.Internal.Game (Game)
import           Game.Model.Group (Group)
import           Game.Model.Slot (Slot(..))
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Negative, Trigger)
import           Util (Lift)

-- | Applies actions when a 'Status' ends.
data Bomb
    = Done   -- ^ Applied when a 'Status' is removed for any reason.
    | Expire -- ^ Applied when a 'Status' reaches the end of its duration.
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance ToJSON Bomb


-- | 'Original', 'Shippuden', or 'Reanimated'.
data Category
    = Original
    | Shippuden
    | Reanimated
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

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
    getClasses Channel{skill = Skill{classes}} = classes

instance HasID Channel where
    from Channel{skill = Skill{owner, name}} = ID { user = owner, owner, name }


-- | Types of channeling for 'Skill's.
data Channeling
    = Instant
    | Passive
    | Action  Duration
    | Control Duration
    | Ongoing Duration
    deriving (Eq, Ord, Show, Generic)

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
    , price    :: Int
    , bio      :: Text
    , groups   :: EnumSet Group
    , skills   :: NonNull Vector (NonNull Vector Skill)
    , category :: Category
    , ident    :: Text
    } deriving (Generic)

instance ToJSON Character

instance ToMarkup Character where
    toMarkup Character{name, category} = HTML.toMarkup name
                                         ++ HTML.toMarkup category

instance Show Character where
    showsPrec i Character{ident} = showsPrec i ident

instance Eq Character where
    (==) = (==) `on` \Character{category, name} -> (category, name)

instance Ord Character where
    compare = comparing \Character{category, name} -> (category, name)


-- | A 'Skill' copied from a different character.
data Copy = Copy
    { skill :: Skill
    , dur   :: Duration
    } deriving (Generic)

instance ToJSON Copy

instance Classed Copy where
    getClasses (Copy Skill{classes} _) = classes

instance HasID Copy where
    from (Copy Skill{name, owner} _) = ID { user = owner, owner, name }


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
    getClasses Destructible{skill = Skill{classes}} = classes

instance HasID Destructible where
    from Destructible{user, skill = Skill{name, owner}} = ID { user
                                                             , owner
                                                             , name
                                                             }


data Direction
    = Toward
    | From
    | Per
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance ToJSON Direction


-- | From 'Effect.Face'. Used only as an encoding intermediary.
data Face = Face
    { icon :: Text
    , user :: Slot
    } deriving (Eq, Show, Generic)

instance ToJSON Face


-- Used for 'Game.Ninja.cooldowns' and 'Game.Ninja.charges'.
-- Generated from a 'Skill'.
data Key = Key Text Slot deriving (Eq, Ord, Show, Generic)

instance Hashable Key

toText :: Key -> Text
toText (Key x y) = Slot.toChar y :< x
{-# INLINABLE toText #-}

instance ToJSON Key where
    toJSON = toJSON . toText

instance ToJSONKey Key where
    toJSONKey = toJSONKeyText toText


-- | In-game character, indexed between 0 and 5.
data Ninja = Ninja
    { slot       :: Slot                -- ^ 'Model.Game.Ninjas' index (0-5)
    , character  :: Character
    , health     :: Int                 -- ^ Starts at @100@
    , cooldowns  :: HashMap Key Int     -- ^ Starts empty
    , charges    :: HashMap Key Int     -- ^ Starts at @0@s
    , copies     :: Vector (Maybe Copy) -- ^ Starts at @Nothing@s
    , defense    :: [Destructible]      -- ^ Starts empty
    , barrier    :: [Destructible]      -- ^ Starts empty
    , statuses   :: [Status]            -- ^ Starts empty
    , channels   :: [Channel]           -- ^ Starts empty
    , traps      :: [Trap]              -- ^ Starts empty
    , onTurn     :: Bool                -- ^ Starts false
    , lastSkill  :: Maybe Skill         -- ^ Starts at @Nothing@
    , triggers   :: HashSet Trigger     -- ^ Empty at the start of each action
    , negatives  :: EnumSet Negative    -- ^ Empty at the start of each turn
    , skills     :: ~(Vector Skill)     -- ^ Processed automatically
    , effects    :: ~[Effect]           -- ^ Processed automatically
    , face       :: ~(Maybe Face)       -- ^ Processed automatically
    }

instance ToJSON Ninja where
    toJSON Ninja
        { barrier
        , channels
        , character = Character{ident}
        , charges
        , cooldowns
        , copies
        , defense
        , health
        , face
        , skills
        , slot
        , statuses
        , traps
        } = object
        [ "slot"      .= slot
        , "character" .= ident
        , "health"    .= health
        , "cooldowns" .= cooldowns
        , "charges"   .= charges
        , "defense"   .= defense
        , "barrier"   .= barrier
        , "statuses"  .= statuses
        , "copies"    .= copies
        , "channels"  .= channels
        , "traps"     .= traps
        , "face"      .= face
        , "skills"    .= skills
        ]


instance Parity Ninja where
    even = Parity.even . slot
    {-# INLINE even #-}


data Range
    = AtLeast
    | AtMost
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance ToJSON Range


data Requirement
    = Unusable
    | UserHas Range Int Text
    | TargetHas Range Int Text
    | TargetHasFromAny Range Int Text
    | UserHealth Range Int
    | TargetHealth Range Int
    | UserChannel Bool Text
    | UserDefense Range Int Text
    | UserTrap Bool Text
    | TargetCategory Bool Category
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Requirement


-- | A move that a 'Character' can perform.
data Skill = Skill
    { name      :: Text              -- ^ Name
    , desc      :: Text              -- ^ Description
    , require   :: [Requirement]     -- ^ Defaults to empty
    , classes   :: EnumSet Class     -- ^ Defaults to empty
    , cost      :: Chakras           -- ^ Defaults to empty
    , cooldown  :: Int               -- ^ Defaults to @0@
    , charges   :: Int               -- ^ Defaults to @0@
    , dur       :: Channeling        -- ^ Defaults to 'Instant'
    , start     :: [Runnable Target] -- ^ Defaults to empty
    , always    :: [Runnable Target] -- ^ Defaults to empty
    , stunned   :: [Runnable Target] -- ^ Defaults to empty
    , effects   :: [Runnable Target] -- ^ Defaults to empty
    , end       :: [Runnable Target] -- ^ Defaults to empty
    , changes   :: Ninja -> Skill -> Skill -- ^ Defaults to 'id'
    , owner     :: Slot
    }

instance ToJSON Skill where
    toJSON Skill
        { name
        , desc
        , classes
        , cost
        , cooldown
        , charges
        , dur
        , start
        , always
        , stunned
        , effects
        , end
        , owner
        } = object
        [ "name"     .= name
        , "desc"     .= desc
        , "classes"  .= classes
        , "cost"     .= cost
        , "cooldown" .= cooldown
        , "charges"  .= charges
        , "dur"      .= dur
        , "start"    .= start
        , "always"   .= always
        , "stunned"  .= stunned
        , "effects"  .= effects
        , "end"      .= end
        , "owner"    .= owner
        ]

instance Classed Skill where
    getClasses Skill{classes} = classes

instance HasID Skill where
    from Skill{owner, name} = ID { user = owner, owner, name }


-- | A status effect affecting a 'Ninja'.
data Status = Status
    { amount  :: Int  -- ^ Starts at 1
    , name    :: Text -- ^ Label
    , user    :: Slot -- ^ User
    , skill   :: Skill
    , effects :: [Effect]
    , classes :: EnumSet Class
    , bombs   :: [Runnable Bomb]
    , dur     :: Duration
    } deriving (Generic)

instance ToJSON Status

instance Classed Status where
    getClasses Status{classes} = classes

instance HasID Status where
    from Status{name, user, skill = Skill{owner}} = ID { user, owner, name }


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
    deriving (Bounded, Enum, Eq, Ord, Show, Generic)

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
        , dur
        } = object
        [ "direction" .= direction
        , "trigger"   .= trigger
        , "name"      .= name
        , "skill"     .= skill
        , "user"      .= user
        , "classes"   .= classes
        , "dur"       .= dur
        ]

instance Classed Trap where
    getClasses Trap{classes} = classes

instance HasID Trap where
    from Trap{user, name, skill = Skill{owner}} = ID { user, owner, name }


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
    getClasses Context{skill = Skill{classes}} = classes

instance HasID Context where
    from Context{user, skill = Skill{owner, name}} = ID { user, owner, name }


instance MonadRandom m => MonadRandom (ReaderT Context m)

-- | Basic game-handling. @MonadGame@ provides functionality for querying and
-- modifying 'Game' state and 'Ninja's.
class Monad m => MonadGame m where
    game      :: m Game
    alterGame :: (Game -> Game) -> m ()
    ninjas    :: m (Vector Ninja)
    ninja     :: Slot -> m Ninja
    write     :: Slot -> Ninja -> m ()
    modify    :: Slot -> (Ninja -> Ninja) -> m ()
    modifyAll :: (Ninja -> Ninja) -> m ()

    default game :: Lift MonadGame m
                 => m Game
    game = lift game
    {-# INLINE game #-}
    default alterGame :: Lift MonadGame m
                      => (Game -> Game) -> m ()
    alterGame = lift . alterGame
    {-# INLINE alterGame #-}
    default ninjas :: Lift MonadGame m
                   => m (Vector Ninja)
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
class (MonadGame m, MonadRandom m) => MonadPlay m where
    context :: m Context
    with    :: ∀ a. (Context -> Context) -> m a -> m a

    default context :: Lift MonadPlay m
                    => m Context
    context = lift context
    {-# INLINE context #-}

instance (MonadGame m, MonadRandom m) => MonadPlay (ReaderT Context m) where
    context = ask
    {-# INLINE context #-}
    with    = local
    {-# INLINE with #-}

-- | Impredicatively-typed monad constraint.
type RunConstraint a = ∀ m. MonadPlay m => m a

-- | 'RunConstraint' with an argument.
type IntRunConstraint a = ∀ m. MonadPlay m => Int -> m a

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
