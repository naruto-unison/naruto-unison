-- | Missions and their components.
module Mission.Goal
  ( Mission(..)
  , Goal(..), character
  , Objective(..), belongsTo
  , Span(..)
  , WinType(..)
  , ActionHook, ChakraHook, StoreHook, TrapHook, TriggerHook, TurnHook, Store
  ) where

import ClassyPrelude

import qualified Game.Characters as Characters
import           Game.Model.Chakra (Chakras)
import           Game.Model.Character (Character)
import           Game.Model.Ninja (Ninja)
import           Game.Model.Player (Player)
import           Game.Model.Slot (Slot)
import           Game.Model.Trigger (Trigger)

-- | Some mission objectives require a persistent object for tracking progress.
type Store = IntSet

-- | Used in 'HookAction'.
type ActionHook = Text -- ^ Skill name.
                  -> Ninja -- ^ User.
                  -> Ninja -- ^ Target before action.
                  -> Ninja -- ^ Target after action.
                  -> Int

-- | Used in 'HookChakra'.
type ChakraHook = (Chakras, Chakras) -- ^ Chakra before action, user's first.
                  -> (Chakras, Chakras) -- ^ Chakra after action, user's first.
                  -> Int

-- | Used in 'HookStore'.
type StoreHook = Text -- ^ Skill name.
                 -> Ninja -- ^ User.
                 -> Ninja -- ^ Target before action.
                 -> Ninja -- ^ Target after action.
                 -> Store
                 -> (Store, Int)

-- | Used in 'HookTrap'.
type TrapHook = Slot -- ^ User.
                -> Ninja -- ^ Target after triggering trap.
                -> Store
                -> (Store, Int)

-- | Used in 'HookTrigger'.
type TriggerHook = Ninja -- ^ User.
                   -> Bool

-- | Used in 'HookTurn'.
type TurnHook = Player -- ^ Whose turn it is.
                -> Ninja -- User.
                -> Ninja -- ^ Target at beginning of turn.
                -> Ninja -- ^ Target at end of turn.
                -> Store
                -> (Store, Int)

-- | How long an objective goes before being reset.
data Span
    = Moment -- ^ Resets at the end of an action.
    | Turn   -- ^ Resets at the end of a turn.
    | Match  -- ^ Resets at the end of a game.
    | Career -- ^ Never resets.
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

-- | Whether wins are cumulative or must be uninterrupted by losses or ties.
data WinType
    = WinConsecutive
    | WinTotal
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

-- | The core component of @Mission@s.
data Objective
    = Win WinType [Text]
    | Consecutive Text [Text]
    | HookAction Text Text ActionHook
    | HookChakra Text Text ChakraHook
    | HookStore Text Text StoreHook
    | HookTrap Text Text TrapHook
    | HookTrigger Text Trigger TriggerHook
    | HookTurn Text TurnHook

-- | Most 'Objective's are specific to a character.
user :: Objective -> Maybe Text
user Win{}                  = Nothing
user (Consecutive name _)   = Just name
user (HookAction name _ _)  = Just name
user (HookChakra name _ _)  = Just name
user (HookStore name _ _)   = Just name
user (HookTrap name _ _)    = Just name
user (HookTrigger name _ _) = Just name
user (HookTurn name _)      = Just name

-- | Schema component in use in @Mission.Missions@ modules.
data Goal = Reach { reach     :: Int
                  , spanning  :: Span
                  , desc      :: Text
                  , objective :: Objective
                  }

-- | Main schema in @Mission.Missions@ modules.
data Mission = Mission { char  :: Text
                       , goals :: Seq Goal
                       }

instance Eq Mission where
    (==) = (==) `on` char

-- | Uses 'user' to map a @Goal@ to the @Character@ that it hooks.
character :: Goal -> Maybe Character
character x = Characters.lookup =<< user (objective x)

-- | True if the @Goal@ belongs to a Character, as given by 'Character.ident'.
belongsTo :: Text -> Goal -> Bool
belongsTo name x = maybe False (== name) . user $ objective x
