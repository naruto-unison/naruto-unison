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

type Store = IntSet

type ActionHook = Text -- ^ Skill name.
                  -> Ninja -- ^ User.
                  -> Ninja -- ^ Target before action.
                  -> Ninja -- ^ Target after action.
                  -> Int

type ChakraHook = (Chakras, Chakras) -- ^ Chakra before action, user's first.
                  -> (Chakras, Chakras) -- ^ Chakra after action, user's first.
                  -> Int

type StoreHook = Text -- ^ Skill name.
                 -> Ninja -- ^ User.
                 -> Ninja -- ^ Target before action.
                 -> Ninja -- ^ Target after action.
                 -> Store
                 -> (Store, Int)

type TrapHook = Slot -- ^ User.
                -> Ninja -- ^ Target after triggering trap.
                -> Store
                -> (Store, Int)

type TriggerHook = Ninja -- ^ User.
                   -> Bool

type TurnHook = Player -- ^ Whose turn it is.
                -> Ninja -- User.
                -> Ninja -- ^ Target at beginning of turn.
                -> Ninja -- ^ Target at end of turn.
                -> Store
                -> (Store, Int)

data Span
    = Moment
    | Turn
    | Match
    | Career
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data WinType
    = WinConsecutive
    | WinTotal
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Objective
    = Win WinType [Text]
    | Consecutive Text [Text]
    | HookAction Text Text ActionHook
    | HookChakra Text Text ChakraHook
    | HookStore Text Text StoreHook
    | HookTrap Text Text TrapHook
    | HookTrigger Text Trigger TriggerHook
    | HookTurn Text TurnHook

user :: Objective -> Maybe Text
user Win{}                  = Nothing
user (Consecutive name _)   = Just name
user (HookAction name _ _)  = Just name
user (HookChakra name _ _)  = Just name
user (HookStore name _ _)   = Just name
user (HookTrap name _ _)    = Just name
user (HookTrigger name _ _) = Just name
user (HookTurn name _)      = Just name

data Goal = Reach { reach     :: Int
                  , spanning  :: Span
                  , desc      :: Text
                  , objective :: Objective
                  }

data Mission = Mission { char  :: Text
                       , goals :: Seq Goal
                       }

instance Eq Mission where
    (==) = (==) `on` char

character :: Goal -> Maybe Character
character x = Characters.lookup =<< user (objective x)

belongsTo :: Text -> Goal -> Bool
belongsTo name x = maybe False (name ==) . user $ objective x
