module Mission.Goal
  ( Mission(..)
  , Goal(..), character
  , Objective(..), belongsTo
  , Span(..)
  , ActionHook, ChakraHook, StoreHook, TrapHook, TurnHook, Store
  ) where

import ClassyPrelude

import qualified Game.Characters as Characters
import           Game.Model.Chakra (Chakras)
import           Game.Model.Character (Character)
import           Game.Model.Ninja (Ninja)

type Store = IntSet

type ActionHook = Ninja -- ^ User.
                  -> Ninja -- ^ Target before action.
                  -> Ninja -- ^ Target after action.
                  -> Int

type ChakraHook = (Chakras, Chakras) -- ^ Chakra before action, user's first.
                  -> (Chakras, Chakras) -- ^ Chakra after action, user's first.
                  -> Int

type StoreHook = Ninja -- ^ User.
                 -> Ninja -- ^ Target before action.
                 -> Ninja -- ^ Target after action.
                 -> Store
                 -> (Store, Int)

type TrapHook = Ninja -- ^ Target after triggering trap.
                -> Store
                -> (Store, Int)

type TurnHook = Ninja -- User.
                -> Ninja -- ^ Target at beginning of turn.
                -> Ninja -- ^ Target at end of turn.
                -> Int

data Span
    = Moment
    | Turn
    | Match
    | Career
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Objective
    = Win [Text]
    | Consecutive Text [Text]
    | HookAction Text Text ActionHook
    | HookChakra Text Text ChakraHook
    | HookStore Text Text StoreHook
    | HookTrap Text Text TrapHook
    | HookTurn Text TurnHook

user :: Objective -> Maybe Text
user Win{}                 = Nothing
user (Consecutive name _)  = Just name
user (HookAction name _ _) = Just name
user (HookChakra name _ _) = Just name
user (HookStore name _ _)  = Just name
user (HookTrap name _ _)   = Just name
user (HookTurn name _)     = Just name

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
