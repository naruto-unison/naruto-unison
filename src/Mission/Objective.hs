module Mission.Objective
  ( Objective(..), ident
  , Span(..)
  , WinType(..)
  ) where

import ClassyPrelude

import Game.Model.Trigger (Trigger)
import Mission.Hooks.Action (ActionHook)
import Mission.Hooks.Chakra (ChakraHook)
import Mission.Hooks.Store (StoreHook)
import Mission.Hooks.Trap (TrapHook, TriggerHook)
import Mission.Hooks.Turn (TurnHook)

-- | How long an objective goes before being reset.
data Span
    = Moment -- ^ Resets at the end of an action.
    | Turn   -- ^ Resets at the end of a turn.
    | Match  -- ^ Resets at the end of a game.
    | Career -- ^ Never resets.
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Whether wins are cumulative or must be uninterrupted by losses or ties.
data WinType
    = WinConsecutive
    | WinTotal
    deriving (Bounded, Enum, Eq, Ord, Show)

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
ident :: Objective -> Maybe Text
ident (Win _ _)           = Nothing
ident (Consecutive c _)   = Just c
ident (HookAction c _ _)  = Just c
ident (HookChakra c _ _)  = Just c
ident (HookStore c _ _)   = Just c
ident (HookTrap c _ _)    = Just c
ident (HookTrigger c _ _) = Just c
ident (HookTurn c _)      = Just c
