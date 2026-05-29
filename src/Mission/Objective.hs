module Mission.Objective
  ( Objective(..), ident
  , Span(..)
  , WinType(..)
  , ActionHook, ChakraHook, StoreHook, TrapHook, TriggerHook, TurnHook, Store
  ) where

import ClassyPrelude

import Game.Model.Chakras (Chakras)
import Game.Model.Ninja (Ninja)
import Game.Model.Player (Player)
import Game.Model.Slot (Slot)
import Game.Model.Trigger (Trigger)

-- | Some mission objectives require a persistent object for tracking progress.
type Store = IntSet

-- | Used in 'HookAction'.
type ActionHook = Text  -- ^ Skill name.
               -> Ninja -- ^ User.
               -> Ninja -- ^ Target before action.
               -> Ninja -- ^ Target after action.
               -> Int

-- | Used in 'HookChakra'.
type ChakraHook = (Chakras, Chakras) -- ^ Chakra before action, user's first.
               -> (Chakras, Chakras) -- ^ Chakra after action, user's first.
               -> Int

-- | Used in 'HookStore'.
type StoreHook = Text  -- ^ Skill name.
              -> Ninja -- ^ User.
              -> Ninja -- ^ Target before action.
              -> Ninja -- ^ Target after action.
              -> Store
              -> (Store, Int)

-- | Used in 'HookTrap'.
type TrapHook = Slot  -- ^ User.
             -> Ninja -- ^ Target after triggering trap.
             -> Store
             -> (Store, Int)

-- | Used in 'HookTrigger'.
type TriggerHook = Ninja -- ^ User.
                -> Bool

-- | Used in 'HookTurn'.
type TurnHook = Player -- ^ Whose turn it is.
             -> Ninja  -- User.
             -> Ninja  -- ^ Target at beginning of turn.
             -> Ninja  -- ^ Target at end of turn.
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
ident :: Objective -> Maybe Text
ident (Win _ _)           = Nothing
ident (Consecutive c _)   = Just c
ident (HookAction c _ _)  = Just c
ident (HookChakra c _ _)  = Just c
ident (HookStore c _ _)   = Just c
ident (HookTrap c _ _)    = Just c
ident (HookTrigger c _ _) = Just c
ident (HookTurn c _)      = Just c
