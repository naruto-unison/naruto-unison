module Mission.Goal
  ( Mission(..)
  , Goal(..)
  , Objective(..), involves
  , Span(..)
  , HookFunc, Store
  ) where

import ClassyPrelude

import Util ((∈))
import Game.Model.Ninja (Ninja)

type Store = IntSet
type HookFunc = Ninja -- ^ User.
                -> Ninja -- ^ Target before action.
                -> Ninja -- ^ Target after action.
                -> Store
                -> (Store, Int)

data Span
    = Turn
    | Match
    | Career

data Objective
    = Win [Text]
    | Hook Text Text HookFunc
    | HookTurn Text (Ninja -> Store -> (Store, Int))
    | UseAllSkills Text

involves :: Text -> Objective -> Bool
involves name (Win names) = name ∈ names
involves name (Hook name' _ _) = name == name'
involves name (HookTurn name' _) = name == name'
involves name (UseAllSkills name') = name == name'

data Goal = Reach { reach     :: Int
                  , spanning  :: Span
                  , desc      :: Text
                  , objective :: Objective
                  }

data Mission = Mission { character :: Text
                       , goals     :: Seq Goal
                       }

instance Eq Mission where
    (==) = (==) `on` character
