module Mission.Goal
  ( Mission(..)
  , Goal(..), character
  , Objective(..), belongsTo
  , Span(..)
  , HookFunc, TurnFunc, Store
  ) where

import ClassyPrelude

import           Game.Model.Character (Character)
import           Game.Model.Ninja (Ninja)
import qualified Game.Characters as Characters

type Store = IntSet
type HookFunc = Ninja -- ^ User.
                -> Ninja -- ^ Target before action.
                -> Ninja -- ^ Target after action.
                -> Store
                -> (Store, Int)

type TurnFunc = Ninja -- User.
                -> Ninja -- ^ Target at end of turn.
                -> Store
                -> (Store, Int)

data Span
    = Turn
    | Match
    | Career

data Objective
    = Win [Text]
    | Hook Text Text HookFunc
    | HookTurn Text TurnFunc
    | UseAllSkills Text

user :: Objective -> Maybe Text
user Win{}               = Nothing
user (Hook name _ _)     = Just name
user (HookTurn name _)   = Just name
user (UseAllSkills name) = Just name

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
character x = Characters.lookupName =<< user (objective x)

belongsTo :: Text -> Goal -> Bool
belongsTo name x = maybe False (name ==) . user $ objective x
