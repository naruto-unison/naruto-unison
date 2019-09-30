module Mission.Goal
  ( Mission(..)
  , Goal(..), character
  , Objective(..), involves
  , Span(..)
  , HookFunc, Store
  ) where

import ClassyPrelude

import           Util ((∈))
import           Game.Model.Character (Character)
import           Game.Model.Ninja (Ninja)
import qualified Game.Characters as Characters

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

data Mission = Mission { char  :: Text
                       , goals :: Seq Goal
                       }

instance Eq Mission where
    (==) = (==) `on` char

character :: Goal -> Maybe Character
character x = case objective x of
    Win _               -> Nothing
    (Hook name _ _)     -> Characters.lookupName name
    (HookTurn name _)   -> Characters.lookupName name
    (UseAllSkills name) -> Characters.lookupName name
