-- | Missions and their components.
module Mission.Goal
  ( Mission(..)
  , Goal(..), character
  , belongsTo
  ) where

import ClassyPrelude

import qualified Game.Characters as Characters
import           Game.Model.Character (Character)
import           Mission.Objective (Objective(..), Span(..))
import qualified Mission.Objective as Objective

-- | Schema component in use in @Mission.Missions@ modules.
data Goal = Reach
    { spanning  :: Span
    , reach     :: Int
    , desc      :: Text
    , objective :: Objective
    }

-- | Main schema in @Mission.Missions@ modules.
data Mission = Mission
    { char  :: Text
    , goals :: Seq Goal
    }

instance Eq Mission where
    (==) = (==) `on` char

-- | Uses 'user' to map a @Goal@ to the @Character@ that it hooks.
character :: Goal -> Maybe Character
character Reach{objective} = Characters.lookup =<< Objective.ident objective

-- | True if the @Goal@ belongs to a Character, as given by 'Character.ident'.
belongsTo :: Text -> Goal -> Bool
belongsTo ident Reach{objective} = case Objective.ident objective of
    Just ident' -> ident' == ident
    Nothing     -> False
