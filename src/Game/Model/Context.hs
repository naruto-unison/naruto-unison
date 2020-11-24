module Game.Model.Context
  ( Context(..)
  , illegal
  , fromStatus
  , reflect
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import           Game.Model.Internal (Context(..))
import           Game.Model.Player (Player)
import qualified Game.Model.Skill as Skill
import           Game.Model.Status (Status(Status))
import qualified Game.Model.Status as Status

fromStatus :: Status -> Context
fromStatus Status{skill, user, classes} = Context
    { skill     = skill { Skill.classes = Skill.classes skill ++ classes }
    , user
    , target    = user
    , new       = False
    , continues = False
    }

reflect :: Context -> Context
reflect context = context { target = user context }

-- | A 'Player' attempts to control a 'Ninja' not on their team.
illegal :: Player -> Context -> Bool
illegal p a = not . Parity.allied p $ user a
