module Game.Model.Context
  ( Context(..)
  , illegal
  , fromStatus
  , reflect
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import           Game.Model.Internal (Context(..), Status(Status))
import qualified Game.Model.Internal
import           Game.Model.Player (Player)
import qualified Game.Model.Internal.Skill as Skill

fromStatus :: Status -> Context
fromStatus Status{skill, user, classes} = Context
    { skill     = skill { Skill.classes = skill.classes ++ classes }
    , user
    , target    = user
    , new       = False
    , continues = False
    }

reflect :: Context -> Context
reflect context = context { target = context.user }

-- | A 'Player' attempts to control a 'Ninja' not on their team.
illegal :: Player -> Context -> Bool
illegal p Context{user} = not $ Parity.allied p user
