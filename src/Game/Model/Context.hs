module Game.Model.Context
  ( Context(..)
  , fromStatus
  , reflect
  ) where

import ClassyPrelude

import           Game.Model.Internal (Context(..))
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
