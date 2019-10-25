module Game.Model.Context
  ( Context(..)
  , fromStatus
  , reflect
  ) where

import ClassyPrelude

import           Game.Model.Internal (Context(..))
import           Game.Model.Status (Status)
import qualified Game.Model.Status as Status

fromStatus :: Status -> Context
fromStatus st = Context { skill     = Status.skill st
                        , user      = Status.user st
                        , target    = Status.user st
                        , new       = False
                        , continues = False
                        }

reflect :: Context -> Context
reflect context = context { target = user context }
