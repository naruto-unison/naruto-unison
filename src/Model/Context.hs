module Model.Context
  ( Context(..)
  , fromStatus
  , reflect
  ) where

import           Model.Internal (Context(..))
import           Model.Status (Status)
import qualified Model.Status as Status

fromStatus :: Status -> Context
fromStatus st = Context { skill  = Status.skill st
                        , source = Status.source st
                        , user   = Status.source st
                        , target = Status.source st
                        }

reflect :: Context -> Context
reflect ctx = ctx { user   = target ctx
                  , target = user ctx
                  }
