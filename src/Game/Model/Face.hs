module Game.Model.Face
  ( Face(..)
  , new
  ) where

import ClassyPrelude

import           Game.Model.Internal (Face(..), Status(Status))
import qualified Game.Model.Internal

new :: Status -> Face
new Status{name, user} = Face
    { icon = (toLower name)
    , user
    }
