module Game.Model.Delay (Delay(..), new) where

import ClassyPrelude

import           Game.Model.Duration (Duration)
import           Game.Model.Internal (Context, Delay(..), RunConstraint, Runnable(To))
import qualified Game.Model.Internal

new :: Context -> Duration -> RunConstraint () -> Delay
new target dur run = Delay
    { effect = To { target, run }
    , dur    = succ dur
    }
