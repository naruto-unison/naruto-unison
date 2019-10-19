module Game.Model.Delay (Delay(..), new) where

import ClassyPrelude

import           Game.Model.Context (Context)
import           Game.Model.Duration (Duration, incr, sync)
import           Game.Model.Internal (Delay(..))
import           Game.Model.Runnable (RunConstraint, Runnable(To))
import qualified Game.Model.Runnable as Runnable

new :: Context -> Duration -> RunConstraint () -> Delay
new target dur run = Delay
    { effect = To { target, run }
    , dur    = incr $ sync dur
    }
