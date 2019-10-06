module Game.Model.Delay (Delay(..), new) where

import ClassyPrelude

import           Game.Model.Context (Context)
import           Game.Model.Duration (Duration, incr, sync)
import           Game.Model.Internal (Delay(..))
import           Game.Model.Runnable (RunConstraint)
import qualified Game.Model.Runnable as Runnable

new :: Context -> Duration -> RunConstraint () -> Delay
new context dur f = Delay
    { effect = Runnable.To
        { Runnable.target = context
        , Runnable.run    = f
        }
    , dur    = incr $ sync dur
    }
