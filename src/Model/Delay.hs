module Model.Delay    (Delay(..), new) where

import ClassyPrelude

import           Model.Internal (Delay(..))
import qualified Model.Context as Context
import           Model.Context (Context)
import           Model.Duration (Duration, incr, sync)
import qualified Model.Runnable as Runnable
import           Model.Runnable (RunConstraint)

new :: Context -> Duration -> RunConstraint () -> Delay
new context dur f = Delay
    { skill  = Context.skill context
    , user   = Context.user context
    , effect = Runnable.To
        { Runnable.target = context
        , Runnable.run    = f
        }
    , dur    = incr $ sync dur
    }
