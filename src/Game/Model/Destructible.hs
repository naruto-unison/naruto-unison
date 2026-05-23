module Game.Model.Destructible
  ( Destructible(..)
  , new
  , negate
  , setDur, setFinish, setWhile
  ) where

import ClassyPrelude hiding (negate)

import           Game.Model.Duration (Duration)
import           Game.Model.Internal (Context(Context), Destructible(..), Runnable(To), IntRunConstraint, RunConstraint)
import qualified Game.Model.Internal.Context as Context

new :: Context
    -> Duration
    -> Int -- ^ Initial amount.
    -> Destructible
new Context{skill, user} dur amount = Destructible
    { user
    , skill
    , finish = Nothing
    , while  = Nothing
    , amount
    , dur
    }

negate :: Destructible -> Destructible
negate b@Destructible{amount} = b { amount = -amount
                             , finish = Nothing
                             , while = Nothing
                             }

setDur :: Duration -> Destructible -> Destructible
setDur dur barrier = barrier { dur }

setFinish ::IntRunConstraint () -> Destructible -> Destructible
setFinish effect barrier@Destructible{amount}
  | amount <= 0 = barrier
  | otherwise   = barrier { finish = Just \i -> To () $ effect i }

setWhile :: RunConstraint () -> Destructible -> Destructible
setWhile effect barrier@Destructible{amount}
  | amount <= 0 = barrier
  | otherwise   = barrier { while = Just $ To () effect }

