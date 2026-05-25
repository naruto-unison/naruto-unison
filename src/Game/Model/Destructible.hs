module Game.Model.Destructible
  ( Destructible(..)
  , negate
  ) where

import Game.Model.Internal (Destructible(..))

negate :: Destructible -> Destructible
negate b@Destructible{amount} = b { amount = -amount, effects = [] }
