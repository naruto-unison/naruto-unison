module Model.Slot
  ( Slot
  , all
  , team
  ) where

import Data.Array ((..), filter)
import Data.Int (even)

type Slot = Int

all :: Array Slot
all = 0 .. 5

team :: Array Slot
team = filter even all
