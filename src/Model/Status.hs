module Model.Status
  ( Status(..)
  , Bomb(..)
  , new, dead
  , unfold
  , remove, removeMatch
  ) where

import ClassyPrelude.Yesod hiding (Status, delete)
import qualified Data.List as List

import           Core.Util (incr, sync)
import           Model.Internal (Bomb(..), Effect(..), Status(..))
import qualified Model.Copy as Copy
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)

new :: Slot -> Int -> Skill -> Status
new source dur skill =
    Status { amount  = 1
           , name    = Skill.name skill
           , root    = Copy.root skill source
           , source  = source
           , user    = source
           , skill   = skill
           , effects = []
           , classes = Skill.classes skill
           , bombs   = []
           , maxDur  = dur'
           , dur     = dur'
           }
  where
    dur' = Copy.maxDur (Skill.copying skill) . incr $ sync dur

dead :: Slot -> Status
dead slot = Status { amount  = 1
                   , name    = "dead"
                   , root    = slot
                   , source  = slot
                   , user    = slot
                   , skill   = Skill.new
                   , effects = [Plague]
                   , classes = []
                   , bombs   = []
                   , maxDur  = -1
                   , dur     = -1
                   }

-- | Replicates all 'statusEfs' of a 'Status' by its 'statusCount'.
unfold :: Status -> Status
unfold st = st { effects = effects st >>= replicate (amount st) }

-- | Decreases 'amount' and removes the 'Status' if the amount reaches 0.
decr :: Int -> Status -> [Status] -> [Status]
decr i x xs
  | amount x > i = x { amount = amount x - i } : List.delete x xs
  | otherwise  = List.delete x xs

-- | Decreases 'amount' by 1 and removes the 'Status' if the amount reaches 0.
remove :: Status -> [Status] -> [Status]
remove = decr 1

-- | Decreases the 'amount' of a matching 'Status' by some number
-- | and removes the Status if the amount reaches 0.
removeMatch :: Int -> (Status -> Bool) -> [Status] -> [Status]
removeMatch i predic xs = case find predic xs of
    Nothing -> xs
    Just x  -> decr i x xs
