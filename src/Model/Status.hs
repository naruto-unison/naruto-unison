module Model.Status
  ( Status(..)
  , Bomb(..)
  , new, dead
  , remove, removeMatch
  , unfold
  ) where

import ClassyPrelude

import           Model.Internal (Bomb(..), Effect(..), Status(..))
import qualified Model.Copy as Copy
import           Model.Duration (Duration, incr, sync)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)

new :: Slot -> Duration -> Skill -> Status
new user dur skill =
    Status { amount  = 1
           , name    = Skill.name skill
           , source  = Copy.source skill user
           , user    = user
           , skill   = skill
           , effects = mempty
           , classes = Skill.classes skill
           , bombs   = []
           , maxDur  = dur'
           , dur     = dur'
           }
  where
    dur' = Copy.maxDur (Skill.copying skill) . incr $ sync dur

-- | Applies 'Plague' to prevent healing or resurrecting dead targets.
dead :: Slot -> Status
dead slot = Status { amount  = 1
                   , name    = "dead"
                   , source  = slot
                   , user    = slot
                   , skill   = Skill.new
                   , effects = singleton Plague
                   , classes = mempty
                   , bombs   = []
                   , maxDur  = -1
                   , dur     = -1
                   }

-- | Decreases 'amount' and removes the @Status@ if the amount reaches 0.
decr :: Int -> Status -> [Status] -> [Status]
decr i x xs
  | amount x > i = x { amount = amount x - i } : x `delete` xs
  | otherwise    = x `delete` xs

-- | Decreases 'amount' by 1 and removes the @Status@ if the amount reaches 0.
remove :: Status -> [Status] -> [Status]
remove = decr 1

-- | Decreases the 'amount' of a matching @Status@ by some number
-- | and removes the @Status@ if the amount reaches 0.
removeMatch :: Int -> (Status -> Bool) -> [Status] -> [Status]
removeMatch i predic xs = case find predic xs of
    Nothing -> xs
    Just x  -> decr i x xs

-- | Replicates 'effects' by 'amount' so that they can be summed.
unfold :: Status -> Status
unfold st = st { amount = 1, effects = effects st >>= replicate (amount st) }
