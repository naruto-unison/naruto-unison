module Model.Status
  ( Status(..)
  , Bomb(..)
  , new
  , remove, removeMatch
  ) where

import ClassyPrelude

import           Model.Internal (Bomb(..), Status(..))
import qualified Model.Copy as Copy
import           Model.Duration (Duration, incr, sync)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)

new :: Slot -> Duration -> Skill -> Status
new user dur skill =
    Status { amount  = 1
           , name    = Skill.name skill
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
