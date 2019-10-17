module Game.Model.Status
  ( Status(..)
  , Bomb(..)
  , new
  , remove, removeEffect
  ) where

import ClassyPrelude

import           Game.Model.Duration (Duration, incr, sync)
import           Game.Model.Effect (Effect)
import           Game.Model.Internal (Bomb(..), Status(..))
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)

new :: Slot -> Duration -> Skill -> Status
new user dur skill =
    Status { amount  = 1
           , name    = Skill.name skill
           , user
           , skill
           , effects = mempty
           , classes = Skill.classes skill
           , bombs   = []
           , maxDur  = incr $ sync dur
           , dur     = incr $ sync dur
           }

remove :: Int -- ^ 'amount'
       -> Text -- ^ 'name'
       -> Slot -- ^ 'user'
       -> [Status] -> [Status]
remove 0 _ _ xs = xs
remove _ _ _ [] = []
remove i name' user' (x:xs)
  | user x /= user' || name x /= name' = x : remove i name' user' xs
  | amt > i                            = x { amount = amt - i } : xs
  | otherwise                          = remove (i - amt) name' user' xs
  where
    amt = amount x

removeEffect :: Effect -> [Status] -> [Status]
removeEffect ef = mapMaybe f
  where
    f st
      | null before = Just st
      | null after  = Nothing
      | otherwise   = Just st { effects = after }
      where
        before = effects st
        after  = filter (/= ef) before
