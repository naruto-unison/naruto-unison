module Game.Model.Status
  ( Status(..)
  , Bomb(..)
  , new
  , remove, removeEffect
  ) where

import ClassyPrelude

import qualified Class.Labeled as Labeled
import           Game.Model.Duration (Duration)
import           Game.Model.Effect (Effect)
import           Game.Model.Internal (Bomb(..), Skill(Skill), Status(..))
import qualified Game.Model.Internal
import           Game.Model.Slot (Slot)

new :: Slot -> Duration -> Skill -> Status
new user dur skill@Skill{classes, name} = Status
    { amount  = 1
    , name
    , user
    , skill
    , effects = mempty
    , classes
    , bombs   = []
    , maxDur  = succ dur
    , dur     = succ dur
    }

remove :: Int -- ^ 'amount'
       -> Text -- ^ 'name'
       -> Slot -- ^ 'user'
       -> [Status] -> [Status]
remove 0 _ _ xs = xs
remove _ _ _ [] = []
remove i name user (x:xs)
  | not $ Labeled.match name user x = x : remove i name user xs
  | amt > i                         = x { amount = amt - i } : xs
  | otherwise                       = remove (i - amt) name user xs
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
