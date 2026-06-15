module Game.Model.Status
  ( Status(..)
  , Bomb(..)
  , new
  , addClasses
  , remove, removeEffect
  ) where

import ClassyPrelude

import           Data.Enum.Set (EnumSet)
import           Game.Model.Class (Class)
import           Game.Model.Duration (Duration)
import           Game.Model.Effect (Effect)
import           Game.Model.ID (ID)
import qualified Game.Model.ID as ID
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
    , bombs   =  mempty
    , maxDur  = succ dur
    , dur     = succ dur
    }

addClasses :: EnumSet Class -> Status -> Status
addClasses classes status@Status{classes = classes'} =
    status { classes = classes ++ classes' }

remove :: Int -- ^ 'amount'
       -> ID -- ^ 'name'
       -> [Status] -> [Status]
remove 0 _ xs = xs
remove _ _ [] = []
remove i statusID (x:xs)
  | ID.from x /= statusID = x : remove i statusID xs
  | amt > i               = x { amount = amt - i } : xs
  | otherwise             = remove (i - amt) statusID xs
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
