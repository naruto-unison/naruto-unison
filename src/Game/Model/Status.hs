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
import           Game.Model.Internal (Bomb(..), Skill, Status(..))
import qualified Game.Model.Internal
import           Game.Model.Slot (Slot)

new :: Slot -> Duration -> Skill -> Status
new user dur skill = Status
    { amount  = 1
    , name    = skill.name
    , user
    , skill
    , effects = mempty
    , classes = skill.classes
    , bombs   =  mempty
    , dur
    }

addClasses :: EnumSet Class -> Status -> Status
addClasses classes status = status { classes = classes ++ status.classes }

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
      | null st.effects = Just st
      | null filtered   = Nothing
      | otherwise       = Just st { effects = filtered }
      where
        filtered  = filter (/= ef) st.effects
