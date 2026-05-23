module Class.Classed
  ( Classed(..)
  , nonStack
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import           Game.Model.Class (Class(..))
import           Util ((∈))

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> EnumSet Class

-- | Conditionally adds an item to a list of items depending on its classes.
-- If it is classified as 'Nonstacking', it will remove older items with the
-- same name and user.
nonStack :: ∀ a. (Labeled a, Classed a) => a -> [a] -> [a]
nonStack x xs
  | Hidden ∈ classes x      = x : xs
  | Nonstacking ∈ classes x = x : filter (not . Labeled.eq x) xs
  | otherwise               = x : xs
