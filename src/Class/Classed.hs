module Class.Classed
  ( Classed(..)
  , nonStack
  ) where

import ClassyPrelude

import Data.Enum.Set.Class (EnumSet)

import           Util ((∈))
import qualified Class.Labeled as Labeled
import           Class.Labeled (Labeled)
import           Game.Model.Class (Class(..))

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> EnumSet Class

-- | Conditionally adds an item to a list of items depending on its classes.
-- If it is classified as 'Nonstacking', it will remove older items with the
-- same name and user.
nonStack :: ∀ a b. (Labeled a, Classed b) => b -> a -> [a] -> [a]
nonStack c x xs
  | Hidden ∈ classes c      = x : xs
  | Nonstacking ∈ classes c = x : filter (not . Labeled.eq x) xs
  | otherwise               = x : xs
