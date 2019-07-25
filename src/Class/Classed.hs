module Class.Classed
  ( Classed(..)
  , nonStack
  ) where

import ClassyPrelude

import           Core.Util ((∈))
import qualified Class.Labeled as Labeled
import           Class.Labeled (Labeled)
import           Model.Class (Class(..), ClassSet)

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> ClassSet
instance Classed Class where
    classes = singletonSet

-- | Conditionally adds an item to a list of items depending on its classes.
-- If the item is classified as 'Single', it will not be added if there is
-- another item with the same name and user.
-- If it is classified as 'Nonstacking', it will remove older items with the
-- same name and user.
nonStack :: ∀ a b. (Labeled a, Classed b) => b -> a -> [a] -> [a]
nonStack c x xs
  | Single ∈ classes c && any (Labeled.eq x) xs = xs
  | Nonstacking ∈ classes c = x : filter (not . Labeled.eq x) xs
  | otherwise               = x : xs
