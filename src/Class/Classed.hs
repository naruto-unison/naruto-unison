module Class.Classed
  ( Classed(..)
  , nonStack, nonStackAll
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import           Game.Model.Class (Class(..))
import           Util ((∈), (∉))

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> EnumSet Class

isNonStack :: ∀ a. Classed a => a -> Bool
isNonStack x = Nonstacking ∈ classes x && Hidden ∉ classes x

-- | Conditionally adds an item to a list of items depending on its classes.
-- If it is classified as 'Nonstacking', it will remove older items with the
-- same name and user.
nonStack :: ∀ o. (IsSequence o, Labeled (Element o), Classed (Element o))
         => Element o -> o -> o
nonStack x xs
  | isNonStack x = x `cons` filter f xs
  | otherwise    = x `cons` xs
  where
    f y = not (Labeled.eq x y) || Nonstacking ∉ classes y

-- | Conditionally adds multiple items to a list of items depending on their
-- classes. If an item is classified as 'Nonstacking', it will remove older
-- items with the same name and user.
nonStackAll :: ∀ o. (IsSequence o, Labeled (Element o), Classed (Element o))
         => o -> o -> o
nonStackAll xs ys
  | any isNonStack xs = foldr nonStack ys xs
  | otherwise         = xs ++ ys
