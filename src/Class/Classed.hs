module Class.Classed
  ( Classed(..)
  , nonStack
  ) where

import ClassyPrelude.Yesod

import           Core.Util ((∈))
import qualified Class.Labeled as Labeled
import           Class.Labeled (Labeled)
import           Model.Class (Class(..))

class Classed a where
    classes :: a -> [Class]

nonStack :: ∀ a b. (Labeled a, Classed b) => b -> a -> [a] -> [a]
nonStack c x xs
  | Single ∈ classes c && any (Labeled.eq x) xs = xs
  | Nonstacking ∈ classes c = x : filter (not . Labeled.eq x) xs
  | otherwise               = x : xs
