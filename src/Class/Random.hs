-- | Monadic constraints for generating random data.
module Class.Random
  ( RandomT(..)
  , choose
  ) where

import ClassyPrelude.Yesod
import Data.List ((!!))

import Model.Internal (RandomT(..))

-- | Randomly selects an element from a list. 
-- Returns 'Nothing' on an empty list.
choose :: ∀ m a. RandomT m => [a] -> m (Maybe a)
choose [] = return Nothing
choose xs = Just . (xs !!) <$> random 0 (length xs - 1)
