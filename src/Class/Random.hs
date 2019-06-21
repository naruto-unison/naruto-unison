-- | Monadic constraints for generating random data.
module Class.Random
  ( MonadRandom(..)
  , choose
  ) where

import ClassyPrelude
import Data.List ((!!))

import Model.Internal (MonadRandom(..))

-- | Randomly selects an element from a list.
-- Returns 'Nothing' on an empty list.
choose :: ∀ m a. MonadRandom m => [a] -> m (Maybe a)
choose [] = return Nothing
choose xs = Just . (xs !!) <$> random 0 (length xs - 1)
