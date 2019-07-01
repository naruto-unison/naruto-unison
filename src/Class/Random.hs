-- | Monadic constraints for generating random data.
module Class.Random
  ( MonadRandom(..)
  , choose
  , enum
  ) where

import ClassyPrelude

import Model.Internal (MonadRandom(..))

-- | Randomly selects an element from a list.
-- Returns 'Nothing' on an empty list.
choose :: ∀ m a. MonadRandom m => [a] -> m (Maybe a)
choose [] = return Nothing
choose xs = Just . (xs `unsafeIndex`) <$> random 0 (length xs - 1)

-- | Randomly selects an element of an enumerated type.
enum :: ∀ m a. (MonadRandom m, Bounded a, Enum a) => m a
enum = toEnum <$> random (fromEnum (minBound :: a)) (fromEnum (maxBound :: a))
