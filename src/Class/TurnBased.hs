module Class.TurnBased
  ( TurnBased(..)
  , decr
  ) where

import ClassyPrelude

-- | Typeclass for structures that expire after a set number of turns.
class TurnBased a where
    -- | Number of turns before expiration. If <= 0, never expires.
    getDur :: a -> Int
    -- | Updates the remaining number of turns after a turn has passed.
    setDur :: Int -> a -> a

-- If @'getDur' <= 0, has no effect.
-- If @'getDur' == 1, deletes the structure; it has expired.
-- Otherwise, decreases the remaining duration by 1.
decr :: ∀ a. TurnBased a => a -> Maybe a
decr a
  | getDur a == 0 = Just a
  | getDur a <= 1 = Nothing
  | otherwise     = Just $ setDur (getDur a - 1) a
