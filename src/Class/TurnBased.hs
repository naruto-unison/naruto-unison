module Class.TurnBased
  ( TurnBased(..)
  , decr
  ) where

import ClassyPrelude

import Game.Model.Duration (Duration(..))

-- | Typeclass for structures that expire after a set number of turns.k
--
-- Instances should obey the following laws:
--
-- * @getDur (setDur x) y == x@
-- * @setDur (getDur x) x == x@
class TurnBased a where
    -- | Number of turns before expiration. If @<= 0@, never expires.
    getDur :: a -> Duration
    -- | Updates the remaining number of turns after a turn has passed.
    setDur :: Duration -> a -> a

-- | If @'getDur' == Duration 0@, has no effect.
-- If @'getDur' == Duration 1@, deletes the structure; it has expired.
-- Otherwise, decrements the remaining duration by 1.
decr :: ∀ a. TurnBased a => a -> Maybe a
decr x = case sync $ getDur x of
    0 -> Just x
    1 -> Nothing
    d -> Just $ setDur (Duration $ d - 1) x
