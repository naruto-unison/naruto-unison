module Class.TurnBased
  ( TurnBased(..)
  , decr
  , expiring
  ) where

import ClassyPrelude

import           Game.Model.Duration (Duration(..))
import           Game.Model.Internal (Channeling(..), Channel(Channel), Copy(Copy), Destructible(Destructible), Status(Status), Trap(Trap))
import qualified Game.Model.Internal
import qualified Game.Model.Internal.Channel as Channel
import qualified Game.Model.Internal.Copy as Copy
import qualified Game.Model.Internal.Destructible as Destructible
import qualified Game.Model.Internal.Status as Status
import qualified Game.Model.Internal.Trap as Trap

-- | Typeclass for structures that expire after a set number of turns.
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

-- | If @'getDur' == 'Permanent'@, has no effect.
-- If @'getDur' == Duration 1@, deletes the structure; it has expired.
-- Otherwise, decrements the remaining duration by 1.
decr :: ∀ a. TurnBased a => a -> Maybe a
decr x
  | dur < 1     = Nothing
  | otherwise   = Just $ setDur (pred dur) x -- @pred Permanent == Permanent@
  where
    dur = getDur x

expiring :: ∀ a. TurnBased a => a -> Bool
expiring x = getDur x < 1

instance TurnBased Destructible where
    getDur Destructible{dur} = dur
    setDur d x = x { Destructible.dur = d }

instance TurnBased Channel where
    getDur (Channel _ _ _ dur) = getDur dur
    setDur d x = x { Channel.dur = setDur d $ Channel.dur x }

instance TurnBased Channeling where
    getDur Instant     = 1
    getDur Passive     = Permanent
    getDur (Action d)  = d
    getDur (Control d) = d
    getDur (Ongoing d) = d
    setDur _ Instant     = Instant
    setDur _ Passive     = Passive
    setDur d (Action _)  = Action d
    setDur d (Control _) = Control d
    setDur d (Ongoing _) = Ongoing d

instance TurnBased Copy where
    getDur Copy{dur} = dur
    setDur d x = x { Copy.dur = d }

instance TurnBased Status where
    getDur Status{dur} = dur
    setDur d x = x { Status.dur = d }

instance TurnBased Trap where
    getDur Trap{dur} = dur
    setDur d x = x { Trap.dur = d }
