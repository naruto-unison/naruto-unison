module Class.TurnBased
  ( TurnBased(..)
  , decr
  ) where

import ClassyPrelude

import           Game.Model.Duration (Duration(..))
import           Game.Model.Defense (Defense)
import qualified Game.Model.Defense as Defense
import           Game.Model.Internal (Barrier, Channeling(..), Channel, Copy, Delay, Status, Trap)
import qualified Game.Model.Internal.Barrier as Barrier
import qualified Game.Model.Internal.Channel as Channel
import qualified Game.Model.Internal.Copy as Copy
import qualified Game.Model.Internal.Delay as Delay
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
decr x = case getDur x of
    Duration 1 -> Nothing
    dur        -> Just $ setDur (pred dur) x -- @Pred Permanent == Permanent@

instance TurnBased Barrier where
    getDur = Barrier.dur
    setDur d x = x { Barrier.dur = d }

instance TurnBased Channel where
    getDur     = getDur . Channel.dur
    setDur d x = x { Channel.dur = setDur d $ Channel.dur x }

instance TurnBased Channeling where
    getDur Instant     = 1
    getDur (Action d)  = d
    getDur (Control d) = d
    getDur (Ongoing d) = d
    setDur _ Instant     = Instant
    setDur d (Action _)  = Action d
    setDur d (Control _) = Control d
    setDur d (Ongoing _) = Ongoing d

instance TurnBased Copy where
    getDur = Copy.dur
    setDur d x = x { Copy.dur = d }

instance TurnBased Delay where
    getDur = Delay.dur
    setDur d x = x { Delay.dur = d }

instance TurnBased Defense where
    getDur     = Defense.dur
    setDur d x = x { Defense.dur = d }

instance TurnBased Status where
    getDur = Status.dur
    setDur d x = x { Status.dur = d }

instance TurnBased Trap where
    getDur = Trap.dur
    setDur d x = x { Trap.dur = d }
