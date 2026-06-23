module Class.TurnBased
  ( TurnBased(..)
  , decrement
  , expiring
  , increment
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
    -- | Extends a duration.
    addDur :: Duration -> a -> a
    addDur dur x = setDur (getDur x + dur) x

-- | If @'getDur' == 'Permanent'@, has no effect.
-- If @'getDur' == Duration 1@, deletes the structure; it has expired.
-- Otherwise, decrements the remaining duration by 1.
decrement :: ∀ a. TurnBased a => a -> Maybe a
decrement x = case getDur x of
    Permanent  -> Just x
    Duration 0 -> Nothing
    Duration d -> Just $! setDur (Duration $ d - 1) x

-- | If @'getDur' == 'Permanent'@, has no effect.
-- Otherwise, increases the remaining duration by 1.
increment :: ∀ a. TurnBased a => a -> a
increment x = case getDur x of
    Permanent  -> x
    Duration d -> setDur (Duration $ d + 1) x

expiring :: ∀ a. TurnBased a => a -> Bool
expiring x = getDur x == 0

instance TurnBased Duration where
    getDur = id
    setDur = const

instance TurnBased Destructible where
    getDur Destructible{dur} = dur
    setDur d x = x { Destructible.dur = d }

instance TurnBased Channel where
    getDur Channel{dur} = getDur dur
    setDur d x = x { Channel.dur = setDur d x.dur }
    addDur d x = x { Channel.dur = addDur d x.dur }

instance TurnBased Channeling where
    getDur Instant     = 1
    getDur Passive     = Permanent
    getDur (Action d)  = d
    getDur (Control d) = d
    getDur (Ongoing d) = d

    setDur _ Instant     = Instant
    setDur _ Passive     = Passive
    setDur d (Action  _) = Action  d
    setDur d (Control _) = Control d
    setDur d (Ongoing _) = Ongoing d

    addDur _ Instant      = Instant
    addDur _ Passive      = Passive
    addDur d (Action  d') = Action  $ d + d'
    addDur d (Control d') = Control $ d + d'
    addDur d (Ongoing d') = Ongoing $ d + d'

instance TurnBased Copy where
    getDur Copy{dur} = dur
    setDur d x = x { Copy.dur = d }

instance TurnBased Status where
    getDur Status{dur} = dur
    setDur d x = x { Status.dur = d }

instance TurnBased Trap where
    getDur Trap{dur} = dur
    setDur d x = x { Trap.dur = d }
