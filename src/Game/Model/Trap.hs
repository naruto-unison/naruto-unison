module Game.Model.Trap
  ( Trap(..)
  , Direction(..)
  , context
  , isExpiring
  , uncopied
  ) where

import ClassyPrelude

import           Class.TurnBased (TurnBased)
import qualified Class.TurnBased as TurnBased
import           Game.Model.ID (HasID, ID)
import qualified Game.Model.ID as ID
import           Game.Model.Internal (Context, Trap(..), Direction(..), Ninja(Ninja))
import qualified Game.Model.Internal
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Trigger (Trigger(..))

-- | True if the 'Trap' was caused by an original skill.
-- False if it was caused by a copied skill.
uncopied :: Trap -> Bool
uncopied Trap{user, skill} = user == skill.owner

isExpiringMatch :: ∀ a. (HasID a, TurnBased a) => ID -> a -> Bool
isExpiringMatch triggerID a = TurnBased.expiring a && ID.from a == triggerID

isExpiring :: Ninja -> Trap -> Bool
isExpiring Ninja{health = 0} Trap{trigger = OnBreak _} = True
isExpiring Ninja{barrier, defense} Trap{trigger = OnBreak destrID} =
    any (isExpiringMatch destrID) barrier
    || any (isExpiringMatch destrID) defense
isExpiring _ _ = False

context :: Trap -> Context
context Trap{effect, tracker} = Runnable.target $ effect tracker
