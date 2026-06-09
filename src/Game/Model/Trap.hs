module Game.Model.Trap
  ( Trap(..)
  , Direction(..)
  , isExpiring
  , uncopied
  ) where

import ClassyPrelude

import           Class.TurnBased (TurnBased)
import qualified Class.TurnBased as TurnBased
import           Game.Model.ID (HasID, ID(ID))
import qualified Game.Model.ID as ID
import           Game.Model.Internal (Trap(..), Direction(..), Ninja(Ninja), Skill(Skill))
import qualified Game.Model.Internal
import           Game.Model.Trigger (Trigger(..))

-- | True if the 'Trap' was caused by an original skill.
-- False if it was caused by a copied skill.
uncopied :: Trap -> Bool
uncopied Trap{user, skill = Skill{owner}} = user == owner

isExpiringMatch :: ∀ a. (HasID a, TurnBased a) => ID -> a -> Bool
isExpiringMatch trapID a = TurnBased.expiring a && ID.from a == trapID

isExpiring :: Ninja -> Trap -> Bool
isExpiring Ninja{health = 0} Trap{trigger = OnBreak _} = True
isExpiring Ninja{barrier, defense} Trap{user, skill = Skill{owner}, trigger = OnBreak name} =
    any (isExpiringMatch ID { user, owner, name }) $ barrier ++ defense
isExpiring _ _ = False
