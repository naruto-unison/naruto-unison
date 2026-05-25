module Game.Model.Trap
  ( Trap(..)
  , Direction(..)
  , isExpiring
  , uncopied
  ) where

import ClassyPrelude

import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import           Class.TurnBased (TurnBased)
import qualified Class.TurnBased as TurnBased
import           Game.Model.Internal (Trap(..), Direction(..), Ninja(Ninja), Skill(owner))
import qualified Game.Model.Internal
import           Game.Model.Slot (Slot)
import           Game.Model.Trigger (Trigger(..))

-- | True if the 'Trap' was caused by an original skill.
-- False if it was caused by a copied skill.
uncopied :: Trap -> Bool
uncopied Trap{skill, user} = owner skill == user

isExpiringMatch :: ∀ a. (Labeled a, TurnBased a) => Text -> Slot -> a -> Bool
isExpiringMatch name user a = TurnBased.expiring a && Labeled.match name user a

isExpiring :: Ninja -> Trap -> Bool
isExpiring Ninja{health = 0} Trap{trigger = OnBreak _} = True
isExpiring Ninja{barrier, defense} Trap{user, trigger = OnBreak name} =
    any (isExpiringMatch name user) $ barrier ++ defense
isExpiring _ _ = False
