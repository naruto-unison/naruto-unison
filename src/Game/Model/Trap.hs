module Game.Model.Trap
  ( Trap(..)
  , Direction(..)
  , context
  , isExpiring
  , uncopied
  ) where

import ClassyPrelude

import qualified Class.TurnBased as TurnBased
import qualified Game.Model.ID as ID
import           Game.Model.Internal (Context, Trap(..), Direction(..), Ninja(Ninja))
import qualified Game.Model.Internal
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Trigger (Trigger(..))

-- | True if the 'Trap' was caused by an original skill.
-- False if it was caused by a copied skill.
uncopied :: Trap -> Bool
uncopied Trap{user, skill} = user == skill.owner

isExpiring :: Ninja -> Trap -> Bool
isExpiring Ninja{health = 0} Trap{trigger = OnBreak _} = True
isExpiring Ninja{barrier, defense} Trap{trigger = OnBreak destrID} =
    hasExpiring barrier || hasExpiring defense
  where
    hasExpiring destructibles = not (null matching)
                                && all TurnBased.expiring matching
      where
        matching = filter ((== destrID) . ID.from) destructibles
isExpiring _ _ = False

context :: Trap -> Context
context Trap{effect} = Runnable.target $ effect 0
