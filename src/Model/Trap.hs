module Model.Trap
  ( Trap(..)
  , Trigger(..), isCounter
  , Direction(..)
  , Transform
  ) where

import ClassyPrelude

import Model.Internal (Trap(..), Trigger(..), Direction(..), Game)
import Model.Slot (Slot)

-- | The type signature of 'Trap' actions.
type Transform = Int  -- ^ Amount (optional argument for traps).
                 -> Slot -- ^ 'Game.ninjas' index of the user. Used as an optional argument by trap effects.
                 -> Game -- ^ Before.
                 -> Game -- ^ After.

isCounter :: Trigger -> Bool
isCounter Counter{}    = True
isCounter CounterAll{} = True
isCounter Countered{}  = True
isCounter _            = False
