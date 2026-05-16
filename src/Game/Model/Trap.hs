module Game.Model.Trap
  ( Trap(..)
  , Direction(..)
  , uncopied
  ) where

import ClassyPrelude

import Game.Model.Internal (Trap(..), Direction(..), Skill(owner))

-- | True if the 'Trap' was caused by an original skill.
-- False if it was caused by a copied skill.
uncopied :: Trap -> Bool
uncopied x = user x == owner (skill x)
