module Game.Model.Trap
  ( Trap(..)
  , Direction(..)
  , uncopied
  ) where

import ClassyPrelude

import Game.Model.Internal (Trap(..), Direction(..), Skill(owner))

uncopied :: Trap -> Bool
uncopied x = user x == owner (skill x)
