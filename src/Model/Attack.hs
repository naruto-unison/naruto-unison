module Model.Attack
  ( Attack(..)
  ) where

import ClassyPrelude.Yesod

data Attack
    = Afflict
    | Pierce
    | Damage
    | Demolish
    deriving (Eq)
