module Model.Trap
  ( Trap(..)
  , Trigger(..), isCounter
  , Direction(..)
  ) where

import ClassyPrelude

import Model.Internal (Trap(..), Trigger(..), Direction(..))

isCounter :: Trigger -> Bool
isCounter Counter{}    = True
isCounter CounterAll{} = True
isCounter Countered{}  = True
isCounter _            = False
