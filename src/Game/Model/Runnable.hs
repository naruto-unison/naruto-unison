module Game.Model.Runnable
  ( Runnable(..)
  , retarget
  , RunConstraint
  ) where

import ClassyPrelude

import Game.Model.Internal (Runnable(..), RunConstraint)

retarget :: (a -> b) -> Runnable a -> Runnable b
retarget f x = x { target = f $ target x }
