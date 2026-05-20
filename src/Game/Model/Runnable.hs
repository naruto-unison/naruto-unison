module Game.Model.Runnable
  ( Runnable(..)
  , retarget
  , RunConstraint
  , IntRunConstraint
  ) where

import ClassyPrelude

import Game.Model.Internal (Runnable(..), RunConstraint, IntRunConstraint)

-- | Adjusts 'target'.
retarget :: (a -> b) -> Runnable a -> Runnable b
retarget f x = x { target = f $ target x }
