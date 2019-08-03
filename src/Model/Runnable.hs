module Model.Runnable
  ( Runnable(..)
  , retarget
  , RunConstraint
  ) where

import ClassyPrelude

import Model.Internal (Runnable(..), RunConstraint)

retarget :: (a -> a) -> Runnable a -> Runnable a
retarget f x = x { target = f $ target x }
