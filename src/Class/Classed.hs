module Class.Classed (Classed(..)) where

import ClassyPrelude

import           Data.Enum.Set (EnumSet)
import qualified Data.Enum.Set.Base as Base

import Game.Model.Class (Class(..))

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> EnumSet Class

instance Classed (Base.EnumSet Word64 Class) where
    classes = id
    {-# INLINE classes #-}

instance Classed [Class] where
    classes = setFromList
    {-# INLINE classes #-}
