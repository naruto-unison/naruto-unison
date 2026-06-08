module Class.Classed (Classed(..)) where

import Data.Enum.Set (EnumSet)

import Game.Model.Class (Class(..))

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> EnumSet Class
