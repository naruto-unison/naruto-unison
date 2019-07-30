module Class.Display (Display(..)) where

import ClassyPrelude

import Data.Text.Lazy.Builder.Int as IntBuilder

class Display a where
    display :: a -> TextBuilder

instance Display Text where
    display = toBuilder

instance Display Int where
    display = IntBuilder.decimal
