module Class.Display (Display(..), display') where

import ClassyPrelude

import Data.Text.Lazy.Builder.Int as IntBuilder

class Display a where
    display :: a -> TextBuilder

instance Display Text where
    display = toBuilder
    {-# INLINE display #-}

instance Display String where
    display = toBuilder
    {-# INLINE display #-}

instance Display Int where
    display = IntBuilder.decimal
    {-# INLINE display #-}

display' :: ∀ a. Display a => a -> LText
display' = builderToLazy . display
{-# INLINE display' #-}
