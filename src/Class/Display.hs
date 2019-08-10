module Class.Display (Display(..)) where

import ClassyPrelude

import qualified Data.Text.Lazy.Builder.Int as IntBuilder

class Display a where
    display :: a -> TextBuilder
    display' :: a -> LText
    display' = builderToLazy . display
    {-# INLINE display' #-}

instance Display Text where
    display = toBuilder
    {-# INLINE display #-}
    display' = fromStrict
    {-# INLINE display' #-}

instance Display String where
    display = toBuilder
    {-# INLINE display #-}
    display' = pack
    {-# INLINE display' #-}

instance Display Int where
    display = IntBuilder.decimal
    {-# INLINE display #-}
