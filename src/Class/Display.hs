module Class.Display (Display(..)) where

import Prelude

import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder.Int as IntBuilder

-- | A class for types with textual descriptions.
-- Output is given as a @TextBuilder@ so that larger @Display@ instances may be
-- assembled out of smaller ones. For example:
--
-- > {-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}
-- > newtype Coord = Coord Int deriving (Display)
-- > data Point = Point { x :: Coord, y :: Coord }
-- >
-- > instance Display Point where
-- >     display Point{x, y} = "(" ++ display x ++ ", " ++ display y ++ ")"
class Display a where
    display :: a -> Builder
    -- | Extracts a lazy @Text@ from the value of @display@.
    -- The default implementation simply wraps around 'Builder.toLazyText',
    -- and instances are not expected to provide their own implementation.
    -- The option exists primarily to spare cases such as 'Text' and 'Lazy.Text'
    -- pointless round trips through 'Builder's.
    display' :: a -> Lazy.Text
    display' = Builder.toLazyText . display
    {-# INLINE display' #-}
    -- Well, it also exists to shorten
    -- import Class.Display (Display(..), display') to (Display(..))
    -- (Insert joke re: laziness)

instance Display Text where
    display = Builder.fromText
    {-# INLINE display #-}
    display' = Lazy.fromStrict
    {-# INLINE display' #-}

instance Display Lazy.Text where
    display = Builder.fromLazyText
    {-# INLINE display #-}
    display' = id
    {-# INLINE display' #-}

instance Display String where
    display = Builder.fromString
    {-# INLINE display #-}
    display' = Lazy.pack
    {-# INLINE display' #-}

instance Display Int where
    display = IntBuilder.decimal
    {-# INLINE display #-}
