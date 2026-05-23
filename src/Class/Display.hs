module Class.Display
  ( Display(..)
  , commas
  , shorten, unaccent
  ) where

import ClassyPrelude hiding (Builder)

import           Data.Attoparsec.Text (notInClass)
import qualified Data.Text.Lazy as Lazy
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
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
--
-- Instances should satisfy the following law:
--
-- @display' x == 'Builder.toLazyText' (display x)
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

instance Display Int64 where
    display = IntBuilder.decimal
    {-# INLINE display #-}

-- | Divides a list of @Text@s into a single, comma-separated @Text@ ended
-- with a provided conjunction.
commas :: Builder -> [Builder] -> Builder
commas conj = go
  where
    conj'      = " " <> conj <> " "
    go []      = mempty
    go [x]     = x
    go [x,y]   = x <> conj' <> y
    go [x,y,z] = x <> ", " <> y <> "," <> conj' <> z
    go (x:xs)  = x <> ", " <> go xs

-- | Removes spaces and special characters.
shorten :: Text -> Text
shorten xs = omap unaccent $ filter (notInClass "- _:()®'/?") xs

-- | Turns special characters into ordinary characters.
unaccent :: Char -> Char
unaccent 'ō' = 'o'
unaccent 'Ō' = 'O'
unaccent 'ū' = 'u'
unaccent 'Ū' = 'U'
unaccent 'ä' = 'a'
unaccent x   = x
{-# INLINE unaccent #-}
