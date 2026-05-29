module Game.Model.Character
  ( Character(..)
  , Category(..)
  , identFrom
  , clean
  , format
  ) where

import ClassyPrelude

import Class.Display (unaccent)
import Game.Model.Internal (Character(..), Category(..))

-- | Turns 'format' output into 'ident'.
clean :: Text -> Text
clean name = f `omap` toLower name
  where
    f ' ' = '-'
    f x   = unaccent x

formatFull :: Category -> Text -> Text
formatFull Original   name = name
formatFull Shippuden  name = name ++ " (S)"
formatFull Reanimated name = name ++ " (R)"

-- | Used in website links.
identFrom :: Category -> Text -> Text
identFrom category name = clean $ formatFull category name

-- | Pretty-prints a @Character@'s 'name' and 'category'.
format :: Character -> Text
format Character{category, name} = formatFull category name
