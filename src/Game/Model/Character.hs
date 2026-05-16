module Game.Model.Character
  ( Character(..)
  , ident
  , Category(..)
  , identFrom
  , clean
  , format
  ) where

import ClassyPrelude

import Game.Model.Internal (Character(..), Category(..))
import Util (unaccent)

-- | Turns 'format' output into 'ident'.
clean :: Text -> Text
clean name = omap f $ toLower name
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

-- | Used in website links.
ident :: Character -> Text
ident x = identFrom (category x) $ name x

-- | Pretty-prints a @Character@'s 'name' and 'category'.
format :: Character -> Text
format x = formatFull (category x) $ name x
