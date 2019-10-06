module Game.Model.Character
  ( Character(..)
  , format
  , Category(..)
  ) where

import ClassyPrelude

import Game.Model.Internal (Character(..), Category(..))
import Util (unaccent)

format :: Character -> Text
format Character{name, category} = omap clean . toLower $ case category of
    Original   -> name
    Shippuden  -> name ++ " (S)"
    Reanimated -> name ++ " (R)"
  where
    clean ' ' = '-'
    clean x   = unaccent x
