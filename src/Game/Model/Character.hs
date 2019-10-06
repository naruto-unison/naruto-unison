module Game.Model.Character
  ( Character(..)
  , format
  , Category(..)
  , formatFrom
  ) where

import ClassyPrelude

import Game.Model.Internal (Character(..), Category(..))
import Util (unaccent)

formatFrom :: Category -> Text -> Text
formatFrom category name = omap clean . toLower $ case category of
    Original   -> name
    Shippuden  -> name ++ " (S)"
    Reanimated -> name ++ " (R)"
  where
    clean ' ' = '-'
    clean x   = unaccent x

format :: Character -> Text
format Character{..} = formatFrom category name
