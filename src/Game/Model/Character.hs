module Game.Model.Character
  ( Character(..)
  , format
  , Category(..)
  , formatFrom
  , clean
  ) where

import ClassyPrelude

import Game.Model.Internal (Character(..), Category(..))
import Util (unaccent)

clean :: Text -> Text
clean name = omap f $ toLower name
  where
    f ' ' = '-'
    f x   = unaccent x

formatFrom :: Category -> Text -> Text
formatFrom category name = clean case category of
    Original   -> name
    Shippuden  -> name ++ " (S)"
    Reanimated -> name ++ " (R)"

format :: Character -> Text
format Character{..} = formatFrom category name
