module Game.Model.Character (Character(..), format, Category(..)) where
import Game.Model.Internal  (Character(..), Category(..))

import ClassyPrelude

format :: Character -> Text
format (Character name _ _ Original)   = name
format (Character name _ _ Shippuden)  = name ++ " (S)"
format (Character name _ _ Reanimated) = name ++ " (R)"
