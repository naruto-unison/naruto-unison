module Model.Character (Character(..), format, Category(..)) where
import Model.Internal  (Character(..), Category(..))

import ClassyPrelude

format :: Character -> Text
format (Character name _ _ _ Original)   = name
format (Character name _ _ _ Shippuden)  = name ++ " (S)"
format (Character name _ _ _ Reanimated) = name ++ " (R)"
