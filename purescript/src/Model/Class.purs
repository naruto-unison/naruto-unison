module Model.Class
  ( Class
  , filter
  ) where

import Prelude
import Data.Array ((\\))
import Data.Function.Memoize (memoize)

type Class = String

filter :: Boolean -> Array String -> Array String
filter = memoize \hideMore ->
    (_ \\ if hideMore then moreHidden <> hidden else hidden)

hidden :: Array String
hidden =
    [ "NonMental"
    , "Bloodline"
    , "Genjutsu"
    , "Ninjutsu"
    , "Taijutsu"
    , "Random"
    , "Necromancy"

    , "All"
    , "Harmful"
    , "Affliction"
    , "NonAffliction"
    , "NonMental"
    , "Nonstacking"
    , "Resource"
    , "Extending"
    , "Hidden"
    , "Shifted"
    , "Unshifted"
    , "Direct"
    , "BaseTrap"
    , "Healing"
    ]

moreHidden :: Array String
moreHidden =
    [ "Single"
    , "Bypassing"
    , "Uncounterable"
    , "Unreflectable"
    ]
