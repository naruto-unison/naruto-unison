module Game.Model.Destructible
    ( Destructible(..)
    , name
    ) where

import ClassyPrelude hiding (negate)

import Game.Model.Internal (Destructible(..), Skill(Skill))
import qualified Game.Model.Internal

name :: Destructible -> Text
name Destructible{skill = Skill{name = skillName}} = skillName
