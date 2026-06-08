-- | 'Transform' processing.
module Game.Engine.Skills
  ( change
  , also
  , changeWith, changeWithChannel, changeWithDefense
  , changePer
  ) where

import ClassyPrelude hiding (swap)


import qualified Game.Engine.Effects as Effects
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (is)
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill

-- | The type signature of 'changes'.
type Transform = (Ninja -> Skill -> Skill)

-- | Combines two 'Transform's.
also :: Transform -> Transform -> Transform
(f `also` g) n = g n . f n

-- | Applies a 'Transform' conditional upon 'N.has'.
changeWith :: Text -> (Skill -> Skill) -> Transform
changeWith name f n@Ninja{slot}
  | N.has name slot n = f
  | otherwise         = id

-- | Applies a 'Transform' conditional upon 'N.numStacks'.
changePer :: Text -> (Int -> Skill -> Skill) -> Transform
changePer name f n@Ninja{slot} = f $ N.numStacks name slot n

-- | Applies a 'Transform' conditional upon 'N.isChanneling'.
changeWithChannel :: Text -> (Skill -> Skill) -> Transform
changeWithChannel name f n
  | N.isChanneling name n = f
  | otherwise             = id

-- | Applies a 'Transform' conditional upon 'N.hasDefense'.
changeWithDefense :: Text -> (Skill -> Skill) -> Transform
changeWithDefense name f n@Ninja{slot}
  | N.hasDefense name slot n = f
  | otherwise                = id

-- | Modifies a 'Skill' by its 'Skill.change' and any other effects on it.
change :: Transform
change n skill@Skill{changes, classes, cost} =
    Skill.chakraClasses
    . changeIf Swap Skill.swap
    . changeIf Restrict Skill.restrict
    $ changes n skill { Skill.cost = Effects.exhaust classes n ++ cost }
  where
    changeIf ef f
      | n `is` ef = f
      | otherwise = id
