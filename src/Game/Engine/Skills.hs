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
import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID
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

toID :: Text -> Ninja -> ID
toID name Ninja{slot} = ID { user = slot, owner = slot, name }

-- | Applies a 'Transform' conditional upon 'N.has'.
changeWith :: Text -> (Skill -> Skill) -> Transform
changeWith name f n
  | N.has (toID name n) n = f
  | otherwise             = id

-- | Applies a 'Transform' conditional upon 'N.amount'.
changePer :: Text -> (Int -> Skill -> Skill) -> Transform
changePer name f n = f $ N.amount (toID name n) n

-- | Applies a 'Transform' conditional upon 'N.isChanneling'.
changeWithChannel :: Text -> (Skill -> Skill) -> Transform
changeWithChannel name f n skill@Skill{owner}
  | N.isChanneling skillID n = f skill
  | otherwise                = skill
  where
    skillID = ID { user = owner, owner, name }

-- | Applies a 'Transform' conditional upon 'N.hasDefense'.
changeWithDefense :: Text -> (Skill -> Skill) -> Transform
changeWithDefense name f n
  | N.hasDefense (toID name n) n = f
  | otherwise                    = id

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
