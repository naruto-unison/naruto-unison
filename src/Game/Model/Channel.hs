module Game.Model.Channel
  ( Channel(..), name
  , interruptible
  , Channeling(..)
  , isControl
  ) where

import ClassyPrelude

import           Game.Model.Internal (Channel(..), Channeling(..), Skill(Skill))
import qualified Game.Model.Internal

name :: Channel -> Text
name Channel{skill = Skill{name = skillName}} = skillName

-- | 'Control' and 'Action' 'Model.Skill.Skill's can be interrupted.
-- Others cannot, because they are not considered user actions.
interruptible :: Channel -> Bool
interruptible Channel{dur = Control{}} = True
interruptible Channel{dur = Action{}}  = True
interruptible _                        = False

isControl :: Channeling -> Bool
isControl Control{} = True
isControl _         = False
