module Game.Model.Channel
  ( Channel(..), name
  , interruptible
  , Channeling(..)
  , ignoreStun
  ) where

import ClassyPrelude

import           Game.Model.Internal (Channel(..), Channeling(..), Skill(Skill))
import qualified Game.Model.Internal

name :: Channel -> Text
name (Channel Skill{name = skillName} _ _ _) = skillName

-- | 'Control' and 'Action' 'Model.Skill.Skill's can be interrupted.
-- Others cannot, because they are not considered user actions.
interruptible :: Channel -> Bool
interruptible (Channel _ _ _ Control{}) = True
interruptible (Channel _ _ _ Action{})  = True
interruptible _                       = False

-- | 'Passive' and 'Ongoing' effects are not affected by 'Model.Effect.Stun'.
ignoreStun :: Channeling -> Bool
ignoreStun Passive   = True
ignoreStun Ongoing{} = True
ignoreStun _         = False
