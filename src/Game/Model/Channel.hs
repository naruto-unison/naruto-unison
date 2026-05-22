module Game.Model.Channel
  ( Channel(..)
  , interruptible
  , Channeling(..)
  , ignoreStun
  ) where

import ClassyPrelude

import Game.Model.Internal (Channel(..), Channeling(..))

-- | 'Control' and 'Action' 'Model.Skill.Skill's can be interrupted.
-- Others cannot, because they are not considered user actions.
interruptible :: Channel -> Bool
interruptible (Channel _ _ _ Control{}) = True
interruptible (Channel _ _ _ Action{})  = True
interruptible _                       = False

-- | 'Ongoing' effects are not affected by 'Model.Effect.Stun'.
ignoreStun :: Channeling -> Bool
ignoreStun Ongoing{} = True
ignoreStun _         = False
