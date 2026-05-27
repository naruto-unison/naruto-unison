module Game.Model.Channel
  ( Channel(..)
  , interruptible
  , Channeling(..)
  , ignoreStun
  , isOngoing
  ) where

import ClassyPrelude

import qualified Class.TurnBased as TurnBased
import           Game.Model.Internal (Channel(..), Channeling(..))

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

isOngoing :: Channel -> Bool
isOngoing (Channel _ _ new dur) = not new && not (TurnBased.expiring dur)
