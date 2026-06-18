
module Game.Model.Trigger
  ( Trigger(..)
  , affectsDead
  , isCounter
  , isSingleUse
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))

import           Class.Classed (Classed(..))
import           Class.Display (Display(..))
import           Game.Model.Class (Class(..), lower)
import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID

-- | Conditions to activate a 'Game.Model.Trap.Trap'
data Trigger
    = Counter Class
    | CounterAll Class
    | Countered Class
    | Nullified
    | OnAction Class
    | OnBreak ID
    | OnChakra
    | OnDamage
    | OnDamaged Class
    | OnDeath
    | OnDefend
    | OnExecute
    | OnHarm
    | OnHarmed Class
    | OnHeal
    | OnHelp
    | OnHelped
    | OnInvulnerable
    | OnNoAction
    | OnReduce
    | OnReflect
    | OnSacrifice
    | OnStun
    | OnStunned
    | PerDamaged
    | Resurrect
    deriving (Eq, Ord, Show, Generic)

instance Hashable Trigger

instance ToJSON Trigger where
    toJSON = toJSON . display'

instance Classed Trigger where
    getClasses (Counter cla)      = singleton cla
    getClasses (CounterAll cla)   = singleton cla
    getClasses (Countered cla)    = singleton cla
    getClasses (OnAction cla)     = singleton cla
    getClasses (OnDamaged cla)    = singleton cla
    getClasses (OnHarmed cla)     = singleton cla
    getClasses _                  = mempty

instance Display Trigger where
    display (Counter Uncounterable)    = "Next skill received from an enemy will be negated."
    display (CounterAll Uncounterable) = "All skills received from enemies will be negated."
    display (Countered Uncounterable)  = "Next harmful skill used will be negated."
    display (Counter All)      = "Next skill received from an enemy will be countered."
    display (Counter cla)      = "Next " ++ lower cla ++ " skill received from an enemy will be countered."
    display (CounterAll All)   = "All skills received from enemies will be countered."
    display (CounterAll cla)   = "All " ++ lower cla ++ " skills received from enemies will be countered."
    display (Countered All)    = "Next skill used on an enemy will be countered."
    display (Countered cla)    = "Next " ++ lower cla ++ " skill used on an enemy will be countered."
    display Nullified          = "Next skill used will be countered."
    display (OnAction  All)    = "Trigger: Use any skill."
    display (OnAction  cla)    = "Trigger: Use " ++ lower cla ++ " skills."
    display (OnBreak ID{name}) = "Trigger: Lose all destructible defense from [" ++ display name ++ "]."
    display OnChakra           = "Trigger: Gain, deplete, or absorb chakra."
    display OnDamage           = "Trigger: Deal damage."
    display (OnDamaged All)    = "Trigger: Receive damage."
    display (OnDamaged cla)    = "Trigger: Receive " ++ lower cla ++ " damage."
    display OnDeath            = "Trigger: Die."
    display OnDefend           = "Trigger: Provide destructible defense."
    display OnExecute          = "Trigger: Execute an enemy with an instant-kill effect."
    display OnHarm             = "Trigger: Use a skill on an enemy."
    display (OnHarmed All)     = "Trigger: Be affected by a new skill from an enemy."
    display (OnHarmed cla)     = "Trigger: Be affected by a new " ++ lower cla ++ " skill from an enemy."
    display OnHeal             = "Trigger: Restore health."
    display OnHelp             = "Trigger: Use a skill on an ally."
    display OnHelped           = "Trigger: Be affected by a new skill from an ally."
    display OnInvulnerable     = "Trigger: Become invulnerable."
    display OnNoAction         = "Trigger: Do not use a new skill."
    display OnReduce           = "Trigger: Apply damage reduction."
    display OnReflect          = "Trigger: Reflect a skill."
    display OnSacrifice        = "Trigger: Use a skill that sacrifices the user's health."
    display OnStun             = "Trigger: Apply a stun or disabling effect."
    display OnStunned          = "Trigger: Stunned."
    display PerDamaged         = "Trigger: Receive damage."
    display Resurrect          = "Trigger: Reach 0 health."

affectsDead :: Trigger -> Bool
affectsDead OnDeath   = True
affectsDead Resurrect = True
affectsDead _         = False

isCounter :: Trigger -> Bool
isCounter Counter{}    = True
isCounter CounterAll{} = True
isCounter Countered{}  = True
isCounter _            = False

isSingleUse :: Trigger -> Bool
isSingleUse Counter{} = True
isSingleUse OnBreak{} = True
isSingleUse OnDeath   = True
isSingleUse Resurrect = True
isSingleUse _         = False
