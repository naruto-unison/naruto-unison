{-# LANGUAGE DeriveAnyClass #-}

module Game.Model.Trigger
  ( Trigger(..)
  , isCounter
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))

import Class.Classed (Classed(..))
import Class.Display (Display(..))
import Game.Model.Class (Class(..), lower)

-- | Conditions to activate a 'Game.Model.Trap.Trap'
data Trigger
    = Counter Class
    | CounterAll Class
    | Countered Class
    | Nullified
    | OnAction Class
    | OnNoAction
    | OnBreak Text
    | OnChakra
    | OnDamage
    | OnDamaged Class
    | OnDeath
    | OnDefend
    | OnHarm
    | OnHarmed Class
    | OnHeal
    | OnHelped
    | OnInvulnerable
    | OnReduce
    | OnReflect
    | OnRes
    | OnStun
    | OnStunned
    | PerDamaged
    deriving (Eq, Ord, Show, Read, Generic, Hashable)

instance ToJSON Trigger where
    toJSON = toJSON . display'
    {-# INLINE toJSON #-}

instance Classed Trigger where
    classes (Counter cla)      = singletonSet cla
    classes (CounterAll cla)   = singletonSet cla
    classes (Countered cla)    = singletonSet cla
    classes (OnAction cla)     = singletonSet cla
    classes (OnDamaged cla)    = singletonSet cla
    classes (OnHarmed cla)     = singletonSet cla
    classes _                  = mempty

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
    display (OnAction  All)    = "Trigger: Use any skill"
    display (OnAction  cla)    = "Trigger: Use " ++ lower cla ++ " skills"
    display (OnBreak   name)   = "Trigger: Lose all destructible defense from '" ++ display name ++ "'"
    display OnChakra           = "Trigger: Gain, deplete, or absorb chakra"
    display OnDamage           = "Trigger: Deal damage"
    display (OnDamaged All)    = "Trigger: Receive damage"
    display (OnDamaged cla)    = "Trigger: Receive " ++ lower cla ++ " damage"
    display OnDeath            = "Trigger: Die"
    display OnDefend           = "Trigger: Provide destructible defense"
    display OnHarm             = "Trigger: Use a skill on an enemy"
    display (OnHarmed All)     = "Trigger: Be affected by a new skill from an enemy"
    display (OnHarmed cla)     = "Trigger: Be affected by a new " ++ lower cla ++ " skill from an enemy"
    display OnHeal             = "Trigger: Restore health"
    display OnHelped           = "Trigger: Be affected by a new skill from an ally"
    display OnInvulnerable     = "Trigger: Become invulnerable"
    display OnNoAction         = "Trigger: Do not use a new skill"
    display OnReduce           = "Trigger: Apply damage reduction"
    display OnReflect          = "Trigger: Reflect a skill"
    display OnRes              = "Trigger: Reach 0 health"
    display OnStun             = "Trigger: Apply a stun or disabling effect"
    display OnStunned          = "Trigger: Stunned"
    display PerDamaged         = "Trigger: Receive damage"

isCounter :: Trigger -> Bool
isCounter Counter{}    = True
isCounter CounterAll{} = True
isCounter Countered{}  = True
isCounter _            = False
