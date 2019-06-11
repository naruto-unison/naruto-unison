module Model.Requirement
  ( Requirement(..)
  , targetable
  , usable
  , succeed
  ) where

import ClassyPrelude.Yesod
import Data.Sequence ((!?))

import           Core.Util ((∈), (∉), intersects)
import qualified Class.Parity as Parity
import           Model.Internal (Requirement(..))
import qualified Model.Channel as Channel
import           Model.Class (Class(..))
import           Model.Effect (Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)
import qualified Engine.Cooldown as Cooldown
import qualified Engine.Effects as Effects

usable :: Ninja
       -> Maybe Int -- ^ Index in 'Character.skills'
       -> Skill -> Skill
usable n s sk
  | Skill.charges sk > 0 && uncharged                     = unusable
  | maybe False (>0) $ s >>= (Cooldown.active n !?)       = unusable
  | isNothing s && Channel.noInterrupt (Skill.channel sk) = sk'
  | Skill.classes sk `intersects` Effects.stun n          = unusable
  | isNothing s                                           = sk'
  | Single ∉ Skill.classes sk                             = sk'
  | Ninja.isChanneling (Skill.name sk) n                  = unusable
  | Ninja.has (Skill.name sk) (Ninja.slot n) n            = unusable
  | Ninja.hasDefense (Skill.name sk) (Ninja.slot n) n     = unusable
  | Ninja.hasTrap (Skill.name sk) (Ninja.slot n) n        = unusable
  | otherwise                                             = sk'
  where
    uncharged = maybe False (>= Skill.charges sk) $
                s >>= (Ninja.charges n !?)
    unusable  = sk { Skill.require = Unusable }
    sk'       = sk { Skill.require = isUsable $ Skill.require sk }
    isUsable req@HasI{}
      | isNothing s                  = Usable
      | succeed req (Ninja.slot n) n = Usable
      | otherwise                    = Unusable
    isUsable x = x

succeed :: Requirement -> Slot -> Ninja -> Bool
succeed Usable      _ _ = True
succeed Unusable    _ _ = False
succeed (HasU name) t n = t == Ninja.slot n
                              || Ninja.has name t n
                              || Ninja.hasTrap name t n
succeed (HasI i name) t n
  | i > 0     = t /= Ninja.slot n || Ninja.numActive name n >= i
  | i < 0     = t /= Ninja.slot n || Ninja.numActive name n < (-i)
  | otherwise = True

targetable :: Skill -> Ninja -> Ninja -> Ninja -> Bool
targetable skill nSrc n nt
  | not $ succeed (Skill.require skill) source nt                = False
  | not (Ninja.alive nt) && Necromancy ∉ classes                 = False
  | Ninja.alive nt && source /= target && Necromancy ∈ classes   = False
  | Bypassing ∈ classes                                          = True
  | harm && (classes `intersects` Effects.immune nt)             = False
  | source /= target && not harm && Ninja.is Seal nt             = False
  | user /= target && (Ninja.is Isolate n || dueling || taunted) = False
  | target ∈ Effects.targets Block n                             = False
  | otherwise                                                    = True
  where
    classes = Skill.classes skill
    source  = Ninja.slot nSrc
    user    = Ninja.slot n
    target  = Ninja.slot nt
    harm    = not $ Parity.allied user target && Parity.allied source target
    dueling = notIn user $ Effects.targets Duel nt
    taunted = notIn target $ Effects.targets Taunt n
    notIn a xs = not (null xs) && a ∉ xs
