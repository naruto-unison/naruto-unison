module Model.Requirement
  ( Requirement(..)
  , targetable
  , usable
  , succeed
  ) where

import ClassyPrelude

import           Core.Util ((!?), (∈), (∉), intersectsSet)
import qualified Class.Parity as Parity
import           Model.Internal (Requirement(..))
import qualified Model.Channel as Channel
import           Model.Class (Class(..))
import           Model.Effect (Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, is)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)
import qualified Engine.Cooldown as Cooldown
import qualified Engine.Effects as Effects

-- | Processes 'Skill.require'.
usable :: Ninja
       -> Maybe Int -- ^ Index in 'Character.skills'.
       -> Skill -> Skill
usable n s sk
  | Skill.charges sk > 0 && uncharged                 = unusable
  | maybe False (> 0) $ (Cooldown.active n !?) =<< s  = unusable
  | isNothing s && Channel.ignoreStun (Skill.dur sk)  = sk'
  | Skill.classes sk `intersectsSet` Effects.stun n   = unusable
  | isNothing s                                       = sk'
  | Single ∉ Skill.classes sk                         = sk'
  | Ninja.isChanneling (Skill.name sk) n              = unusable
  | Ninja.has (Skill.name sk) (Ninja.slot n) n        = unusable
  | Ninja.hasDefense (Skill.name sk) (Ninja.slot n) n = unusable
  | Ninja.hasTrap (Skill.name sk) (Ninja.slot n) n    = unusable
  | otherwise                                         = sk'
  where
    uncharged = maybe False (>= Skill.charges sk) $
                (Ninja.charges n !?) =<< s
    unusable  = sk { Skill.require = Unusable }
    sk'       = sk { Skill.require = isUsable $ Skill.require sk }
    isUsable req@HasI{}
      | isNothing s                  = Usable
      | succeed req (Ninja.slot n) n = Usable
      | otherwise                    = Unusable
    isUsable x = x

-- | Checks whether a user passes the 'Skill.require' of a 'Skill'.
succeed :: Requirement -> Slot -> Ninja -> Bool
succeed Usable      _ _ = True
succeed Unusable    _ _ = False
succeed (HasU i name) t n
  | t == Ninja.slot n = True
  | i == 1            = Ninja.has name t n || Ninja.hasTrap name t n
  | i > 0             = Ninja.numStacks name t n >= i
  | i < 0             = Ninja.numStacks name t n < i
                        && not (Ninja.hasTrap name t n)
  | otherwise         = True
succeed (HasI i name) t n
  | t /= Ninja.slot n = True
  | i > 0             = Ninja.numActive name n >= i
  | i < 0             = Ninja.numActive name n < (-i)
  | otherwise         = True

-- | Checks whether a @Skill@ can be used on a target.
targetable :: Skill -- ^ @Skill@ to check.
           -> Ninja -- ^ User.
           -> Ninja -- ^ Target.
           -> Bool
targetable skill n nt
  | not $ succeed (Skill.require skill) user nt              = False
  | not (Ninja.alive nt) && Necromancy ∉ classes             = False
  | Ninja.alive nt && user /= target && Necromancy ∈ classes = False
  | Bypassing ∈ classes                                      = True
  | harm && (classes `intersectsSet` Effects.immune nt)      = False
  | user /= target && not harm && nt `is` Seal               = False
  | user /= target && (dueling || taunted)                   = False
  | target ∈ Effects.block  n                                = False
  | otherwise                                                = True
  where
    classes = Skill.classes skill
    user    = Ninja.slot n
    target  = Ninja.slot nt
    harm    = not $ Parity.allied user target
    dueling = notIn user $ Effects.duel nt
    taunted = notIn target $ Effects.taunt n
    notIn a xs = not (null xs) && a ∉ xs
