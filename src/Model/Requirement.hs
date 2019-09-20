module Model.Requirement
  ( Requirement(..)
  , targetable
  , usable
  , succeed
  ) where

import ClassyPrelude

import           Core.Util ((!?), (∈), (∉), intersects)
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
  | Skill.classes sk `intersects` Effects.stun n      = unusable
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
  | i > 0             = Ninja.numStacks name t n >= i
  | i < 0             = Ninja.numStacks name t n < (-i)
  | otherwise         = True
succeed (HasI i name) t n
  | t /= Ninja.slot n = True
  | i == 1            = Ninja.has name t n || Ninja.isChanneling name n
  | i > 0             = Ninja.numStacks name t n >= i
  | i < 0             = Ninja.numStacks name t n < (-i)
                        && not (Ninja.isChanneling name n)
  | otherwise         = True
succeed (DefenseI i name) t n
  | t /= Ninja.slot n = True
  | i > 0             = Ninja.defenseAmount name t n >= i
  | i < 0             = Ninja.defenseAmount name t n < (-i)
  | otherwise         = True

-- | Checks whether a @Skill@ can be used on a target.
targetable :: Skill -- ^ @Skill@ to check.
           -> Ninja -- ^ User.
           -> Ninja -- ^ Target.
           -> Bool
targetable skill n nt
  | not $ succeed (Skill.require skill) user nt = False
  | not (Ninja.alive nt) && not necro = False
  | user == target                    = True
  | Ninja.alive nt && necro           = False
  | harm && n `is` BlockEnemies       = False
  | not harm && n `is` BlockAllies    = False
  | Bypassing ∈ classes               = True
  | harm && invuln                    = False
  | not harm && nt `is` Alone         = False
  | notIn user $ Effects.duel nt      = False
  | notIn target $ Effects.taunt n    = False
  | target ∈ Effects.block n          = False
  | otherwise                         = True
  where
    classes = Skill.classes skill
    user    = Ninja.slot n
    target  = Ninja.slot nt
    necro   = Necromancy ∈ classes
    harm    = not $ Parity.allied user target
    invuln  = classes `intersects` Effects.invulnerable nt
    notIn a xs = not (null xs) && a ∉ xs
