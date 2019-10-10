module Game.Model.Requirement
  ( Requirement(..)
  , targetable
  , usable
  , succeed
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import qualified Game.Engine.Cooldown as Cooldown
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Channel as Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Internal (Requirement(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((!?), (∈), (∉), intersects)

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
  | otherwise         = not $ Ninja.has name t n
succeed (HasI i name) t n
  | t /= Ninja.slot n = True
  | i == 1            = Ninja.has name t n || Ninja.isChanneling name n
  | i > 0             = Ninja.numStacks name t n >= i
  | i < 0             = Ninja.numStacks name t n < (-i)
                        && not (Ninja.isChanneling name n)
  | otherwise         = not $ Ninja.has name t n || Ninja.isChanneling name n
succeed (DefenseI i name) t n
  | t /= Ninja.slot n = True
  | i > 0             = Ninja.defenseAmount name t n >= i
  | i < 0             = Ninja.defenseAmount name t n < (-i)
  | otherwise         = not $ Ninja.hasDefense name t n

-- | Checks whether a @Skill@ can be used on a target.
targetable :: Skill -- ^ @Skill@ to check.
           -> Ninja -- ^ User.
           -> Ninja -- ^ Target.
           -> Bool
targetable skill n nt
  | not $ succeed (Skill.require skill) user nt = False
  | not (Ninja.alive nt) && not necro     = False
  | user == target                        = True
  | Ninja.alive nt && necro               = False
  | harm && n `is` BlockEnemies           = False
  | not harm && n `is` BlockAllies        = False
  | harm && invuln && Bypassing ∉ classes = False
  | not harm && nt `is` Alone             = False
  | notIn user $ Effects.duel nt          = False
  | notIn target $ Effects.taunt n        = False
  | target ∈ Effects.block n              = False
  | otherwise                             = True
  where
    classes = Skill.classes skill
    user    = Ninja.slot n
    target  = Ninja.slot nt
    necro   = Necromancy ∈ classes
    harm    = not $ Parity.allied user target
    invuln  = classes `intersects` Effects.invulnerable nt
    notIn a xs = not (null xs) && a ∉ xs
