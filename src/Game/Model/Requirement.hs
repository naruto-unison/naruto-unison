module Game.Model.Requirement
  ( Requirement(..)
  , targetable
  , usable
  , succeed
  , targets
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import qualified Class.Parity as Parity
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Channel as Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.ID (ID(ID))
import qualified Game.Model.ID
import           Game.Model.Internal (Requirement(..))
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill), Target(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((∈), (∉), intersects)

-- | Processes 'Skill.require'.
usable :: Bool -- ^ New.
       -> Ninja -> Skill -> Skill
usable new n@Ninja{slot} x@Skill{charges, cooldown, classes, owner}
  | not new                                  = x'
  | cooldown /= 0 && N.cooldowns `atLeast` 1 = unusable
  | charges == 0                             = x'
  | N.charges `atLeast` charges              = unusable
  | otherwise                                = x'
  where
    getter `atLeast` limit = case key `lookup` getter n of
                                Just value -> value >= limit
                                Nothing    -> False
    key      = Skill.key x
    unusable = x { Skill.require = Unusable }
    required = x { Skill.require = isUsable $ Skill.require x }
    x'
      | Channel.ignoreStun $ Skill.dur x          = required
      | not $ classes `intersects` Effects.stun n = required
      | new                                       = unusable
      | otherwise = required { Skill.effects = Skill.stunned required }
    isUsable req@UserHas{}
      | not new                  = Usable
      | succeed owner req slot n = Usable
      | otherwise                = Unusable
    isUsable y = y

meetStatusRequirements :: Int -> ID -> Ninja -> Bool
meetStatusRequirements 0 statusID n = not $ N.has statusID n
meetStatusRequirements 1 statusID n = N.has statusID n
meetStatusRequirements i statusID n = N.numStacks statusID n >= i

-- | Checks whether a user passes the 'Skill.require' of a 'Skill'.
succeed :: Slot -> Requirement -> Slot -> Ninja -> Bool
succeed _     Usable      _ _ = True
succeed _     Unusable    _ _ = False
succeed owner (UserHas i name) user n@Ninja{slot = target}
  | user /= target = True
  | otherwise      = meetStatusRequirements i ID { user, owner, name } n
succeed owner (TargetHas i name) user n@Ninja{slot = target}
  | user == target = True
  | otherwise      = meetStatusRequirements i ID { user, owner, name } n
succeed _     (UserHealth i) user Ninja{health, slot = target}
  | user /= target = True
  | otherwise      = health <= i
succeed _     (TargetHealth i) user Ninja{health, slot = target}
  | user == target = True
  | otherwise      = health <= i
succeed _     (UserChannel expected name) user n@Ninja{slot = target}
  | user /= target = True
  | otherwise      = expected == N.isChanneling name n
succeed owner (UserDefense i name) user n@Ninja{slot = target}
  | user /= target = True
  | i > 0          = N.defenseAmount ID { user, owner, name } n >= i
  | otherwise      = not $ N.hasDefense ID { user, owner, name } n
succeed _     (UserTrap expected name) user n@Ninja{slot = target}
  | user /= target = True
  | otherwise      = expected == N.hasOwn' N.traps name n

-- | Checks whether a @Skill@ can be used on a target.
targetable :: Skill -- ^ @Skill@ to check.
           -> Ninja -- ^ User.
           -> Ninja -- ^ Target.
           -> Bool
targetable Skill{classes, owner, require} n@Ninja{slot = user} nt@Ninja{slot = target}
  | not $ succeed owner require user nt = False
  | user == target                      = True
  | not (N.alive nt) && Necromancy ∉ classes = False
  | harm && n `is` BlockEnemies         = False
  | not harm && n `is` BlockAllies      = False
  | harm && invuln && not bypass        = False
  | not harm && nt `is` Alone           = False
  | user `notIn` Effects.duel nt        = False
  | target `notIn` Effects.taunt n      = False
  | target ∈ Effects.block n            = False
  | otherwise                           = True
  where
    harm         = not $ Parity.allied user target
    invuln       = classes `intersects` Effects.invulnerable nt
    bypass       = Bypassing ∈ classes || n `is` Bypass
    a `notIn` xs = not (null xs) && a ∉ xs

-- | All targets that a @Skill@ from a a specific 'Ninja' affects.
targets :: [Ninja] -> Ninja -> Skill -> [Ninja]
targets ns n@Ninja{slot = user} skill = filter filt ns
  where
    filt nt = targetSlot (N.slot nt) && targetable skill n nt
    ts      = Skill.targets skill
    targetSlot t
      | Everyone ∈ ts                = True
      | not $ Parity.allied user t   = ts `intersects` harmTargets
      | ts `intersects` xAllyTargets = user /= t
      | ts `intersects` allyTargets  = True
      | user == t                    = not $ ts `intersects` harmTargets
      | otherwise                    = False

harmTargets  :: EnumSet Target
harmTargets   = setFromList [Enemy, Enemies, REnemy, XEnemies]
xAllyTargets :: EnumSet Target
xAllyTargets  = setFromList [XAlly, XAllies]
allyTargets  :: EnumSet Target
allyTargets   = setFromList [Ally, Allies, RAlly, RXAlly]
