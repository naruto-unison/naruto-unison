module Game.Model.Requirement
  ( Requirement(..)
  , Range(..)
  , targetable
  , usable
  , targets
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import qualified Class.Parity as Parity
import           Class.Stackable (Stackable)
import qualified Game.Engine.Effects as Effects
import           Game.Model.Channel (Channeling(..))
import qualified Game.Model.Channel as Channel
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.ID (HasID, ID(ID))
import qualified Game.Model.ID
import           Game.Model.Internal (Range(..), Requirement(..))
import           Game.Model.Ninja (Ninja(Ninja), is)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill(Skill), Target(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((∈), (∉), (?), intersects)

isStunned :: Ninja -> Skill -> Bool
isStunned _ Skill{dur = Passive}   = False
isStunned _ Skill{dur = Ongoing{}} = False
isStunned n Skill{classes}         = classes `intersects` Effects.stun n

isUserHas :: Requirement -> Bool
isUserHas UserHas{} = True
isUserHas _         = False

-- | Processes 'Skill.require'.
usable :: Bool -- ^ New.
       -> Ninja -> Skill -> Skill
usable False n skill@Skill{require}
  | isStunned n skill = fulfilled { Skill.effects = mempty }
  | otherwise         = fulfilled
  where
    fulfilled
      | isUserHas require = skill { Skill.require = Usable }
      | otherwise         = skill

usable True n@Ninja{slot} skill@Skill{charges, cooldown, dur, require}
  | isUncastable            = skill { Skill.require = Unusable }
  | not $ isUserHas require = skill
  | succeed skill slot n    = skill { Skill.require = Usable }
  | otherwise               = skill { Skill.require = Unusable }
  where
    isUncastable = cooldown /= 0 && N.cooldowns `atLeast` 1
        || charges /= 0 && N.charges `atLeast` charges
        || Channel.isControl dur && n `is` Silence
        || isStunned n skill
    getter `atLeast` limit = case getter n ? Skill.key skill of
        Just value -> value >= limit
        Nothing    -> False

inRange :: Range -> Int -> Int -> Bool
inRange AtLeast minI i = i >= minI
inRange AtMost  maxI i = i <= maxI

requireAmount :: ∀ a. (HasID a, Stackable a)
              => (Ninja -> [a]) -> Range -> Int -> ID -> Ninja -> Bool
requireAmount getter AtMost  0 itemID n = not $ N.has' getter itemID n
requireAmount getter AtLeast 1 itemID n = N.has' getter itemID n
requireAmount getter r  i itemID n = inRange r i $ N.amount' getter itemID n

-- | Checks whether a user passes the 'Skill.require' of a 'Skill'.
succeed :: Skill -> Slot -> Ninja -> Bool
succeed Skill{require = Usable} _ _   = True
succeed Skill{require = Unusable} _ _ = False
succeed Skill{require = UserHas r i name, owner} user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = requireAmount N.statuses r i ID { user, owner, name } n
succeed Skill{require = TargetHas r i name, owner} user n@Ninja{slot}
  | user == slot = True
  | otherwise    = requireAmount N.statuses r i ID { user, owner, name } n
succeed Skill{require = UserHealth r i} user Ninja{health, slot}
  | user /= slot = True
  | otherwise    = inRange r i health
succeed Skill{require = TargetHealth r i} user Ninja{health, slot}
  | user == slot = True
  | otherwise    = inRange r i health
succeed Skill{require = UserChannel expected name, owner} user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = expected == N.isChanneling ID { user, owner, name } n
succeed Skill{require = UserDefense r i name, owner} user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = requireAmount N.defense r i ID { user, owner, name } n
succeed Skill{require = UserTrap expected name, owner} user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = expected == N.hasTrap ID { user, owner, name } n

-- | Checks whether a @Skill@ can be used on a target.
targetable :: Skill -- ^ @Skill@ to check.
           -> Ninja -- ^ User.
           -> Ninja -- ^ Target.
           -> Bool
targetable skill@Skill{classes} n@Ninja{slot = user} nt@Ninja{slot = target}
  | not $ succeed skill user nt    = False
  | user == target                 = True
  | not (N.alive nt) && Necromancy ∉ classes = False
  | harm && n `is` BlockEnemies    = False
  | not harm && n `is` BlockAllies = False
  | harm && invuln && not bypass   = False
  | not harm && nt `is` Alone      = False
  | user `notIn` Effects.duel nt   = False
  | target `notIn` Effects.taunt n = False
  | target ∈ Effects.block n       = False
  | otherwise                      = True
  where
    harm   = not $ Parity.allied user target
    invuln = classes `intersects` Effects.invulnerable nt
    bypass = Bypassing ∈ classes || n `is` Bypass
    a `notIn` xs = not (null xs) && a ∉ xs

-- | All targets that a @Skill@ from a a specific 'Ninja' affects.
targets :: ∀ o. (IsSequence o, Ninja ~ Element o)
        => o -> Ninja -> Skill -> o
targets ns n@Ninja{slot = user} skill = filter filt ns
  where
    filt nt = targetSlot nt.slot && targetable skill n nt
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
