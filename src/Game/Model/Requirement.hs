module Game.Model.Requirement
  ( Requirement(..)
  , Range(..)
  , withSkillName
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
import qualified Game.Model.Character
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

withSkillName :: Text -> Requirement -> Requirement
withSkillName name (UserHas r i "") = UserHas r i name
withSkillName name (TargetHas r i "") = TargetHas r i name
withSkillName name (TargetHasFromAny r i "") = TargetHasFromAny r i name
withSkillName name (UserChannel b "") = UserChannel b name
withSkillName name (UserDefense r i "") = UserDefense r i name
withSkillName name (UserTrap b "") = UserTrap b name
withSkillName _ req = req

-- | Processes 'Skill.require'.
usable :: Bool -- ^ New.
       -> Ninja -> Skill -> Skill
usable False n skill
  | isStunned skill = skill' { Skill.effects = skill.stunned }
  | otherwise       = skill'
  where
    skill' = skill { Skill.require = filter (not . skip) skill.require }
    skip UserHas{} = True
    skip _         = False
    isStunned Skill{dur = Passive}   = False
    isStunned Skill{dur = Ongoing{}} = False
    isStunned Skill{classes}         = classes `intersects` Effects.stun n

usable True n@Ninja{slot} skill@Skill{charges, classes, dur, owner}
  | isUncastable = skill { Skill.require = [Unusable] }
  | otherwise    = skill { Skill.require = mapMaybe complete skill.require }
  where
    isUncastable = Skill.hasCooldown skill && N.cooldowns `atLeast` 1
        || Skill.hasCharges skill && N.charges `atLeast` charges
        || Channel.isControl dur && n `is` Silence
        || classes `intersects` Effects.stun n
    getter `atLeast` limit = case getter n ? Skill.key skill of
        Just value -> value >= limit
        Nothing    -> False
    complete req@UserHas{}
      | succeed req owner slot n = Nothing
      | otherwise                = Just Unusable
    complete req                 = Just req

inRange :: Range -> Int -> Int -> Bool
inRange AtLeast minI i = i >= minI
inRange AtMost  maxI i = i <= maxI

requireAmount :: ∀ a. (HasID a, Stackable a)
              => (Ninja -> [a]) -> Range -> Int -> ID -> Ninja -> Bool
requireAmount getter AtMost  0 itemID n = not $ N.has' getter itemID n
requireAmount getter AtLeast 1 itemID n = N.has' getter itemID n
requireAmount getter r  i itemID n = inRange r i $ N.amount' getter itemID n

requireFromAny :: ∀ a. (HasID a, Stackable a)
              => (Ninja -> [a]) -> Range -> Int -> Text -> Ninja -> Bool
requireFromAny getter AtMost  0 name n = not $ N.hasFromAny' getter name n
requireFromAny getter AtLeast 1 name n = N.hasFromAny' getter name n
requireFromAny getter r  i name n = inRange r i $ N.amountFromAny' getter name n

-- | Checks whether a user passes the 'Skill.require' of a 'Skill'.
succeed :: Requirement -> Slot -> Slot -> Ninja -> Bool
succeed Unusable _ _ _ = False

succeed (UserHas r i name) owner user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = requireAmount N.statuses r i ID { user, owner, name } n
succeed (TargetHas r i name) owner user n@Ninja{slot}
  | user == slot = True
  | otherwise    = requireAmount N.statuses r i ID { user, owner, name } n

succeed (TargetHasFromAny r i name) _ user n@Ninja{slot}
  | user == slot = True
  | otherwise    = requireFromAny N.statuses r i name n

succeed (UserHealth r i) _  user Ninja{health, slot}
  | user /= slot = True
  | otherwise    = inRange r i health
succeed (TargetHealth r i) _ user Ninja{health, slot}
  | user == slot = True
  | otherwise    = inRange r i health

succeed (UserChannel expected name) owner user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = expected == N.isChanneling ID { user, owner, name } n

succeed (UserDefense r i name) owner user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = requireAmount N.defense r i ID { user, owner, name } n

succeed (UserTrap expected name) owner user n@Ninja{slot}
  | user /= slot = True
  | otherwise    = expected == N.hasTrap ID { user, owner, name } n

succeed (TargetCategory expected category) _ user n@Ninja{slot}
  | user == slot = True
  | otherwise    = expected == (n.character.category == category)


-- | Checks whether a @Skill@ can be used on a target.
targetable :: Skill -- ^ @Skill@ to check.
           -> Ninja -- ^ User.
           -> Ninja -- ^ Target.
           -> Bool
targetable Skill{classes, require, owner} n@Ninja{slot = user} nt@Ninja{slot = target}
  | any unfulfilled require        = False
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
    unfulfilled req = not $ succeed req owner user nt

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
