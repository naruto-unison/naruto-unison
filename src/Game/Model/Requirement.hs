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
import           Game.Model.Internal (Requirement(..))
import           Game.Model.Ninja (Ninja, is)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill(Skill), Target(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import           Util ((∈), (∉), intersects)

-- | Processes 'Skill.require'.
usable :: Bool -- ^ New.
       -> Ninja -> Skill -> Skill
usable new n x
  | charges > 0 && Ninja.charges `atLeast` charges    = unusable
  | new && Ninja.cooldowns `atLeast` 1                = unusable
  | not $ Skill.classes x `intersects` Effects.stun n = x'
  | new                                               = unusable
  | Channel.ignoreStun $ Skill.dur x                  = x'
  | otherwise = x' { Skill.effects = Skill.stunned x' }
  where
    getter `atLeast` limit = maybe False (>= limit) $ key `lookup` getter n
    charges  = Skill.charges x
    key      = Skill.key x
    unusable = x { Skill.require = Unusable }
    x'       = x { Skill.require = isUsable $ Skill.require x }
    isUsable req@HasI{}
      | not new                      = Usable
      | succeed req (Ninja.slot n) n = Usable
      | otherwise                    = Unusable
    isUsable y = y

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
targetable Skill{require = Unusable} _ _ = False
targetable Skill{classes} n nt
  | user == target                 = True
  | harm && n `is` BlockEnemies    = False
  | not harm && n `is` BlockAllies = False
  | harm && invuln && not bypass   = False
  | not harm && nt `is` Alone      = False
  | notIn user $ Effects.duel nt   = False
  | notIn target $ Effects.taunt n = False
  | target ∈ Effects.block n       = False
  | otherwise                      = True
  where
    user   = Ninja.slot n
    target = Ninja.slot nt
    harm   = not $ Parity.allied user target
    invuln = classes `intersects` Effects.invulnerable nt
    bypass = Bypassing ∈ classes || n `is` Bypass
    notIn a xs = not (null xs) && a ∉ xs

-- | All targets that a @Skill@ from a a specific 'Ninja' affects.
targets :: [Ninja] -> Ninja -> Skill -> [Ninja]
targets ns n skill = filter filt ns
  where
    filt nt = targetSlot (Ninja.slot nt) && targetable skill n nt
              && ((Necromancy ∈ Skill.classes skill) /= Ninja.alive nt)
    user    = Ninja.slot n
    ts      = setFromList $
              Runnable.target <$> Skill.start skill ++ Skill.effects skill
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
