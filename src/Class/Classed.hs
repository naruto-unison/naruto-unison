module Class.Classed
  ( Classed(..)
  , nonStack
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Labeled (Labeled)
import qualified Class.Labeled as Labeled
import           Game.Model.Class (Class(..))
import           Game.Model.Effect (Effect(..))
import           Game.Model.Internal (Channel, Copy, Delay, Skill, Status, Trap)
import qualified Game.Model.Internal.Channel as Channel
import qualified Game.Model.Internal.Context as Context
import qualified Game.Model.Internal.Copy as Copy
import qualified Game.Model.Internal.Delay as Delay
import qualified Game.Model.Internal.Runnable as Runnable
import qualified Game.Model.Internal.Skill as Skill
import qualified Game.Model.Internal.Status as Status
import qualified Game.Model.Internal.Trap as Trap
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈))

-- | A type with 'Class'es.
class Classed a where
    classes :: a -> EnumSet Class

-- | Conditionally adds an item to a list of items depending on its classes.
-- If it is classified as 'Nonstacking', it will remove older items with the
-- same name and user.
nonStack :: ∀ a b. (Labeled a, Classed b) => b -> a -> [a] -> [a]
nonStack c x xs
  | Hidden ∈ classes c      = x : xs
  | Nonstacking ∈ classes c = x : filter (not . Labeled.eq x) xs
  | otherwise               = x : xs

instance Classed Channel where
    classes = classes . Channel.skill

instance Classed Copy where
    classes = classes . Copy.skill

instance Classed Delay where
    classes = classes . Context.skill .  Runnable.target . Delay.effect

instance Classed Effect where
    classes (Bleed c _ _)      = c
    classes (Exhaust c)        = c
    classes (Invulnerable c)   = singletonSet c
    classes (Reduce c _ _)     = c
    classes (ReflectAll c)     = singletonSet c
    classes (Strengthen c _ _) = c
    classes (Stun c)           = singletonSet c
    classes (Weaken c _ _)     = c
    classes _                  = mempty

instance Classed Skill where
    classes = Skill.classes

instance Classed Status where
    classes = Status.classes

instance Classed Trap where
    classes = Trap.classes

instance Classed Trigger where
    classes (Counter cla)      = singletonSet cla
    classes (CounterAll cla)   = singletonSet cla
    classes (Countered cla)    = singletonSet cla
    classes (OnAction cla)     = singletonSet cla
    classes (OnDamaged cla)    = singletonSet cla
    classes (OnHarmed cla)     = singletonSet cla
    classes _                  = mempty
