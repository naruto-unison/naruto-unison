module Import (module X, shouldBe, shouldNotBe) where

import ClassyPrelude as X hiding ((\\), fromList, toList)
import Class.Play as X (user, target, nUser, nTarget, game)
import Game.Action.Chakra as X
import Game.Action.Channel as X
import Game.Action.Combat as X
import Game.Action.Skill as X
import Game.Action.Status as X
import Game.Action.Trap as X
import Game.Characters.Import as X (self, targetHas, userHas, allies, enemies, everyone, numAffected)
import Game.Model.Chakra as X (Chakra(..))
import Game.Model.Character as X (Category(..), Character)
import Game.Model.Class as X (Class(..))
import Game.Model.Duration as X (Duration(..))
import Game.Model.Effect as X (Amount(..), Constructor(..), Effect(..))
import Game.Model.Game as X (chakra)
import Game.Model.Ninja as X (Ninja(charges, health), has, hasOwn, numAnyStacks, is, isChanneling, totalBarrier, totalDefense)
import Game.Model.Skill as X (Target(..))
import Game.Model.Trigger as X (Trigger(..))
import GHC.Exts as X (fromList, toList)
import Test.Hspec as X hiding (context, it, shouldBe, shouldNotBe)
import Sim as X
import SkillExample as X

import qualified Test.Hspec as Hspec

infix 1 `shouldBe`
shouldBe :: ∀ m a. (HasCallStack, Monad m, Eq a, Show a)
         => a -> a -> m Expectation
shouldBe x y = return $ Hspec.shouldBe x y

infix 1 `shouldNotBe`
shouldNotBe :: ∀ m a. (HasCallStack, Monad m, Eq a, Show a)
            => a -> a -> m Expectation
shouldNotBe x y = return $ Hspec.shouldNotBe x y
