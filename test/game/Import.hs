module Import (module X, gameChakras, shouldBe, shouldNotBe) where

import ClassyPrelude as X hiding ((\\), fromList, toList)
import Game.Action.Chakra as X
import Game.Action.Channel as X
import Game.Action.Combat as X
import Game.Action.Skill as X
import Game.Action.Status as X
import Game.Action.Trap as X
import Game.Characters.Import as X (self, target, targetHas, user, userHas, allies, enemies, everyone, numAffected)
import Game.Engine.Ninjas as X (hasSkill)
import Game.Model.Chakra as X (Chakra(..))
import Game.Model.Character as X (Category(..), Character)
import Game.Model.Class as X (Class(..))
import Game.Model.Duration as X (Duration(..))
import Game.Model.Effect as X (Amount(..), Constructor(..), Effect(..))
import Game.Model.Game as X (chakra)
import Game.Model.Ninja as X (Ninja(Ninja, charges, cooldowns, effects, health), numAnyStacks, is, isChanneling, totalBarrier, totalDefense)
import Game.Model.Skill as X (Target(..))
import Game.Model.Trigger as X (Trigger(..))
import GHC.Exts as X (fromList, toList)
import Test.Hspec as X hiding (context, it, shouldBe, shouldNotBe)
import Sim as X (simAt, describeCategory, simOf)
import SkillExample as X

import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Game as Game
import           Class.Play (MonadPlay(..))
import qualified Class.Play as P

import qualified Test.Hspec as Hspec

infix 1 `shouldBe`
shouldBe :: ∀ m a. (HasCallStack, Monad m, Eq a, Show a)
         => a -> a -> m Expectation
shouldBe x y = return $ Hspec.shouldBe x y

infix 1 `shouldNotBe`
shouldNotBe :: ∀ m a. (HasCallStack, Monad m, Eq a, Show a)
            => a -> a -> m Expectation
shouldNotBe x y = return $ Hspec.shouldNotBe x y

gameChakras :: ∀ m. MonadPlay m => m (Chakras, Chakras)
gameChakras = Game.chakra <$> P.game
