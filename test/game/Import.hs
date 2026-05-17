module Import (module X, gameChakras, shouldBe, shouldNotBe) where

import Game.Characters.Import as X
import Game.Engine.Ninjas as X (hasSkill)
import Game.Model.Ninja as X (Ninja(Ninja, charges, cooldowns, effects), totalBarrier, totalDefense)
import Test.Hspec as X hiding (context, it, shouldBe, shouldNotBe)
import Sim as X (simAt, describeCategory, simOf)
import SkillExample as X

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
