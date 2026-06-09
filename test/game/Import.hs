module Import
    ( module Import'
    , gameChakras
    , hasSkill
    , measureDamage, measureDamageTo, measureHealing, measureHealingTo
    , shouldBe, shouldNotBe
    ) where

import Game.Characters.Import as Import'
import Game.Model.Ninja as Import' (Ninja(Ninja, charges, cooldowns, effects), totalBarrier, totalDefense)
import Test.Hspec as Import' hiding (context, it, shouldBe, shouldNotBe)
import Sim as Import' (simAt, describeCategory, simOf)
import SkillExample as Import'

import           Class.Play (MonadPlay(..))
import qualified Class.Play as P
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Game as Game
import qualified Game.Model.Skill as Skill
import qualified Sim

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

-- | Searches 'skills'.
hasSkill :: Text -- ^ 'Skill.name'.
         -> Ninja -> Bool
hasSkill name = any ((== name) . Skill.name) . skills . Ninjas.processSkills

measureDamage :: ∀ m. MonadPlay m => m () -> m Int
measureDamage f = do
    healthBefore <- target health
    f
    healthAfter <- target health
    return $ healthBefore - healthAfter

measureHealing :: ∀ m. MonadPlay m => m () -> m Int
measureHealing f = negate <$> measureDamage f

measureDamageTo :: ∀ m. MonadPlay m => Target -> m () -> m Int
measureDamageTo t f = do
    healthBefore <- health <$> Sim.targets t
    f
    healthAfter <- health <$> Sim.targets t
    return $ healthBefore - healthAfter

measureHealingTo :: ∀ m. MonadPlay m => Target -> m () -> m Int
measureHealingTo t f = negate <$> measureDamageTo t f
