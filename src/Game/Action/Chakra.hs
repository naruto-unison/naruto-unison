-- | Actions that characters can use to manipulate 'Chakra'.
module Game.Action.Chakra
  ( absorb, absorb1
  , deplete, deplete1
  , gain
  , healFromChakra
  , kabuto
  ) where

import ClassyPrelude

import Data.Enum.Set.Class (EnumSet)

import           Class.Play (MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Game.Engine.Chakras as Chakras
import qualified Game.Engine.Ninjas as Ninjas
import           Game.Model.Chakra (Chakra(..))
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Effect (Effect(..))
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (is)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Game.Model.Trigger (Trigger(..))
import           Util ((—))

-- ** CHAKRA
-- | Adds a finite amount of @Chakra@ to the 'Game.chakra' of the target's team.
-- 'Rand's are replaced by other @Chakra@ types selected by 'Chakras.random'.
gain :: ∀ m. (MonadPlay m, MonadRandom m) => [Chakra] -> m ()
gain chakras = P.unsilenced do
    user   <- P.user
    target <- P.target
    rand   <- replicateM (length rands) Chakra.random
    P.alter $ Game.adjustChakra target (+ Chakra.collect (rand ++ nonrands))
    P.trigger user [OnChakra]
  where
    (rands, nonrands) = partition (Rand ==) chakras

-- | Removes some number of @Chakra@s from the 'Game.chakra' of the target's team
-- team. 'Chakra's are selected at random by 'Chakras.remove'.
deplete :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m ()
deplete amount = P.unsilenced . void $ Chakras.remove amount

-- | Removes a single 'Chakra' from the enemy team that is one of several types.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra', but
-- only the ones passed in the parameter.
deplete1 :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Chakra -> m ()
deplete1 chakras = P.unsilenced . void $ Chakras.remove1 chakras

-- | Transfers some number of @Chakra@s from the 'Game.chakra' of the target's
-- team to the 'Game.chakra' of the user's team. @Chakra@s are selected at
-- random by 'Chakras.remove'.
absorb :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m ()
absorb amount = P.unsilenced do
    user    <- P.user
    chakras <- Chakras.remove amount
    P.alter $ Game.adjustChakra user (+ chakras)

-- | Transfers a single 'Chakra' that is one of several types from the
-- 'Game.chakra' of the target's team to the 'Game.chakra' of the user's team.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra', but
-- only the ones passed in the parameter.
absorb1 :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Chakra -> m ()
absorb1 chakras = P.unsilenced do
    user   <- P.user
    chakra <- Chakras.remove1 chakras
    P.alter $ Game.adjustChakra user (+ chakra)

-- | Restores health to the user multiplied by the chakra cost of the target's
-- last skill.
healFromChakra :: ∀ m. MonadPlay m => Int -> m ()
healFromChakra amount = P.unsilenced do
    nUser <- P.nUser
    when (not $ nUser `is` Plague) do
        user       <- P.user
        lastSkill  <- Ninja.lastSkill <$> P.nTarget
        let amount' = amount * maybe 0 (Chakra.total . Skill.cost) lastSkill
        when (amount' > 0) do
            P.modify user $ Ninjas.adjustHealth (+ amount')
            healed <- (— Ninja.health nUser) . Ninja.health <$> P.nUser
            when (healed > 0) $ P.trigger user [OnHeal]

-- | Cycles Kabuto Yakushi's chakra mode through the four types of 'Chakra'.
-- Uses 'Ninjas.kabuto' internally.
kabuto :: ∀ m. MonadPlay m => m ()
kabuto = do
    skill  <- P.skill
    target <- P.target
    P.modify target $ Ninjas.kabuto skill
