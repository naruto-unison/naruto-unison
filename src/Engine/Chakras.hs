-- | 'Game.chakra' processing.
module Engine.Chakras
  ( remove
  , gain
  ) where

import ClassyPrelude

import qualified Data.Vector as Vector

import           Core.Util ((—))
import qualified Class.Play as P
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra(..), Chakras)
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import           Model.Trap (Trigger(..))

-- | Removes some number of 'Chakra's from the target's team.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra'.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m Chakras
remove amount = do
    user    <- P.user
    P.trigger user [OnChakra]
    if amount <= 0 then
        return 0
    else do
        target  <- P.target
        chakras <- Chakra.fromChakras . Game.getChakra target <$> P.game
        removed <- Chakra.collect . Vector.take amount <$> R.shuffle chakras
        P.alter $ Game.adjustChakra target (— removed)
        return removed

-- | Adds as many random 'Chakra's as the number of living 'Ninja.Ninja's on the
-- player's team to the player's 'Game.chakra'.
gain :: ∀ m. (MonadGame m, MonadRandom m) => m ()
gain = do
    player <- P.player
    living <- length . filter (Ninja.playing player) <$> P.ninjas
    randoms :: [Chakra] <- replicateM living Chakra.random
    P.alter $ Game.adjustChakra player (+ Chakra.collect randoms)
