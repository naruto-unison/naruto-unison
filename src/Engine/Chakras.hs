-- | 'Game.chakra' processing.
module Engine.Chakras
  ( random
  , remove
  , gain
  ) where

import ClassyPrelude

import qualified Data.Vector as Vec

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

-- | Randomly picks a 'Chakra' of any kind except 'Rand'.
random :: ∀ m. MonadRandom m => m Chakra
random = toEnum <$> R.random (fromEnum Blood) (fromEnum Tai)

-- | Removes some number of 'Chakra's from the target's team.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra'.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m Chakras
remove amount
  | amount <= 0 = return 0
  | otherwise   = do
    user    <- P.user
    target  <- P.target
    chakras <- Chakra.fromChakras . Game.getChakra target <$> P.game
    removed <- Chakra.collect . Vec.take amount <$> R.shuffle chakras
    P.modify $ Game.adjustChakra target (— removed)
    P.trigger user [OnChakra]
    return removed

-- | Adds as many random 'Chakra's as the number of living 'Ninja.Ninja's on the
-- player's team to the player's 'Game.chakra'.
gain :: ∀ m. (MonadGame m, MonadRandom m) => m ()
gain = do
    player <- P.player
    living <- length . filter (Ninja.playing player) . Game.ninjas <$> P.game
    randoms :: [Chakra] <- replicateM living random
    P.modify $ Game.adjustChakra player (+ Chakra.collect randoms)
