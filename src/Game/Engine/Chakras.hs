-- | 'Game.chakra' processing.
module Game.Engine.Chakras
  ( remove, removeFrom, remove1
  , gain
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Game.Model.Chakra (Chakra(..), Chakras)
import qualified Game.Model.Chakra as Chakra
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Player as Player
import           Game.Model.Trigger (Trigger(..))
import           Util ((—), (∈))

-- | Removes some number of 'Chakra's from the target's team.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra'.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m Chakras
remove amount = do
    user   <- P.user
    target <- P.target
    P.trigger user [OnChakra]
    removeFrom target amount

-- | 'removeChakra' with a specified target.
removeFrom :: ∀ m p. (MonadGame m, MonadRandom m, Parity p)
           => p -> Int -> m Chakras
removeFrom target amount
  | amount <= 0 = return 0
  | otherwise   = do
      chakras <- Chakra.toSequence . removeRandoms . Parity.getOf target .
                 Game.chakra <$> P.game
      removed <- Chakra.collect . take amount <$> R.shuffle chakras
      P.alter $ Game.adjustChakra target (— removed)
      return removed
  where
    removeRandoms x = x { Chakra.rand = 0 }

-- | Removes a single 'Chakra' from the enemy team that is one of several types.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra', but
-- only the ones passed in the parameter.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove1 :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Chakra -> m Chakras
remove1 permitted = do
    user     <- P.user
    target   <- P.target
    P.trigger user [OnChakra]
    chakras :: [Chakra]
             <- filter (∈ permitted) . Chakra.toSequence . Parity.getOf target .
                Game.chakra <$> P.game
    mRemoved <- R.choose chakras
    case mRemoved of
        Nothing                            -> return 0
        Just (Chakra.toChakras -> removed) -> do
            P.alter $ Game.adjustChakra target (— removed)
            return removed

-- | Adds as many random 'Chakra's as the number of living 'Ninja.Ninja's on the
-- player's team to the player's 'Game.chakra'.
gain :: ∀ m. (MonadGame m, MonadRandom m) => m ()
gain = do
    player  <- Player.opponent <$> P.player
    living  <- length . filter Ninja.alive <$> P.allies player
    randoms :: [Chakra]
            <- replicateM living Chakra.random
    P.alter $ Game.adjustChakra player (+ Chakra.collect randoms)
