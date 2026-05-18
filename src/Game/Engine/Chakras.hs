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
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈))

-- | Removes some number of 'Chakra's from the target's team.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra'.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m Chakras
remove amount = do
    Context{target, user} <- P.context
    P.trigger user [OnChakra]
    removeFrom target amount

-- | 'removeChakra' with a specified target.
removeFrom :: ∀ m p. (MonadGame m, MonadRandom m, Parity p)
           => p -> Int -> m Chakras
removeFrom target amount
  | amount <= 0 = return 0
  | otherwise   = do
      Game{chakra} <- P.game
      let chakras = Chakra.toSequence . removeRandoms
                  $ Parity.getOf target chakra
      removed <- Chakra.collect . take amount <$> R.shuffle chakras
      P.alter $ Game.adjustChakra target (- removed)
      return removed
  where
    removeRandoms x = x { Chakra.rand = 0 }

-- | Removes a single 'Chakra' from the enemy team that is one of several types.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra', but
-- only the ones passed in the parameter.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove1 :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Chakra -> m Chakras
remove1 permitted = do
    Context{target, user} <- P.context
    P.trigger user [OnChakra]
    Game{chakra} <- P.game
    let chakras = filter (∈ permitted) . Chakra.toSequence
                $ Parity.getOf target chakra
    mRemoved <- R.choose (chakras :: [Chakra])
    case mRemoved of
        Just (Chakra.toChakras -> removed) -> do
            P.alter $ Game.adjustChakra target (- removed)
            return removed
        Nothing -> return 0

-- | Adds as many random 'Chakra's as the number of living 'N.Ninja's on the
-- player's team to the player's 'Game.chakra'.
gain :: ∀ m. (MonadGame m, MonadRandom m) => m ()
gain = do
    Game{playing} <- P.game
    let player = Player.opponent playing
    living  <- length . filter N.alive <$> P.allies player
    randoms <- replicateM living Chakra.random
    P.alter $ Game.adjustChakra player (+ Chakra.collect (randoms :: [Chakra]))
