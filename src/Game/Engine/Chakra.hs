-- | 'Game.chakra' processing.
module Game.Engine.Chakra
  ( remove, remove1
  , gain
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import qualified Class.Parity as Parity
import           Class.Play (MonadGame, MonadPlay)
import qualified Class.Play as P
import           Class.Random (MonadRandom)
import qualified Class.Random as R
import           Game.Model.Chakras (Chakra(..), Chakras)
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Game (Game(Game))
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as N
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot
import           Game.Model.Trigger (Trigger(..))
import           Util ((∈))

-- | Removes some number of 'Chakra's from the opponent's team.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra'.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove :: ∀ m. (MonadPlay m, MonadRandom m) => Int -> m Chakras
remove amount
  | amount <= 0 = return mempty
  | otherwise = do
        Context{user} <- P.context
        P.trigger user [OnChakra]
        let opponent = 1 + Slot.toInt user
        Game{chakra} <- P.game
        let chakras = fromList . toList . removeRandoms
                    $ Parity.getOf opponent chakra
        removed <- fromList . toList . take amount <$> R.shuffle chakras
        P.alter $ Game.removeChakra opponent removed
        return removed
  where
    removeRandoms x = x { Chakras.rand = 0 }

-- | Removes a single 'Chakra' from the enemy team that is one of several types.
-- 'Chakra's are chosen randomly from the available pool of 'Game.chakra', but
-- only the ones passed in the parameter.
-- Removed 'Chakra's are collected into a 'Chakras' object and returned.
remove1 :: ∀ m. (MonadPlay m, MonadRandom m) => EnumSet Chakra -> m Chakras
remove1 permitted = do
    Context{target, user} <- P.context
    P.trigger user [OnChakra]
    Game{chakra} <- P.game
    let chakras = filter (∈ permitted) . toList $ Parity.getOf target chakra
    mRemoved <- R.choose chakras
    case mRemoved of
        Just (singleton -> removed) -> do
            P.alter $ Game.removeChakra target removed
            return removed
        Nothing -> return mempty

-- | Adds as many random 'Chakra's as the number of living 'N.Ninja's on the
-- player's team to the player's 'Game.chakra'.
gain :: ∀ m. (MonadGame m, MonadRandom m) => m ()
gain = do
    Game{playing} <- P.game
    let player = Player.opponent playing
    living  <- length . filter N.alive <$> P.allies player
    randoms <- replicateM living Chakras.random
    P.alter $ Game.addChakra player $ fromList randoms
