module Game.Model.Game
  ( Game(..), new, newWithChakras
  , setChakra, adjustChakra
  ) where

import ClassyPrelude

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Chakra (Chakras)
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import qualified Game.Model.Slot as Slot

-- | Game state.
data Game = Game { chakra   :: (Chakras, Chakras)
                 -- ^ Starts at @('Chakras' 0 0 0 0 0, 'Chakras' 0 0 0 0 0)@
                 , playing  :: Player
                 -- ^ Starts at 'Player.A'.
                 , victor   :: [Player]
                 -- ^ Starts empty.
                 , inactive :: (Int, Int)
                 -- ^ Starts at @(0, 0)@.
                 , forfeit  :: Bool
                 -- ^ Starts at @False@.
                 } deriving (Eq, Show, Read)

new :: Game
new = Game { chakra   = (0, 0)
           , playing  = Player.A
           , victor   = []
           , inactive = (0, 0)
           , forfeit  = False
           }

newWithChakras :: ∀ m. MonadRandom m => m Game
newWithChakras = do
    randomA  <- Chakra.random
    randomsB <- replicateM @[_] Slot.teamSize Chakra.random
    return
        new { chakra = (Chakra.toChakras randomA, Chakra.collect randomsB) }

setChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
setChakra p x game = game { chakra = Parity.setOf p x $ chakra game }

adjustChakra :: ∀ a. Parity a => a -> (Chakras -> Chakras) -> Game -> Game
adjustChakra p f game = game { chakra = Parity.modifyOf p f $ chakra game }
