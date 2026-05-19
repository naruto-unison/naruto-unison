module Game.Model.Game
  ( Game(..), new, newWithChakras
  , setChakra, addChakra, removeChakra
  , inProgress
  ) where

import ClassyPrelude

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Chakras (Chakras)
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot

-- | Game state.
data Game = Game
    { chakra   :: (Chakras, Chakras)
    -- ^ Starts at @('Chakras' 0 0 0 0 0, 'Chakras' 0 0 0 0 0)@
    , playing  :: Player
    -- ^ Starts at 'Player.A'.
    , victor   :: [Player]
    -- ^ Starts empty.
    , inactive :: (Int, Int)
    -- ^ Starts at @(0, 0)@.
    , forfeit  :: Bool
    -- ^ Starts at @False@.
    , vendetta  :: Maybe Slot
    -- ^ Used by AI.
    } deriving (Eq, Show, Read)

new :: Game
new = Game
    { chakra   = (mempty, mempty)
    , playing  = Player.A
    , victor   = []
    , inactive = (0, 0)
    , forfeit  = False
    , vendetta = Nothing
    }

newWithChakras :: ∀ m. MonadRandom m => m Game
newWithChakras = do
    randA  <- Chakras.random
    randsB <- replicateM Slot.teamSize Chakras.random
    return new { chakra = (singleton randA, fromList randsB) }

setChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
setChakra p x game = game { chakra = Parity.setOf p x $ chakra game }

adjustChakra :: ∀ a. Parity a => a -> (Chakras -> Chakras) -> Game -> Game
adjustChakra p f game = game { chakra = Parity.modifyOf p f $ chakra game }

addChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
addChakra p chakras game = adjustChakra p (++ chakras) game

removeChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
removeChakra p chakras game = adjustChakra p (Chakras.spend chakras) game

-- | The game has not yet ended.
inProgress :: Game -> Bool
inProgress x = null $ victor x
