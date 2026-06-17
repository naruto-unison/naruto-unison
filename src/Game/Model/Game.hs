module Game.Model.Game
    ( Game(..), new, newWithChakras
    , inProgress
    , setChakra, addChakra, removeChakra
    , setVendetta
    , swapPlaying
    , incrementInactive, resetInactive
    , forfeit
    , setVictorBy
    ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

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
    { chakra    :: (Chakras, Chakras)
    -- ^ Starts at @('Chakras' 0 0 0 0 0, 'Chakras' 0 0 0 0 0)@
    , playing   :: Player
    -- ^ Starts at 'Player.A'.
    , victor    :: EnumSet Player
    -- ^ Starts empty.
    , inactive  :: (Int, Int)
    -- ^ Starts at @(0, 0)@.
    , forfeited :: Bool
    -- ^ Starts at @False@.
    , vendetta  :: Maybe Slot
    -- ^ Used by AI.
    } deriving (Eq, Show)

new :: Game
new = Game
    { chakra    = (mempty, mempty)
    , playing   = Player.A
    , victor    = mempty
    , inactive  = (0, 0)
    , forfeited = False
    , vendetta  = Nothing
    }

newWithChakras :: ∀ m. MonadRandom m => m Game
newWithChakras = do
    randsA <- Chakras.random 1
    randsB <- Chakras.random Slot.teamSize
    return new { chakra = (randsA, randsB) }

-- | The game has not yet ended.
inProgress :: Game -> Bool
inProgress x = null x.victor

setChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
setChakra p x game = game { chakra = Parity.setOf p x game.chakra }

adjustChakra :: ∀ a. Parity a => a -> (Chakras -> Chakras) -> Game -> Game
adjustChakra p f game = game { chakra = Parity.modifyOf p f game.chakra }

addChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
addChakra p chakras game = adjustChakra p (++ chakras) game

removeChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
removeChakra p chakras game = adjustChakra p (Chakras.spend chakras) game

setVendetta :: Maybe Slot -> Game -> Game
setVendetta vendetta game = game { vendetta = vendetta }

swapPlaying :: Game -> Game
swapPlaying game = game { playing = Player.opponent game.playing }

incrementInactive :: Player -> Game -> Game
incrementInactive player game =
    game { inactive = Parity.modifyOf player (+1) game.inactive }

resetInactive :: Player -> Game -> Game
resetInactive player game =
    game { inactive = Parity.setOf player 0 game.inactive }

forfeit :: Player -> Game -> Game
forfeit player game = game { victor    = singleton $ Player.opponent player
                           , forfeited = True
                           }

setVictorBy :: (Player -> Bool) -> Game -> Game
setVictorBy isVictor game =
    game { victor = setFromList $ filter isVictor [Player.A, Player.B] }
