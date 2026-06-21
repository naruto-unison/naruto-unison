module Game.Model.Game
    ( Game(..), new, newWithChakras
    , inProgress
    , setChakra, addChakra, removeChakra
    , setVendetta
    , swapPlaying
    , updateDna
    , incrementInactive, resetInactive
    , forfeit
    , setVictorBy
    ) where

import ClassyPrelude

import           Class.Parity (Parity)
import qualified Class.Parity as Parity
import           Class.Random (MonadRandom)
import           Game.Model.Chakras (Chakras)
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Character (Category(..))
import qualified Game.Model.Character as Character
import           Game.Model.Internal.Game (Game(..))
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Player (Player)
import qualified Game.Model.Player as Player
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Util ((∉))

new :: Game
new = Game
    { chakra    = (mempty, mempty)
    , playing   = Player.A
    , victor    = mempty
    , inactive  = (0, 0)
    , forfeited = False
    , dna       = (mempty, mempty)
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

updateDna :: Vector Ninja -> Game -> Game
updateDna ns game =
    game { dna = doUpdate Player.A $ doUpdate Player.B game.dna }
  where
    doUpdate player = Parity.modifyOf player $ calc $ Parity.half player ns
    calc :: Vector Ninja -> Seq Text -> Seq Text
    calc team dna = fromList (idents dead) ++ filter (∉ idents reanimated) dna
      where
        idents = (Character.ident . N.character <$>)
        reanimated :: [Ninja]
        reanimated = filter ((== Reanimated) . Character.category . N.character)
                   $ toList team
        dead = filter (not . N.alive) reanimated
