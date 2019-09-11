module Model.Game
  ( Game(..), new, newWithChakras
  , setChakra, adjustChakra
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import           Class.Random (MonadRandom)
import           Model.Internal (Game(..))
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra, Chakras)
import qualified Model.Player as Player
import qualified Model.Slot as Slot

new :: Game
new = Game { chakra  = (0, 0)
           , delays  = []
           , playing = Player.A
           , victor  = []
           }

newWithChakras :: ∀ m. MonadRandom m => m Game
newWithChakras = do
    randoms :: [Chakra] <- replicateM Slot.teamSize Chakra.random
    return $ adjustChakra Player.A (+ Chakra.collect randoms) new

setChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
setChakra (Parity.even -> True) x game
                   = game { chakra = (x, snd $ chakra game) }
setChakra _ x game = game { chakra = (fst $ chakra game, x) }

adjustChakra :: ∀ a. Parity a => a -> (Chakras -> Chakras) -> Game -> Game
adjustChakra (Parity.even -> True) f game =
                        game { chakra = first  f $ chakra game }
adjustChakra _ f game = game { chakra = second f $ chakra game }
