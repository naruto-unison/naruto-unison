module Model.Game
  ( Game(..), new, newWithChakras
  , getChakra, setChakra, adjustChakra
  ) where

import ClassyPrelude

import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import qualified Class.Random as R
import           Class.Random (MonadRandom)
import           Model.Internal (Game(..))
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra, Chakras)
import qualified Model.Player as Player

new :: Game
new = Game { chakra  = (0, 0)
           , delays  = []
           , playing = Player.A
           , victor  = []
           -- , ninjas  = fromList $ zipWith Ninja.new ns Slot.all
           }

newWithChakras :: ∀ m. MonadRandom m => m Game
newWithChakras = do
    randoms :: [Chakra] <- replicateM 3 R.enum
    return $ adjustChakra Player.A (+ Chakra.collect randoms) new

getChakra :: ∀ a. Parity a => a -> Game -> Chakras
getChakra (Parity.even -> True) = fst . chakra
getChakra _                     = snd . chakra

setChakra :: ∀ a. Parity a => a -> Chakras -> Game -> Game
setChakra (Parity.even -> True) x game
                   = game { chakra = (x, snd $ chakra game) }
setChakra _ x game = game { chakra = (fst $ chakra game, x) }

adjustChakra :: ∀ a. Parity a => a -> (Chakras -> Chakras) -> Game -> Game
adjustChakra (Parity.even -> True) f game =
                        game { chakra = first  f $ chakra game }
adjustChakra _ f game = game { chakra = second f $ chakra game }
