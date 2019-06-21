module Model.Game
  ( Game(..), new
  , ninja, setNinja
  , adjust
  , alter
  , getChakra, setChakra, adjustChakra, gainChakra
  , yieldVictor, forfeit
  , zipNinjasWith
  ) where

import ClassyPrelude

import qualified Data.Sequence as Seq

import qualified Class.Parity as Parity
import           Class.Parity (Parity)
import           Core.Util (enumerate)
import           Model.Internal (Game(..))
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra, Chakras)
import           Model.Character (Character)
import qualified Model.Player as Player
import           Model.Player (Player)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Slot as Slot
import           Model.Slot (Slot)

new :: [Character] -> Game
new ns = Game { ninjas  = fromList $ zipWith Ninja.new ns Slot.all
              , chakra  = (0, 0)
              , delays  = []
              , playing = Player.A
              , victor  = []
              }

ninja :: Slot -> Game -> Ninja
ninja slot game = ninjas game `Seq.index` Slot.toInt slot

setNinja :: Slot -> Ninja -> Game -> Game
setNinja slot n game =
    game { ninjas = Seq.update (Slot.toInt slot) n $ ninjas game }

alter :: (Seq Ninja -> Seq Ninja) -> Game -> Game
alter f game = game { ninjas = f $ ninjas game }

adjust :: Slot -> (Ninja -> Ninja) -> Game -> Game
adjust i f game = game { ninjas = Seq.adjust' f (Slot.toInt i) $ ninjas game }

-- | Adds 1 random chakra per living 'Ninja' on the team of a 'Player'.
gainChakra :: Player -> [Chakra] -> Game -> Game
gainChakra player gain game = adjustChakra player (+ Chakra.collect living) game
  where
    living = take (length . filter (Ninja.playing player) $ ninjas game) gain

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

yieldVictor :: Game -> Game
yieldVictor game
  | not . null $ victor game = game
  | otherwise = game { victor = filter (dead game . Player.opponent) enumerate }

forfeit :: Player -> Game -> Game
forfeit player game
  | not . null $ victor game = game
  | otherwise                = game { victor = [Player.opponent player]
                                    , ninjas = f <$> ninjas game
                                    }
  where
    f n
      | Parity.allied player $ Ninja.slot n = n { Ninja.health = 0 }
      | otherwise                           = n

-- | The entire team of a 'Player' is dead, resulting in defeat.
dead :: Game -> Player -> Bool
dead game player = not . any (Ninja.playing player) $ ninjas game

zipNinjasWith :: ∀ a. (Ninja -> Ninja -> a) -> Game -> Game -> Seq a
zipNinjasWith f game game' = zipWith f (ninjas game) (ninjas game')
