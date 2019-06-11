module Model.Game
  ( Game(..), GameInfo(..)
  , getChakra
  , living
  ) where

import Prelude
import Data.Array ((!!))
import Data.Foldable (sum)
import Data.Int (even)
import Data.Maybe (fromMaybe)
import Generic as G

import Model (Ninja(..))
import Model.Character (Character)
import Model.Chakra (Chakras)
import Model.Slot (Slot)
import Model.User (User)

newtype Game = Game { chakra  :: Array Chakras
                    , ninjas  :: Array Ninja
                    , playing :: Int
                    , victor  :: Array Int
                    , targets :: Array (Array (Array Slot))
                    }

newtype GameInfo = GameInfo { opponent   :: User
                            , left       :: Int
                            , game       :: Game
                            , characters :: Array Character
                            , player     :: Int
                            }

living :: Slot -> Game -> Int
living p (Game g) = sum $ health <$> g.ninjas
  where
    health (Ninja n)
      | even n.slot == even p = min 1 n.health
      | otherwise             = 0

getChakra :: Game -> Int -> Chakras
getChakra (Game {chakra}) player = fromMaybe zero $ chakra !! player

derive instance _120_ :: G.Generic Game _
instance _121_ :: G.Decode Game where
    decode = G.decodeObj
derive instance _123_ :: G.Newtype Game _

derive instance _130_ :: G.Generic GameInfo _
instance _131_ :: G.Decode GameInfo where
    decode = G.decodeObj
derive instance _133_ :: G.Newtype GameInfo _
