{-# OPTIONS_HADDOCK hide, not-home #-}
module Game.Model.Internal.Game (Game(..)) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import Game.Model.Chakras (Chakras)
import Game.Model.Player (Player)
import Game.Model.Slot (Slot)

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
