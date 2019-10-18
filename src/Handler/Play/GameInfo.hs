module Handler.Play.GameInfo (GameInfo(..)) where

import Data.Aeson ((.=), ToJSON(..), object)

import           Application.Model (Key, User)
import           Game.Model.Game (Game(..))
import           Game.Model.Ninja (Ninja)
import           Game.Model.Player (Player)
import qualified Handler.Play.Turn as Turn

-- | Game state details. Sent at the start of games and during practice games.
data GameInfo = GameInfo { vsWho  :: Key User
                         , vsUser :: User
                         , game   :: Game
                         , ninjas :: [Ninja]
                         , player :: Player
                         }

instance ToJSON GameInfo where
    toJSON GameInfo{..} = object
        [ "opponent" .= vsUser
        , "player"   .= player
        , "turn"     .= Turn.new player ninjas game
        ]
