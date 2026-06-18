module Handler.Queue.UserInfo (UserInfo(..)) where

import Control.Concurrent.MVar (MVar)
import Database.Persist (Entity)
import Data.Time.Clock.System (SystemTime)

import Application.Model.User (User)
import Game.Model.Character (Character)
import Handler.Queue.Message (Response)

data UserInfo = UserInfo
    { user   :: Entity User
    , team   :: [Character]
    , joined :: SystemTime
    , chan   :: MVar Response
    }
