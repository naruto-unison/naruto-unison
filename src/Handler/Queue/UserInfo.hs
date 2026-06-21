module Handler.Queue.UserInfo (UserInfo(..)) where

import ClassyPrelude

import Database.Persist (Entity)
import Data.Time.Clock.System (SystemTime)

import Application.Model.User (User)
import Game.Model.Character (Character)
import Handler.Queue.Message (Response)

data UserInfo = UserInfo
    { user   :: Entity User
    , team   :: [Character]
    , dna    :: Seq Text
    , joined :: SystemTime
    , chan   :: MVar Response
    }
