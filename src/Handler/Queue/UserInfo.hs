module Handler.Queue.UserInfo (UserInfo(..)) where

import ClassyPrelude

import Data.Time.Clock.System (SystemTime)

import Application.Model (User)
import Game.Model.Character (Character)
import Handler.Queue.Message (Response)

data UserInfo = UserInfo { user   :: User
                         , team   :: [Character]
                         , joined :: SystemTime
                         , chan   :: TChan Response
                         }
