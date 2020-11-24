{-# LANGUAGE NoStrictData #-}

-- | Messages passed between users as cross-thread communication.
module Handler.Queue.Message
  ( Private(..)
  , Response(..)
  ) where

import ClassyPrelude

import Application.Model (Key, User)
import Game.Model.Character (Character)
import Handler.Play.Wrapper (Wrapper)
import Handler.Play.GameInfo (GameInfo)

-- | Messages between all users queued for games.
data Private
    = Request (Key User) (Key User) [Character]
    | Respond (Key User) Response

data Response = Response { mvar :: MVar Wrapper
                         , info :: GameInfo
                         }
