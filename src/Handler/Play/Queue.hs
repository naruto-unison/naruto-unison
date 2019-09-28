{-# LANGUAGE NoStrictData   #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Messages passed between users as cross-thread communication.
module Handler.Play.Queue
  ( Message(..)
  , Section(..)
  , Failure(..)
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Core.Model (Key, User)
import Handler.Play.Wrapper (Wrapper)
import Game.Model.Character (Character)
import Handler.Play.GameInfo (GameInfo)

-- | Messages between all users queued for games.
data Message
    = Announce (Key User) User [Character] (MVar UTCTime)
    | Request  (Key User) (Key User) [Character]
    | Respond  (Key User) (MVar Wrapper) GameInfo

data Section
    = Quick
    | Private

data Failure
    = AlreadyQueued
    | Canceled
    | InvalidTeam
    | OpponentNotFound
    deriving (Generic, ToJSON)
