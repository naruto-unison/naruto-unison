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

import Application.Model (Key, User)
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
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Failure
    = AlreadyQueued
    | Canceled
    | InvalidTeam
    | Locked
    | NotFound
    deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic, ToJSON)
