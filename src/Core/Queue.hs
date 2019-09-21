{-# LANGUAGE NoStrictData   #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Messages passed between users through
-- 'Control.Concurrent.STM.TChan.TChan's.
module Core.Queue
  ( Message(..)
  , Section(..)
  , Failure(..)
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

import Core.Model (Key, User)
import Core.Wrapper (Wrapper)
import Model.Character (Character)
import Model.GameInfo (GameInfo)

-- | Messages between all users queued for games.
data Message
    = Announce (Key User) User [Character] (MVar UTCTime)
    | Request  (Key User) (Key User) [Character]
    | Respond  (Key User) (TBQueue Wrapper) (TBQueue Wrapper) GameInfo

data Section
    = Quick
    | Private

data Failure
    = AlreadyQueued
    | Canceled
    | InvalidTeam
    | OpponentNotFound
    deriving (Generic, ToJSON)
