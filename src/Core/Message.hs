{-# LANGUAGE NoStrictData #-}
module Core.Message
  ( Game(..)
  , Queue(..)
  ) where

import ClassyPrelude.Yesod

import           Core.Model (User)
import           Model.Character (Character)
import qualified Model.Game as Game
import           Model.GameInfo (GameInfo)

newtype Game = Enact Game.Game

data Queue
    = Announce (Key User) User [Character]
    | Respond  (Key User) (TChan Game) (TChan Game) GameInfo
