{-# LANGUAGE NoStrictData #-}
-- | Messages passed between users through 
-- 'Control.Concurrent.STM.TChan.TChan's.
module Core.Message
  ( Game(..)
  , Queue(..)
  ) where

import ClassyPrelude.Yesod

import           Core.Model (User)
import           Model.Character (Character)
import qualified Model.Game as Game
import           Model.GameInfo (GameInfo)

-- | Messages between the two players of a game.
data Game 
    = Enact Game.Game
    | Forfeit

-- | Messages between all users queued for games.
data Queue
    = Announce (Key User) User [Character]
    | Respond  (Key User) (TChan Game) (TChan Game) GameInfo
