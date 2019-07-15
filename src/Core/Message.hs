{-# LANGUAGE NoStrictData #-}
-- | Messages passed between users through
-- 'Control.Concurrent.STM.TChan.TChan's.
module Core.Message
  ( Game(..)
  , Queue(..)
  ) where

import ClassyPrelude

import Core.Model (Key, User)
import Core.Wrapper (Wrapper)
import Model.Character (Character)
import Model.GameInfo (GameInfo)

-- | Messages between the two players of a game.
data Game
    = Enact Wrapper
    | Forfeit

-- | Messages between all users queued for games.
data Queue
    = Announce (Key User) User [Character]
    | Respond  (Key User) (TBQueue Game) (TBQueue Game) GameInfo
