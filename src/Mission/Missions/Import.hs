{-# OPTIONS_HADDOCK prune #-}

-- Base import for modules in @Mission.Missions@.
module Mission.Missions.Import
  ( module Import
  , win, winConsecutive
  ) where

import ClassyPrelude as Import

import Class.Parity as Import (allied)
import Data.IntSet as Import (size)
import Game.Engine.Effects as Import (isStunned)
import Game.Model.Ninja as Import (alive, health)
import Game.Model.Trigger as Import (Trigger(..))
import Mission.Goal as Import
import Mission.Hooks.Action as Import
import Mission.Hooks.Chakra as Import
import Mission.Hooks.Store as Import
import Mission.Hooks.Trap as Import
import Mission.Hooks.Turn as Import
import Mission.Objective as Import
import Mission.Hooks.Util as Import (hasOwn, used)

import Class.Display (Display(..), buildStrict, commas)

winFull :: WinType -> Int -> [Text] -> Goal
winFull winType reach chars = Reach Career reach desc $ Win winType chars
  where
    desc = buildStrict
        $ "Win " ++ display reach ++ " matches with "
        ++ commas "and" (display <$> chars) ++ " together."

-- | 'WinTotal' objective, given a goal and a team of 'Character's.
win :: Int -> [Text] -> Goal
win = winFull WinTotal

-- 'WinConsecutive' objective, given a goal and a team of 'Character's.
winConsecutive :: Int -> [Text] -> Goal
winConsecutive = winFull WinConsecutive
