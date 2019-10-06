module Mission.Missions
  ( list
  , map
  , characterMissions
  ) where

import ClassyPrelude hiding ((\\), map)

import           Mission.Goal (Goal, Mission)
import qualified Mission.Goal as Goal
import           Util (mapFromKeyed)

import qualified Mission.Missions.Shippuden

list :: [Mission]
list = Mission.Missions.Shippuden.missions
{-# NOINLINE list #-}

map :: HashMap Text (Seq Goal)
map = mapFromKeyed (Goal.char, Goal.goals) list
{-# NOINLINE map #-}

characterMissions :: Text -> [Mission]
characterMissions name = filter (any (Goal.belongsTo name) . Goal.goals) list
