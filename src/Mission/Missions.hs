module Mission.Missions
  ( list
  , map
  , characterMissions
  ) where

import ClassyPrelude hiding ((\\), map)

import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import           Mission.Goal (Goal(..), Mission(..), Objective(..))
import qualified Mission.Goal as Goal
import           Util (mapFromKeyed)

import qualified Mission.Missions.Shippuden

clean :: Mission -> Mission
clean (Mission char goals) =
    Mission (Character.clean char) $ cleanGoal <$> goals
  where
    cleanGoal goal = goal { objective = cleanObjective $ objective goal }
    cleanObjective (Win names) = Win $ Character.clean <$> names
    cleanObjective (Hook name skill fn) = Hook (Character.clean name) skill fn
    cleanObjective (HookTurn name fn) = HookTurn (Character.clean name) fn
    cleanObjective (UseAllSkills x) = UseAllSkills $ Character.clean x

list :: [Mission]
list = clean
       <$> Mission.Missions.Shippuden.missions
{-# NOINLINE list #-}

map :: HashMap Text (Seq Goal)
map = mapFromKeyed (Goal.char, Goal.goals) list
{-# NOINLINE map #-}

characterMissions :: Character -> [Mission]
characterMissions (Character.ident -> name) =
    filter (any (Goal.belongsTo name) . Goal.goals) list
