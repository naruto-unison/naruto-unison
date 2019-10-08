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
    cleanGoal goal = goal { objective = f $ objective goal }
    f (Win names) = Win $ Character.clean <$> names
    f (HookAction name skill fn) = HookAction (Character.clean name) skill fn
    f (HookChakra name skill fn) = HookChakra (Character.clean name) skill fn
    f (HookStore name skill fn)  = HookStore (Character.clean name) skill fn
    f (HookTrap name trap fn)    = HookTrap (Character.clean name) trap fn
    f (HookTurn name fn)         = HookTurn (Character.clean name) fn
    f (Consecutive x skills)     = Consecutive (Character.clean x) $ sort skills

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
