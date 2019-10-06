module Mission.MissionsSpec (spec) where

import TestImport

import qualified Game.Characters as Characters
import qualified Game.Model.Character as Character
import qualified Game.Model.Skill as Skill
import           Mission.Goal (Goal(..), Mission(..), Objective(..))
import qualified Mission.Missions as Missions

spec :: SpecWith ()
spec = traverse_ describeMission Missions.list

lookupChar :: Text -> (Character -> SpecWith ()) -> SpecWith ()
lookupChar name f = case Characters.lookup name of
    Nothing   -> describe (unpack name) $ it "Exists in the database" False
    Just char -> describe (unpack $ Character.name char) $ f char

describeMission :: Mission -> SpecWith ()
describeMission Mission{char, goals} = do
    lookupChar char . const $ return ()
    traverse_ describeGoal goals

describeGoal :: Goal -> SpecWith ()
describeGoal Reach{desc, objective} = describe (unpack desc) $
    describeObjective objective

describeObjective :: Objective -> SpecWith ()
describeObjective (Win names) = traverse_ (`lookupChar` const (return ())) names
describeObjective (Hook name skill _) = lookupChar name $
    it ("Has [" ++ unpack skill ++ "]") . any ((== skill) . Skill.name) .
    join . Character.skills
describeObjective (HookTurn name _) = lookupChar name . const $ return ()
describeObjective (UseAllSkills name) = lookupChar name . const $ return ()
