module Mission.MissionsSpec (spec) where

import Import

import qualified Game.Characters as Characters
import qualified Game.Model.Character as Character
import qualified Game.Model.Skill as Skill
import           Mission.Goal (Goal(..), Mission(..), Objective(..))
import qualified Mission.Missions as Missions
import           Util ((∈))

spec :: SpecWith ()
spec = traverse_ mission Missions.list

lookupChar :: Text -> (Character -> SpecWith ()) -> SpecWith ()
lookupChar name f = case Characters.lookup name of
    Nothing   -> describe (unpack name) $ it "exists in the database" False
    Just char -> describe (unpack $ Character.name char) $ f char

mission :: Mission -> SpecWith ()
mission Mission{char, goals} = do
    lookupChar char . const $ return ()
    traverse_ goal goals

goal :: Goal -> SpecWith ()
goal Reach{desc, objective} = describe (unpack desc) $ f objective
  where
    f (Consecutive name skills) = lookupChar name $ hasSkills skills
    f (HookAction name skill _) = lookupChar name $ hasSkills [skill]
    f (HookChakra name skill _) = lookupChar name $ hasSkills [skill]
    f (HookStore name skill _)  = lookupChar name $ hasSkills [skill]
    f (HookTrap name _ _)       = lookupChar name . const $ return ()
    f (HookTrigger name _ _)    = lookupChar name . const $ return ()
    f (HookTurn name _)         = lookupChar name . const $ return ()
    f (Win _ names) = traverse_ (`lookupChar` const (return ())) names

hasSkills :: [Text] -> Character -> SpecWith ()
hasSkills skills char = traverse_ hasSkill skills
  where
    allSkills  = Skill.name <$> join (Character.skills char)
    hasSkill :: Text -> SpecWith ()
    hasSkill x = it ("has [" ++ unpack x ++ "]") $ x ∈ allSkills
