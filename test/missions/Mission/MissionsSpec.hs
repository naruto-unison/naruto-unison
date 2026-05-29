module Mission.MissionsSpec (spec) where

import ClassyPrelude

import Test.Hspec

import qualified Game.Characters as Characters
import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import qualified Game.Model.Skill as Skill
import           Mission.Goal (Goal(..), Mission(..))
import           Mission.Objective (Objective(..))
import qualified Mission.Missions as Missions
import           Util ((∈))

spec :: SpecWith ()
spec = traverse_ mission Missions.list

lookupChar :: Text -> (Character -> SpecWith ()) -> SpecWith ()
lookupChar name f = case Characters.lookup name of
    Nothing   -> describe (unpack name) $ it "exists in the database" False
    Just char -> describe (unpack $ Character.name char) $ f char

defaultPredicate :: Character -> SpecWith ()
defaultPredicate = const $ return ()

mission :: Mission -> SpecWith ()
mission Mission{char, goals} = do
    lookupChar char defaultPredicate
    traverse_ goal goals

goal :: Goal -> SpecWith ()
goal Reach{desc, objective} = describe (unpack desc) $ f objective
  where
    f (Consecutive ident skills) = lookupChar ident $ hasSkills skills
    f (HookAction ident skill _) = lookupChar ident $ hasSkills [skill]
    f (HookChakra ident skill _) = lookupChar ident $ hasSkills [skill]
    f (HookStore ident skill _)  = lookupChar ident $ hasSkills [skill]
    f (HookTrap ident _ _)       = lookupChar ident defaultPredicate
    f (HookTrigger ident _ _)    = lookupChar ident defaultPredicate
    f (HookTurn ident _)         = lookupChar ident defaultPredicate
    f (Win _ idents) = traverse_ (`lookupChar` defaultPredicate) idents

hasSkills :: [Text] -> Character -> SpecWith ()
hasSkills skills char = traverse_ hasSkill skills
  where
    allSkills  = Skill.name <$> join (Character.skills char)
    hasSkill :: Text -> SpecWith ()
    hasSkill x = it ("has [" ++ unpack x ++ "]") $ x ∈ allSkills
