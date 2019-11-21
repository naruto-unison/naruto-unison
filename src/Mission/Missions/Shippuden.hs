{-# OPTIONS_HADDOCK hide #-}

module Mission.Missions.Shippuden (missions) where

import ClassyPrelude

import           Mission.Goal (Mission)
import qualified Mission.Missions.Shippuden.Kids
import qualified Mission.Missions.Shippuden.Adults
import qualified Mission.Missions.Shippuden.Leaders
import qualified Mission.Missions.Shippuden.Versions

missions :: [Mission]
missions = Mission.Missions.Shippuden.Kids.missions
        ++ Mission.Missions.Shippuden.Adults.missions
        ++ Mission.Missions.Shippuden.Leaders.missions
        ++ Mission.Missions.Shippuden.Versions.missions
