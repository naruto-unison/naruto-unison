{-# OPTIONS_HADDOCK hide #-}

module Mission.Missions.Shippuden (missions) where

import           Mission.Goal (Mission)
import qualified Mission.Missions.Shippuden.Kids

missions :: [Mission]
missions = Mission.Missions.Shippuden.Kids.missions
