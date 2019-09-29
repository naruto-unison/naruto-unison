{-# OPTIONS_HADDOCK hide #-}

module Mission.Shippuden (missions) where

import           Mission.Goal (Mission)
import qualified Mission.Shippuden.Kids

missions :: [Mission]
missions = Mission.Shippuden.Kids.missions
