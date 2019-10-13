{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.OrganizationsSpec (spec) where

import TestImport

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "Izumo and Kotetsu" \useOn -> do
        useOn Enemy "Mace Crush" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Syrup Trap" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage if target has Syrup Trap" $
                    targetHealth - targetHealth' `shouldBe` 10
        {-
        useOn Enemies "Syrup Trap" do
            act
            withClass Mental $ enemyTurn $ return ()
            targetStunned <- Effects.stun <$> P.nTarget
            factory
            act -}
  where
    describeCharacter = describeCategory Original
    -- targetDmg = 55
    -- stacks = 3
    -- healthInterval = 2
