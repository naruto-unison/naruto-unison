{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.VersionsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "One-Tailed Naruto" do
        useOn Enemy "Tailed Beast Rasengan" do
            it "deals bonus damage during Tailed Beast Chakra Arms" do
                Sim.use "Tailed Beast Chakra Arms"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth - 15 `shouldBe` 35 + 10
            it "deals less damage during Inner Chakra Mode" do
                Sim.use "Inner Chakra Mode"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 35 - 10

    describeCharacter "Curse Mark Sasuke" do
        useOn Enemy "Dark Void" do
            it "deals no damage initially" do
                Sim.act
                Sim.turns 1
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "deals damage at end" do
                Sim.act
                Sim.turns 3
                targetHealth <- target health
                100 - targetHealth `shouldBe` 55

        useOn Self "Curse Mark" do
            it "tags user" do
                Sim.act
                user has "Curse Mark"

    describeCharacter "Drunken Lee" do
        useOn REnemy "Unpredictable Assault" do
            it "damages target per Unpredictable Assault" do
                replicateM_ testStacks Sim.act
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 5 * testStacks

        useOn Enemy "Unpredictable Assault" do
            it "deals bonus damage during Drunken Fist" do
                Sim.use "Drunken Fist"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth - 15 `shouldBe` 20 + 5

        useOn Enemy "Drunken Counter" do
            it "counters on target" do
                targeting Self Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "damages with Unpredictable Assault if countered" do
                targeting Self Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "adds Unpredictable Assault if countered" do
                targeting Self Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user has "Unpredictable Assault"

    describeCharacter "Shukaku Gaara" do
        useOn Enemy "Monstrous Sand Arm" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "damages target until target acts" do
                Sim.act
                Sim.turns testStacks
                Sim.as Enemy $ apply Permanent [ Reveal ]
                Sim.turns 5
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10 * (testStacks + 1)

        useOn Self "Sand Transformation" do
            it "defends user" do
                Sim.act
                Sim.turns 6
                defense <- user totalDefense
                defense `shouldBe` 5 * 10
            it "alternates" do
                Sim.act
                Sim.turns 6
                user $ hasSkill "Shukaku Full Release"
            it "alternates other" do
                Sim.act
                Sim.turns 6
                user $ hasSkill "Wind Bullet"

        useOn Enemy "Shukaku Full Release" do
            it "strengthens user" do
                Sim.act
                damage testStacks
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * testStacks

    describeCharacter "Rehabilitated Gaara" do
        useOn Enemies "Sand Burial Prison" do
            it "exhausts targets" do
                Sim.act
                Sim.withClass Mental $ Sim.as XEnemies $ return ()
                targetExhausted <- Effects.exhaust [NonMental]
                                   <$> Sim.targets XEnemies
                targetExhausted `shouldBe` [Rand]
            it "ends if target uses non-mental" do
                Sim.act
                Sim.withClass NonMental $ Sim.as XEnemies $ return ()
                targetExhausted <- Effects.exhaust [NonMental] <$>
                                   Sim.targets XEnemies
                targetExhausted `shouldBe` []
            it "alternates" do
                Sim.act
                user $ hasSkill "Giant Sand Burial"
  where
    describeCharacter = describeCategory Original
    testStacks = 3
