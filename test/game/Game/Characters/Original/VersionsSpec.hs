{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.VersionsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "One-Tailed Naruto" do
        useOn Enemy "Tailed Beast Rasengan" do
            it "deals bonus damage during Tailed Beast Chakra Arms" do
                apply 0 [AntiChannel]
                use "Tailed Beast Chakra Arms"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 35 + 10
            it "deals less damage during Inner Chakra Mode" do
                use "Inner Chakra Mode"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 35 - 10

    describeCharacter "Curse Mark Sasuke" do
        useOn Enemy "Dark Void" do
            it "deals no damage initially" do
                act
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "deals damage at end" do
                act
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 55

        useOn Self "Curse Mark" do
            it "tags user" do
                act
                hasOwn "Curse Mark" <$> nUser

    describeCharacter "Drunken Lee" do
        useOn Enemy "Unpredictable Assault" do
            it "damages target per Unpredictable Assault" do
                replicateM_ stacks act
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 5 * stacks
            it "deals bonus damage during Drunken Fist" do
                apply 0 [AntiChannel]
                use "Drunken Fist"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 5

        useOn Enemy "Drunken Counter" do
            it "counters on target" do
                self act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages with Unpredictable Assault if countered" do
                self act
                as Enemy $ apply 0 [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "adds Unpredictable Assault if countered" do
                self act
                as Enemy $ apply 0 [Reveal]
                hasOwn "Unpredictable Assault" <$> nUser

    describeCharacter "Shukaku Gaara" do
        useOn Enemy "Monstrous Sand Arm" do
            it "counters target" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages target until target acts" do
                act
                turns stacks
                as Enemy $ apply 0 [Reveal]
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 * (stacks + 1)

        useOn Self "Sand Transformation" do
            it "defends user" do
                act
                turns 6
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 5 * 10
            it "alternates" do
                act
                turns 6
                hasSkill "Shukaku Full Release" <$> nUser
            it "alternates other" do
                act
                turns 6
                hasSkill "Wind Bullet" <$> nUser

        useOn Enemy "Shukaku Full Release" do
            it "strengthens user" do
                act
                damage stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * stacks

    describeCharacter "Rehabilitated Gaara" do
        useOn Enemies "Sand Burial Prison" do
            it "exhausts targets" do
                act
                withClass Mental $ as XEnemies $ return ()
                targetExhausted <- Effects.exhaust [NonMental] <$> get XEnemies
                targetExhausted `shouldBe` [Rand]
            it "ends if target uses non-mental" do
                act
                withClass NonMental $ as XEnemies $ return ()
                targetExhausted <- Effects.exhaust [NonMental] <$> get XEnemies
                targetExhausted `shouldBe` []
            it "alternates" do
                act
                hasSkill "Giant Sand Burial" <$> nUser
  where
    describeCharacter = describeCategory Original
    stacks = 3
