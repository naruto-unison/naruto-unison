{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.TeachersSpec (spec) where

import Import

spec :: Spec
spec = parallel do
    describeCharacter "Iruka Umino" do
        useOn Enemy "Shuriken Throw" do
            it "deals bonus damage per health lost" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                self . damage $ 25 * stacks
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10 * stacks

        useOn Enemy "Capture and Arrest" do
            it "does not damage normally" do
                act
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "damages target if they harm" do
                act
                as Enemy $ apply Permanent [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 40
            it "makes target vulnerable if they harm" do
                act
                as Enemy $ apply Permanent [Reveal]
                setHealth 100
                withClass Physical $ damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 25

    describeCharacter "Mizuki" do
        useOn Enemy "Kunai Assault" do
            it "takes turns normally" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15
            it "damages instantly during Successful Ambush" do
                self $ tag' "Successful Ambush" Permanent
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30

        useOn Enemy "Execution Shuriken" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10
            it "deals bonus damage per target health lost" do
                damage $ 20 * 2
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 * 2 + 10 + 2 * 10
            it "deals bonus damage during Successful Ambush" do
                self $ tag' "Successful Ambush" Permanent
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 + 30

        useOn Self "Genjutsu Ambush Tactics" do
            it "does not make invulnerable instantly" do
                act
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser
            it "does not make invulnerable if harmed" do
                act
                as Enemy $ damage dmg
                turns 1
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser
            it "makes invulnerable if not harmed" do
                act
                as Enemy $ apply Permanent [Plague]
                turns 1
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser
            it "tags user if not harmed" do
                act
                turns 1
                hasOwn "Successful Ambush" <$> nUser

    describeCharacter "Anko Mitarashi" do
        useOn Enemy "Dual Pin" do
            it "alternates" do
                act
                hasSkill "Twin Snake Sacrifice" <$> nUser

        useOn Enemy "Twin Snake Sacrifice" do
            it "kills target" do
                use "Dual Pin"
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "kills user" do
                use "Dual Pin"
                self $ apply Permanent [Endure]
                act
                userHealth <- health <$> nUser
                userHealth `shouldBe` 0

    describeCharacter "Kakashi Hatake" do
        useOn Enemy "Lightning Blade" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 50
            it "kills if target has Summoning: Ninja Hounds" do
                use "Summoning: Ninja Hounds"
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0

    describeCharacter "Kurenai Yuhi" do
        useOn Enemy "Demonic Illusion: Entrap" do
            it "adds stacks" do
                replicateM_ stacks act
                userStacks <- numAnyStacks "Illusion" <$> nUser
                userStacks `shouldBe` 3

        useOn Self "Illusory Tree Meld" do
            it "adds destructible defense per Illusion" do
                self $ addStacks "Illusion" stacks
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 10 + 5 * stacks

        useOn Enemy "Demonic Illusion: Sylvan Fetters" do
            it "alternates" do
                act
                hasSkill "Sylvan Fetters Attack" <$> nUser

    describeCharacter "Asuma Sarutobi" do
        useOn Enemies "Flying Swallow" do
            it "alternates self" do
                act
                hasSkill "Finishing Blow" <$> nUser
            it "alternates other" do
                act
                hasSkill "Flying Kick" <$> nUser
            it "lasts longer per Sharpen Blades" do
                replicateM_ stacks $ use "Sharpen Blades"
                act
                turns $ 5 + stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` (2 + stacks) * 15

        useOn Ally "Self-Sacrifice" do
            it "redirects from ally" do
                act
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "redirects to user" do
                act
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser

    describeCharacter "Might Guy" do
        useOn Self "Sixth Gate Opening" do
            it "alternates" do
                act
                hasSkill "Severe Leaf Hurricane" <$> nUser

        useOn Enemy "Counter Punch" do
            it "counters target" do
                act
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered target" do
                act
                as Enemy $ apply Permanent [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30

    describeCharacter "Baki" do
        useOn Ally "Flak Jacket" do
            it "defends target" do
                act
                targetDefense <- totalDefense <$> nTarget
                targetDefense `shouldBe` 50
            it "protects target from effects" do
                as Enemy $ apply Permanent [Plague]
                act
                not . (`is` Plague) <$> nTarget
            it "ends when defense is destroyed" do
                as Enemy $ apply Permanent [Plague]
                act
                as Enemy demolishAll
                (`is` Plague) <$> nTarget
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
