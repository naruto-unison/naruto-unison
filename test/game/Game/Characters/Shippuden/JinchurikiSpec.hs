{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.JinchurikiSpec (spec) where

import Import

spec :: Spec
spec = parallel do
    describeCharacter "Yugito Nii" do
        useOn Self "Two-Tailed Transformation" do
            it "tags user" do
                act
                hasOwn "Two-Tailed Transformation" <$> nUser

        useOn Enemy "Flaming Cat Roar" do
            it "damages target per stack" do
                use "Two-Tailed Transformation"
                replicateM_ stacks act
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 + 5 * stacks
            it "weakens target" do
                self $ tag' "Two-Tailed Transformation" Permanent
                act
                setHealth 100
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 10

        useOn Enemy "Cat Claws" do
            it "damages target per stack" do
                use "Two-Tailed Transformation"
                replicateM_ stacks act
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 5 * stacks
            it "damages others per stack" do
                use "Two-Tailed Transformation"
                replicateM_ stacks act
                everyone $ setHealth 100
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 5 + 5 * stacks

    describeCharacter "Utakata" do
        useOn Enemy "Soap Bubble" do
            it "deals bonus damage if target has Drowning Bubble" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply Permanent [Invulnerable Affliction]
                use "Drowning Bubble"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Killer B" do
        useOn Enemies "Lariat" do
            it "deals bonus damage during Acrobat" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Acrobat"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20

        useOn Enemies "Octopus Hold" do
            it "counters on user" do
                act
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "damages countered" do
                act
                as Enemy $ apply Permanent [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20

    describeCharacter "Eight-Tailed B" do
        useOn Self "Chakra Bones" do
            it "reduces damage" do
                replicateM_ 5 act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` dmg `quot` 2

    describeCharacter "Nine-Tailed Naruto" do
        useOn Self "Four-Tailed Transformation" do
            it "alternates A" do
                act
                hasSkill "Six-Tailed Transformation" <$> nUser

        useOn Self "Six-Tailed Transformation" do
            it "alternates A" do
                act
                hasSkill "Nine-Tailed Transformation" <$> nUser
            it "alternates B" do
                act
                hasSkill "Mini Tailed Beast Bomb Barrage" <$> nUser
            it "alternates C" do
                act
                hasSkill "Clasp" <$> nUser

        useOn Self "Nine-Tailed Transformation" do
            it "alternates A" do
                act
                hasSkill "Raging Flames" <$> nUser
            it "alternates B" do
                act
                hasSkill "Massive Tailed Beast Bomb" <$> nUser
            it "alternates C" do
                act
                hasSkill "Chakra Gathering" <$> nUser
            it "does not kill user" do
                setHealth 2
                use "Four-Tailed Transformation"
                use "Six-Tailed Transformation"
                use "Nine-Tailed Transformation"
                userHealth <- health <$> nUser
                userHealth `shouldBe` 1

        useOn Enemy "Mini Tailed Beast Bomb Barrage" do
            it "normally deals damage over time" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10
            it "normally takes 3 turns" do
                act
                turns 4
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * 10
            it "deals damage instantly if target has Clasp" do
                use "Four-Tailed Transformation"
                use "Six-Tailed Transformation"
                use "Clasp"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30

        useOn Enemy "Massive Tailed Beast Bomb" do
            it "is normally single-target" do
                use "Four-Tailed Transformation"
                use "Six-Tailed Transformation"
                use "Nine-Tailed Transformation"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 0
            it "targets all during Chakra Gathering" do
                use "Four-Tailed Transformation"
                use "Six-Tailed Transformation"
                use "Nine-Tailed Transformation"
                use "Chakra Gathering"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldNotBe` 0


  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    stacks = 3
