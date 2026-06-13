{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.JinchurikiSpec (spec) where

import Import

import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Yugito Nii" do
        useOn Self "Two-Tailed Transformation" do
            it "tags user" do
                Sim.act
                user has "Two-Tailed Transformation"

        useOn Enemy "Flaming Cat Roar" do
            it "damages target per stack" do
                Sim.use "Two-Tailed Transformation"
                replicateM_ testStacks Sim.act
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30 + 5 * testStacks
            it "weakens target" do
                targeting Self $ tag Permanent "Two-Tailed Transformation"
                Sim.act
                setHealth 100
                damaged <- measureDamageTo Self $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 10

        useOn Enemy "Cat Claws" do
            it "damages target per stack" do
                Sim.use "Two-Tailed Transformation"
                replicateM_ testStacks Sim.act
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 15 + 5 * testStacks
            it "damages others per stack" do
                Sim.use "Two-Tailed Transformation"
                replicateM_ testStacks Sim.act
                targeting Everyone $ setHealth 100
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 5 + 5 * testStacks

    describeCharacter "Utakata" do
        useOn Enemy "Soap Bubble" do
            it "deals bonus damage if target has Drowning Bubble" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                apply Permanent skillName [ Invulnerable Affliction ]
                Sim.use "Drowning Bubble"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5

    describeCharacter "Killer B" do
        useOn Enemies "Lariat" do
            it "deals bonus damage during Acrobat" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Acrobat"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 20

        useOn Enemies "Octopus Hold" do
            it "counters on user" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [ Reveal ]
                not <$> target (`is` Reveal)
            it "damages countered" do
                Sim.act
                damaged <- measureDamage
                         $ Sim.as Enemy $ apply Permanent skillName [ Reveal ]
                damaged `shouldBe` 20

    describeCharacter "Eight-Tailed B" do
        useOn Self "Chakra Bones" do
            it "reduces damage" do
                replicateM_ 5 Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` dmg `quot` 2

    describeCharacter "Nine-Tailed Naruto" do
        useOn Self "Four-Tailed Transformation" do
            it "alternates A" do
                Sim.act
                user $ hasSkill "Six-Tailed Transformation"

        useOn Self "Six-Tailed Transformation" do
            it "alternates A" do
                Sim.act
                user $ hasSkill "Nine-Tailed Transformation"
            it "alternates B" do
                Sim.act
                user $ hasSkill "Mini Tailed Beast Bomb Barrage"
            it "alternates C" do
                Sim.act
                user $ hasSkill "Clasp"

        useOn Self "Nine-Tailed Transformation" do
            it "alternates A" do
                Sim.act
                user $ hasSkill "Raging Flames"
            it "alternates B" do
                Sim.act
                user $ hasSkill "Massive Tailed Beast Bomb"
            it "alternates C" do
                Sim.act
                user $ hasSkill "Chakra Gathering"
            it "does not kill user" do
                setHealth 2
                Sim.use "Four-Tailed Transformation"
                Sim.use "Six-Tailed Transformation"
                Sim.use "Nine-Tailed Transformation"
                userHealth <- user health
                userHealth `shouldBe` 1

        useOn Enemy "Mini Tailed Beast Bomb Barrage" do
            it "normally deals damage over time" do
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 10
            it "normally takes 3 turns" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.turns 4
                damaged `shouldBe` 3 * 10
            it "deals damage instantly if target has Clasp" do
                Sim.use "Four-Tailed Transformation"
                Sim.use "Six-Tailed Transformation"
                Sim.use "Clasp"
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30

        useOn Enemy "Massive Tailed Beast Bomb" do
            it "is normally single-target" do
                Sim.use "Four-Tailed Transformation"
                Sim.use "Six-Tailed Transformation"
                Sim.use "Nine-Tailed Transformation"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 0
            it "targets all during Chakra Gathering" do
                Sim.use "Four-Tailed Transformation"
                Sim.use "Six-Tailed Transformation"
                Sim.use "Nine-Tailed Transformation"
                Sim.use "Chakra Gathering"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldNotBe` 0


  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    testStacks = 3
