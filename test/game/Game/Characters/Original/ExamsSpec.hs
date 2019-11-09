{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.ExamsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Hanabi Hyūga" do
        useOn Enemy "Gentle Fist" do
            it "depletes chakra upon drain" do
                gain $ replicate 5 Blood
                act
                as Enemy $ deplete 1
                as Enemy $ deplete 2
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Blood, Blood, Blood])

    describeCharacter "Shigure" do
        useOn Self "Umbrella Toss" do
            it "adds stacks" do
                act
                userStacks <- numAnyStacks "Umbrella" <$> nUser
                userStacks `shouldBe` 4

        useOn Self "Umbrella Gathering" do
            it "reduces damage per Umbrella" do
                addStacks "Umbrella" stacks
                act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 10 * stacks

        useOn Enemies "Senbon Shower" do
            it "damages enemies" do
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 15
            it "spends an Umbrella" do
                self $ addStacks "Umbrella" stacks
                act
                userStacks <- numAnyStacks "Umbrella" <$> nUser
                userStacks `shouldBe` stacks - 1

        useOn Enemy "Senbon Barrage" do
            it "damages enemy per Umbrella" do
                self $ addStacks "Umbrella" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 * stacks
            it "spends all Umbrellas" do
                self $ addStacks "Umbrella" stacks
                act
                not . hasOwn "Umbrella" <$> nUser

    describeCharacter "Oboro" do
        useOn Enemy "Underground Move" do
            it "is normally single-target" do
                act
                targetHealth <- health <$> get XEnemies
                targetHealth `shouldBe` 100
            it "affects all enemies during Fog Clone" do
                use "Fog Clone"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 20

    describeCharacter "Kabuto Yakushi" do
        useOn Enemies "Temple of Nirvana" do
            it "does not stun immediately" do
                act
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` []
            it "stuns on inactive" do
                act
                turns 1
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` [All]
            it "does not stun active" do
                act
                as Enemy $ return ()
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` [All]

    describeCharacter "Dosu Kinuta" do
        useOn Enemy "Resonating Echo Drill" do
            it "deals bonus damage during Echo Speaker Tuning" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Echo Speaker Tuning"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20

        useOn Enemy "Sound Manipulation" do
            it "deals bonus damage if target has Resonating Echo Drill" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Resonating Echo Drill"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage during Echo Speaker Tuning" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Echo Speaker Tuning"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Kin Tsuchi" do
        useOn Enemy "Bell Ring Illusion" do
            it "deals bonus damage during Unnerving Bells" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Unnerving Bells"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 25
            it "does not make user invulnerable normally" do
                act
                as Enemy $ apply 0 [Reveal]
                (`is` Reveal) <$> nUser
            it "makes user invulnerable during Shadow Senbon" do
                use "Shadow Senbon"
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser

        useOn Enemy "Shadow Senbon" do
            it "does not stun normally" do
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "stuns during Unnerving Bells" do
                use "Unnerving Bells"
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not deal damage normally" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "deals damage during Bell Ring Illusion" do
                use "Bell Ring Illusion"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Unnerving Bells" do
            it "does not make target vulnerable normally" do
                act
                withClass Physical $ as Self $ damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 0
            it "makes target vulnerable during Bell Ring Illusion" do
                use "Bell Ring Illusion"
                setHealth 100
                act
                withClass Chakra $ damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 15
            it "makes target vulnerable during Shadow Senbon" do
                use "Shadow Senbon"
                act
                withClass Physical $ damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 15

    describeCharacter "Yoroi Akadō" do
        useOn Enemy "Energy Drain" do
            it "steals health" do
                as Enemy $ damage dmg
                act
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 20
            it "steals chakra during Chakra Focus" do
                gain [Blood, Gen]
                use "Chakra Focus"
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [Gen])

        useOn Enemy "Draining Assault" do
            it "damages target" do
                act
                self $ remove "Draining Assault"
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * 15 + 5
            it "steals chakra during Chakra Focus" do
                gain [Blood, Gen, Nin, Tai]
                use "Chakra Focus"
                act
                turns 5
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood, Gen], [Nin, Tai])

    describeCharacter "Misumi Tsurugi" do
        useOn Enemy "Tighten Joints" do
            it "damages target if target has Soft Physique Modification" do
                use "Soft Physique Modification"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "stuns target if target has Soft Physique Modification" do
                use "Soft Physique Modification"
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
