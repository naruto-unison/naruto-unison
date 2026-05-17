{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.ExamsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Hanabi Hyūga" do
        useOn Enemy "Gentle Fist" do
            it "depletes chakra upon drain" do
                gain $ replicate 5 Blood
                Sim.act
                Sim.as Enemy $ deplete 1
                Sim.as Enemy $ deplete 2
                chakras <- gameChakras
                chakras `shouldBe` ([], [Blood, Blood, Blood])

    describeCharacter "Shigure" do
        useOn Self "Umbrella Toss" do
            it "adds stacks" do
                Sim.act
                userStacks <- user $ numAnyStacks "Umbrella"
                userStacks `shouldBe` 4

        useOn Self "Umbrella Gathering" do
            it "reduces damage per Umbrella" do
                addStacks "Umbrella" stacks
                Sim.act
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 10 * stacks

        useOn Enemies "Senbon Shower" do
            it "damages enemies" do
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 15
            it "spends an Umbrella" do
                self $ addStacks "Umbrella" stacks
                Sim.act
                userStacks <- user $ numAnyStacks "Umbrella"
                userStacks `shouldBe` stacks - 1

        useOn Enemy "Senbon Barrage" do
            it "damages enemy per Umbrella" do
                self $ addStacks "Umbrella" stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15 * stacks
            it "spends all Umbrellas" do
                self $ addStacks "Umbrella" stacks
                Sim.act
                not <$> userHas "Umbrella"

    describeCharacter "Oboro" do
        useOn Enemy "Underground Move" do
            it "is normally single-target" do
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                targetHealth `shouldBe` 100
            it "affects all enemies during Fog Clone" do
                Sim.use "Fog Clone"
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 20

    describeCharacter "Kabuto Yakushi" do
        useOn Enemies "Temple of Nirvana" do
            it "does not stun immediately" do
                Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` []
            it "stuns on inactive" do
                Sim.act
                Sim.turns 1
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` [All]
            it "does not stun active" do
                Sim.act
                Sim.as Enemy $ return ()
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` [All]

    describeCharacter "Dosu Kinuta" do
        useOn Enemy "Resonating Echo Drill" do
            it "deals bonus damage during Echo Speaker Tuning" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Echo Speaker Tuning"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 20

        useOn Enemy "Sound Manipulation" do
            it "deals bonus damage if target has Resonating Echo Drill" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Resonating Echo Drill"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage during Echo Speaker Tuning" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Echo Speaker Tuning"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Kin Tsuchi" do
        useOn Enemy "Bell Ring Illusion" do
            it "deals bonus damage during Unnerving Bells" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Unnerving Bells"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 25
            it "does not make user invulnerable normally" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user (`is` Reveal)
            it "makes user invulnerable during Shadow Senbon" do
                Sim.use "Shadow Senbon"
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)

        useOn Enemy "Shadow Senbon" do
            it "does not stun normally" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "stuns during Unnerving Bells" do
                Sim.use "Unnerving Bells"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not deal damage normally" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "deals damage during Bell Ring Illusion" do
                Sim.use "Bell Ring Illusion"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Unnerving Bells" do
            it "does not make target vulnerable normally" do
                Sim.act
                Sim.withClass Physical $ Sim.as Self $ damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 0
            it "makes target vulnerable during Bell Ring Illusion" do
                Sim.use "Bell Ring Illusion"
                setHealth 100
                Sim.act
                Sim.withClass Chakra $ damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 15
            it "makes target vulnerable during Shadow Senbon" do
                Sim.use "Shadow Senbon"
                Sim.act
                Sim.withClass Physical $ damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 15

    describeCharacter "Yoroi Akadō" do
        useOn Enemy "Energy Drain" do
            it "steals health" do
                Sim.as Enemy $ damage dmg
                Sim.act
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 20
            it "steals chakra during Chakra Focus" do
                gain [Blood, Gen]
                Sim.use "Chakra Focus"
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [Gen])

        useOn Enemy "Draining Assault" do
            it "damages target" do
                Sim.act
                self $ remove "Draining Assault"
                Sim.turns 5
                targetHealth <- target health
                100 - targetHealth `shouldBe` 3 * 15 + 5
            it "steals chakra during Chakra Focus" do
                gain [Blood, Gen, Nin, Tai]
                Sim.use "Chakra Focus"
                Sim.act
                Sim.turns 5
                chakras <- gameChakras
                chakras `shouldBe` ([Blood, Gen], [Nin, Tai])

    describeCharacter "Misumi Tsurugi" do
        useOn Enemy "Tighten Joints" do
            it "damages target if target has Soft Physique Modification" do
                Sim.use "Soft Physique Modification"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "stuns target if target has Soft Physique Modification" do
                Sim.use "Soft Physique Modification"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
