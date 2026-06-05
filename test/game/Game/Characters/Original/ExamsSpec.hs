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
                stacks <- user numStacks "Umbrella"
                stacks `shouldBe` 4

        useOn Self "Umbrella Gathering" do
            it "reduces damage per Umbrella" do
                addStacks "Umbrella" testStacks
                Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 10 * testStacks

        useOn Enemies "Senbon Shower" do
            it "damages enemies" do
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 15
            it "spends an Umbrella" do
                targeting Self $ addStacks "Umbrella" testStacks
                Sim.act
                stacks <- user numStacks "Umbrella"
                stacks `shouldBe` testStacks - 1

        useOn Enemy "Senbon Barrage" do
            it "damages enemy per Umbrella" do
                targeting Self $ addStacks "Umbrella" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 15 * testStacks
            it "spends all Umbrellas" do
                targeting Self $ addStacks "Umbrella" testStacks
                Sim.act
                not <$> user has "Umbrella"

    describeCharacter "Oboro" do
        useOn Enemy "Underground Move" do
            it "is normally single-target" do
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 0
            it "affects all enemies during Fog Clone" do
                Sim.use "Fog Clone"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 20

    describeCharacter "Kabuto Yakushi" do
        useOn Enemies "Temple of Nirvana" do
            it "does not stun immediately" do
                Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` []
            it "stuns on inactive" do
                Sim.act
                Sim.as REnemy $ return ()
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun active" do
                Sim.act
                Sim.as Enemy $ return ()
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

    describeCharacter "Dosu Kinuta" do
        useOn Enemy "Resonating Echo Drill" do
            it "deals bonus damage during Echo Speaker Tuning" do
                Sim.act
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Echo Speaker Tuning"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 20

        useOn Enemy "Sound Manipulation" do
            it "deals bonus damage if target has Resonating Echo Drill" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Resonating Echo Drill"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10
            it "deals bonus damage during Echo Speaker Tuning" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Echo Speaker Tuning"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10

    describeCharacter "Kin Tsuchi" do
        useOn Enemy "Bell Ring Illusion" do
            it "deals bonus damage during Unnerving Bells" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Unnerving Bells"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 25
            it "does not make user invulnerable normally" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "makes user invulnerable during Shadow Senbon" do
                Sim.use "Shadow Senbon"
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)

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
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 0
            it "deals damage during Bell Ring Illusion" do
                Sim.use "Bell Ring Illusion"
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 10

        useOn Enemy "Unnerving Bells" do
            it "does not make target vulnerable normally" do
                Sim.act
                damaged <- measureDamage
                         $ Sim.withClass Physical $ Sim.as Self $ damage dmg
                damaged - dmg `shouldBe` 0
            it "makes target vulnerable during Bell Ring Illusion" do
                Sim.use "Bell Ring Illusion"
                setHealth 100
                Sim.act
                damaged <- measureDamage $ Sim.withClass Chakra $ damage dmg
                damaged - dmg `shouldBe` 15
            it "makes target vulnerable during Shadow Senbon" do
                Sim.use "Shadow Senbon"
                Sim.act
                damaged <- measureDamage $ Sim.withClass Physical $ damage dmg
                damaged - dmg `shouldBe` 15

    describeCharacter "Yoroi Akadō" do
        useOn Enemy "Energy Drain" do
            it "steals health" do
                Sim.as Enemy $ damage dmg
                healed <- measureHealingTo Self Sim.act
                healed `shouldBe` 20
            it "steals chakra during Chakra Focus" do
                gain [Blood, Gen]
                Sim.use "Chakra Focus"
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [Gen])

        useOn Enemy "Draining Assault" do
            it "damages target" do
                damaged <- measureDamage do
                    Sim.act
                    targeting Self $ remove "Draining Assault"
                    Sim.turns 5
                damaged `shouldBe` 3 * 15 + 5
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
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 20
            it "stuns target if target has Soft Physique Modification" do
                Sim.use "Soft Physique Modification"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
  where
    describeCharacter = describeCategory Original
    dmg = 55
    testStacks = 3
