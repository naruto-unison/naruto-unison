{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.AdultsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Pakura" do
        useOn Enemy "Scorch Style" do
            it "harms harm" do
                Sim.act
                self $ Sim.as XEnemies $ return ()
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Super Steam Kill" do
            it "damages target" do
                apply Permanent [AntiChannel]
                Sim.use "Scorch Style"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 40

    describeCharacter "Gari" do
        useOn Enemy "Exploding Palm" do
            it "deals stacking damage" do
                replicateM_ stacks Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 * stacks

        useOn Enemy "Ground Pound" do
            it "damages on harm" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 25
            it "reduces damage" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` dmg `quot` 4
            it "heals user" do
                Sim.as Enemy $ damage dmg
                Sim.act
                Sim.as Enemy $ return ()
                Sim.turns 3
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 2 * 15

    describeCharacter "Ginkaku" do
        useOn Enemy "Seven Stars Blade" do
            it "adds Spirit Words" do
                replicateM_ stacks Sim.act
                targetStacks <- target $ numAnyStacks "Spirit Word"
                targetStacks `shouldBe` stacks

        useOn Enemy "Amber Purification Jar" do
            it "adds Spirit Words" do
                replicateM_ stacks Sim.act
                targetStacks <- target $ numAnyStacks "Spirit Word"
                targetStacks `shouldBe` stacks

        useOn Enemy "Benihisago" do
            it "damages target per stack" do
                Sim.as XEnemies $ addStacks "Spirit Word" stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10 + 5 * stacks
            it "adds Spirit Words" do
                replicateM_ stacks Sim.act
                targetStacks <- target $ numAnyStacks "Spirit Word"
                targetStacks `shouldBe` stacks
            it "increases Scroll of Fire" do
                replicateM_ stacks Sim.act
                targetStacks <- target $ numAnyStacks "Scroll of Fire"
                targetStacks `shouldBe` stacks

    describeCharacter "Kinkaku" do
        useOn Enemy "Leaf Fan" do
            it "adds Spirit Words" do
                replicateM_ stacks Sim.act
                targetStacks <- target $ numAnyStacks "Spirit Word"
                targetStacks `shouldBe` stacks

        useOn Enemy "Gold Rope" do
            it "damages on harm stacking" do
                replicateM_ 2 Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * 35
            it "adds Spirit Words" do
                replicateM_ stacks Sim.act
                Sim.as Enemy $ return ()
                targetStacks <- target $ numAnyStacks "Spirit Word"
                targetStacks `shouldBe` stacks

        useOn Enemies "Scroll of Fire" do
            it "adds Spirit Words" do
                replicateM_ stacks Sim.act
                targetStacks <- target $ numAnyStacks "Spirit Word"
                targetStacks `shouldBe` stacks
            it "deals bonus damage from stacks" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.as XEnemies $ addStacks "Scroll of Fire" stacks
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * stacks

    describeCharacter "Toroi" do
        useOn Enemy "Demon Wind Shuriken" do
            it "deals bonus damage per Conserving Bee Twin Blades" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Magnetic Field"
                replicateM_ stacks  $ Sim.use "Conserving Bee Twin Blades"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * stacks
            it "deals bonus damage per Magnetic Current" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                replicateM_ stacks  $ Sim.use "Magnetic Current"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * stacks
            it "causes harmers to ignore healing" do
                Sim.act
                Sim.as Enemy $ return ()
                heal 100
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20

        useOn Self "Magnetic Field" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Conserving Bee Twin Blades"

        useOn Enemy "Conserving Bee Twin Blades" do
            it "deals bonus damage per Magnetic Current" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                replicateM_ stacks  $ Sim.use "Magnetic Current"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * stacks

        useOn Enemies "Magnetic Current" do
            it "deals bonus damage per Conserving Bee Twin Blades" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Magnetic Field"
                replicateM_ stacks  $ Sim.use "Conserving Bee Twin Blades"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * stacks

    describeCharacter "Fukai" do
        useOn Enemy "Tailed Beast Bomb Barrage" do
            it "damages targets per Chakra Arms" do
                replicateM_ stacks  $ Sim.use "Chakra Arms"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30 + 5 * stacks

        useOn Enemy "Lariat" do
            it "damages targets per Chakra Arms" do
                replicateM_ stacks  $ Sim.use "Chakra Arms"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15 + 5 * stacks

    describeCharacter "Chiyo" do
        useOn Enemy "Possum" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "damages countered" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "stuns countered" do
                Sim.act
                Sim.as Enemy $ return ()
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical]

    describeCharacter "Chūkichi" do
        useOn Enemy "Silent Killing" do
            it "deals bonus damage during Hidden Frost" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Hidden Frost"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    stacks = 3
