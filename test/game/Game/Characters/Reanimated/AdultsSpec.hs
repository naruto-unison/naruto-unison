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
                damaged <- measureDamageTo XEnemies
                         $ targeting Self $ Sim.as XEnemies $ return ()
                damaged `shouldBe` 10

        useOn Enemy "Super Steam Kill" do
            it "damages target" do
                Sim.use "Scorch Style"
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 40

    describeCharacter "Gari" do
        useOn Enemy "Exploding Palm" do
            it "deals stacking damage" do
                replicateM_ testStacks Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ return ()
                damaged `shouldBe` 20 * testStacks

        useOn Enemy "Ground Pound" do
            it "damages on harm" do
                Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ return ()
                damaged `shouldBe` 25
            it "reduces damage" do
                Sim.act
                Sim.as Enemy $ return ()
                damaged <- measureDamageTo Self $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` dmg `quot` 4
            it "heals user" do
                Sim.as Enemy $ damage dmg
                healing <- measureHealingTo Self do
                    Sim.act
                    Sim.as Enemy $ return ()
                    Sim.turns 3
                healing `shouldBe` 2 * 15

    describeCharacter "Ginkaku" do
        useOn Enemy "Seven Stars Blade" do
            it "adds Spirit Words" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Spirit Word"
                stacks `shouldBe` testStacks

        useOn Enemy "Amber Purification Jar" do
            it "adds Spirit Words" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Spirit Word"
                stacks `shouldBe` testStacks

        useOn Enemy "Benihisago" do
            it "damages target per stack" do
                addStacks "Spirit Word" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 10 + 5 * testStacks
            it "adds Spirit Words" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Spirit Word"
                stacks `shouldBe` testStacks
            it "increases Scroll of Fire" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Scroll of Fire"
                stacks `shouldBe` testStacks

    describeCharacter "Kinkaku" do
        useOn Enemy "Leaf Fan" do
            it "adds Spirit Words" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Spirit Word"
                stacks `shouldBe` testStacks

        useOn Enemy "Gold Rope" do
            it "damages on harm stacking" do
                replicateM_ 2 Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ return ()
                damaged `shouldBe` 2 * 35
            it "adds Spirit Words" do
                replicateM_ testStacks Sim.act
                Sim.as Enemy $ return ()
                stacks <- target numStacks "Spirit Word"
                stacks `shouldBe` testStacks

        useOn Enemies "Scroll of Fire" do
            it "adds Spirit Words" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Spirit Word"
                stacks `shouldBe` testStacks
            it "deals bonus damage from stacks" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                addStacks "Scroll of Fire" testStacks
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * testStacks

    describeCharacter "Toroi" do
        useOn Enemy "Demon Wind Shuriken" do
            it "deals bonus damage per Conserving Bee Twin Blades" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Magnetic Field"
                replicateM_ testStacks  $ Sim.use "Conserving Bee Twin Blades"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * testStacks
            it "deals bonus damage per Magnetic Current" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                replicateM_ testStacks  $ Sim.use "Magnetic Current"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * testStacks
            it "causes harmers to ignore healing" do
                Sim.act
                Sim.as Enemy $ return ()
                healing <- measureHealing $ heal 100
                healing `shouldBe` 0

        useOn Self "Magnetic Field" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Conserving Bee Twin Blades"

        useOn Enemy "Conserving Bee Twin Blades" do
            it "deals bonus damage per Magnetic Current" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                replicateM_ testStacks  $ Sim.use "Magnetic Current"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * testStacks

        useOn Enemies "Magnetic Current" do
            it "deals bonus damage per Conserving Bee Twin Blades" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Magnetic Field"
                replicateM_ testStacks  $ Sim.use "Conserving Bee Twin Blades"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * testStacks

    describeCharacter "Fukai" do
        useOn Enemy "Tailed Beast Bomb Barrage" do
            it "damages targets per Chakra Arms" do
                replicateM_ testStacks  $ Sim.use "Chakra Arms"
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30 + 5 * testStacks

        useOn Enemy "Lariat" do
            it "damages targets per Chakra Arms" do
                replicateM_ testStacks  $ Sim.use "Chakra Arms"
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 15 + 5 * testStacks

    describeCharacter "Chiyo" do
        useOn Enemy "Possum" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [ Reveal ]
                not <$> user (`is` Reveal)
            it "damages countered" do
                Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ return ()
                damaged `shouldBe` 20
            it "stuns countered" do
                Sim.act
                Sim.as Enemy $ return ()
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical]

    describeCharacter "Chūkichi" do
        useOn Enemy "Silent Killing" do
            it "deals bonus damage during Hidden Frost" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Hidden Frost"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    testStacks = 3
