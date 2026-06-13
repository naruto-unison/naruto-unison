{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.LeadersSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Orochimaru" do
        useOn Self "Body Replacement Substitution" do
            it "does not work above 20 health" do
                setHealth 21
                healed <- measureHealing Sim.act
                healed `shouldBe` 0
            it "heals user" do
                setHealth 10
                healed <- measureHealing Sim.act
                healed `shouldBe` 50

    describeCharacter "Jiraiya" do
        useOn Enemy "Giant Flame Bomb" do
            it "damages target per Toad Oil Bomb" do
                addStacks "Toad Oil Bomb" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 20 + 10 * testStacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Toad Oil Bomb"

        useOn Enemy "Toad Oil Bomb" do
            it "adds a stack" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Toad Oil Bomb"
                stacks `shouldBe` testStacks

        useOn Ally "Raging Lion's Mane" do
            it "counters Physical Melee" do
                Sim.act
                damaged <- measureDamageTo Enemy
                         $ Sim.withClasses [Physical, Melee]
                         $ Sim.as Enemy $ return ()
                damaged `shouldBe` 25
            it "counters Physical Ranged" do
                Sim.act
                damaged <- measureDamageTo Enemy
                         $ Sim.withClasses [Physical, Ranged]
                         $ Sim.as Enemy $ return ()
                damaged `shouldBe` 15
            it "counters Physical Ranged" do
                Sim.act
                damaged <- measureDamageTo Enemy
                         $ Sim.withClasses [Physical, Ranged]
                         $ Sim.as Enemy $ return ()
                damaged `shouldBe` 15
            it "stuns Physical Ranged" do
                Sim.act
                Sim.withClasses [Physical, Ranged] $ Sim.as Enemy $ return ()
                targetStunned <- Effects.stun <$> Sim.targets Enemy
                targetStunned `shouldBe` [Physical, Melee]
            it "does nothing otherwise" do
                Sim.act
                damaged <- measureDamageTo Enemy
                         $ Sim.withClasses [Physical]
                         $ Sim.as Enemy $ return ()
                damaged `shouldBe` 0

    describeCharacter "Tsunade" do
        useOn Enemy "Heaven Spear Kick" do
            it "deals bonus damage with Seal" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Strength of One Hundred Seal"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 20
            it "demolishes with Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.as Enemy $ defend Permanent 100
                Sim.act
                targetDefense <- target totalDefense
                targetDefense `shouldBe` 0
            it "spends a Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                not <$> user has "Strength of One Hundred Seal"

        useOn Ally "Heaven Spear Kick" do
            it "makes target of Healing Wave immortal" do
                apply Permanent skillName [Plague]
                Sim.use "Healing Wave"
                Sim.act
                Sim.as Enemy kill
                targetHealth <- target health
                targetHealth `shouldBe` 1

        useOn Ally "Healing Wave" do
            it "heals target" do
                Sim.as Enemy $ damage dmg
                healed <- measureHealing do
                    Sim.act
                    Sim.turns 4
                healed `shouldBe` 30 + 2 * 10
            it "heals more with a Seal" do
                Sim.use "Strength of One Hundred Seal"
                setHealth 1
                healed <- measureHealing do
                    Sim.act
                    Sim.turns 4
                healed `shouldBe` 40 + 3 * 10
            it "spends a Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                not <$> user has "Strength of One Hundred Seal"

        useOn Self "Strength of One Hundred Seal" do
            it "heals more with a Seal" do
                Sim.as Enemy $ damage dmg
                healedWithout <- measureHealing Sim.act
                targeting Self $ setHealth 100
                Sim.as Enemy $ damage dmg
                healedWith <- measureHealing
                            $ Sim.use "Strength of One Hundred Seal"
                healedWith - healedWithout `shouldBe` 25

    describeCharacter "Ōnoki" do
        useOn Enemy "Atomic Dismantling" do
            it "increases in damage" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.at XEnemies $ replicateM_ testStacks $ Sim.act
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10 * testStacks

    describeCharacter "Fukasaku and Shima" do
        useOn Enemies "Demonic Illusion: Gamarinsho" do
            it "does not stun after two" do
                Sim.act
                Sim.act
                Sim.turns 1
                Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` []
            it "stuns after three" do
                replicateM_ 3 Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` [All]
            it "cancels previous stuns" do
                replicateM_ 3 Sim.act
                Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` []
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    testStacks = 3
