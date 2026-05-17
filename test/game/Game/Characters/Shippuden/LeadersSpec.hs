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
                Sim.act
                userHealth <- user health
                userHealth `shouldBe` 21
            it "heals user" do
                setHealth 10
                Sim.act
                userHealth <- user health
                userHealth `shouldBe` 60

    describeCharacter "Jiraiya" do
        useOn Enemy "Giant Flame Bomb" do
            it "damages target per Toad Oil Bomb" do
                addStacks "Toad Oil Bomb" stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 10 * stacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Toad Oil Bomb"

        useOn Enemy "Toad Oil Bomb" do
            it "adds a stack" do
                replicateM_ stacks Sim.act
                numStacks <- targetStacks "Toad Oil Bomb"
                numStacks `shouldBe` stacks

        useOn Ally "Raging Lion's Mane" do
            it "counters Physical Melee" do
                Sim.act
                Sim.withClasses [Physical, Melee] $ Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 25
            it "counters Physical Ranged" do
                Sim.act
                Sim.withClasses [Physical, Ranged] $ Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 15
            it "counters Physical Ranged" do
                Sim.act
                Sim.withClasses [Physical, Ranged] $ Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 15
            it "stuns Physical Ranged" do
                Sim.act
                Sim.withClasses [Physical, Ranged] $ Sim.as Enemy $ return ()
                targetStunned <- Effects.stun <$> Sim.targets Enemy
                targetStunned `shouldBe` [Physical, Melee]
            it "does nothing otherwise" do
                Sim.act
                Sim.withClasses [Physical] $ Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 0

    describeCharacter "Tsunade" do
        useOn Enemy "Heaven Spear Kick" do
            it "deals bonus damage with Seal" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 20
            it "demolishes with Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.as Enemy $ defend Permanent 100
                Sim.act
                targetDefense <- target totalDefense
                targetDefense `shouldBe` 0
            it "spends a Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                not <$> userHas "Strength of One Hundred Seal"

        useOn Ally "Heaven Spear Kick" do
            it "makes target of Healing Wave immortal" do
                apply Permanent [Plague]
                Sim.use "Healing Wave"
                Sim.act
                Sim.as Enemy kill
                targetHealth <- target health
                targetHealth `shouldBe` 1

        useOn Ally "Healing Wave" do
            it "heals target" do
                Sim.as Enemy $ damage dmg
                Sim.act
                Sim.turns 4
                targetHealth <- target health
                dmg - (100 - targetHealth) `shouldBe` 30 + 2 * 10
            it "heals more with a Seal" do
                Sim.use "Strength of One Hundred Seal"
                setHealth 1
                Sim.act
                Sim.turns 4
                targetHealth <- target health
                targetHealth `shouldBe` 1 + 40 + 3 * 10
            it "spends a Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                not <$> userHas "Strength of One Hundred Seal"

        useOn Self "Strength of One Hundred Seal" do
            it "heals more with a Seal" do
                Sim.as Enemy $ damage dmg
                Sim.act
                userHealth <- user health
                self $ setHealth 100
                Sim.as Enemy $ damage dmg
                Sim.use "Strength of One Hundred Seal"
                userHealth' <- user health
                userHealth' - userHealth `shouldBe` 25

    describeCharacter "Ōnoki" do
        useOn Enemy "Atomic Dismantling" do
            it "increases in damage" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.at XEnemies $ replicateM_ stacks $ Sim.act
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10 * stacks

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
    stacks = 3
