{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.LeadersSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Orochimaru" do
        useOn Self "Body Replacement Substitution" do
            it "does not work above 20 health" do
                setHealth 21
                act
                userHealth <- health <$> nUser
                userHealth `shouldBe` 21
            it "heals user" do
                setHealth 10
                act
                userHealth <- health <$> nUser
                userHealth `shouldBe` 60

    describeCharacter "Jiraiya" do
        useOn Enemy "Giant Flame Bomb" do
            it "damages target per Toad Oil Bomb" do
                addStacks "Toad Oil Bomb" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 10 * stacks
            it "alternates" do
                act
                hasSkill "Toad Oil Bomb" <$> nUser

        useOn Enemy "Toad Oil Bomb" do
            it "adds a stack" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Toad Oil Bomb" <$> nTarget
                targetStacks `shouldBe` stacks

        useOn Ally "Raging Lion's Mane" do
            it "counters Physical Melee" do
                act
                withClasses [Physical, Melee] $ as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 25
            it "counters Physical Ranged" do
                act
                withClasses [Physical, Ranged] $ as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 15
            it "counters Physical Ranged" do
                act
                withClasses [Physical, Ranged] $ as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 15
            it "stuns Physical Ranged" do
                act
                withClasses [Physical, Ranged] $ as Enemy $ return ()
                targetStunned <- Effects.stun <$> get Enemy
                targetStunned `shouldBe` [Physical, Melee]
            it "does nothing otherwise" do
                act
                withClasses [Physical] $ as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 0

    describeCharacter "Tsunade" do
        useOn Enemy "Heaven Spear Kick" do
            it "deals bonus damage with Seal" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Strength of One Hundred Seal"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20
            it "demolishes with Seal" do
                use "Strength of One Hundred Seal"
                as Enemy $ defend Permanent 100
                act
                targetDefense <- totalDefense <$> nTarget
                targetDefense `shouldBe` 0
            it "spends a Seal" do
                use "Strength of One Hundred Seal"
                act
                not . hasOwn "Strength of One Hundred Seal" <$> nUser

        useOn Ally "Heaven Spear Kick" do
            it "makes target of Healing Wave immortal" do
                apply Permanent [Plague]
                use "Healing Wave"
                act
                as Enemy kill
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 1

        useOn Ally "Healing Wave" do
            it "heals target" do
                as Enemy $ damage dmg
                act
                turns 4
                targetHealth <- health <$> nTarget
                dmg - (100 - targetHealth) `shouldBe` 30 + 2 * 10
            it "heals more with a Seal" do
                use "Strength of One Hundred Seal"
                setHealth 1
                act
                turns 4
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 1 + 40 + 3 * 10
            it "spends a Seal" do
                use "Strength of One Hundred Seal"
                act
                not . hasOwn "Strength of One Hundred Seal" <$> nUser

        useOn Self "Strength of One Hundred Seal" do
            it "heals more with a Seal" do
                as Enemy $ damage dmg
                act
                userHealth <- health <$> nUser
                self $ setHealth 100
                as Enemy $ damage dmg
                use "Strength of One Hundred Seal"
                userHealth' <- health <$> nUser
                userHealth' - userHealth `shouldBe` 25

    describeCharacter "Ōnoki" do
        useOn Enemy "Atomic Dismantling" do
            it "increases in damage" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                at XEnemies $ replicateM_ stacks $ act
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10 * stacks

    describeCharacter "Fukasaku and Shima" do
        useOn Enemies "Demonic Illusion: Gamarinsho" do
            it "does not stun after two" do
                act
                act
                turns 1
                act
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` []
            it "stuns after three" do
                replicateM_ 3 act
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` [All]
            it "cancels previous stuns" do
                replicateM_ 3 act
                act
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` []
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    stacks = 3
