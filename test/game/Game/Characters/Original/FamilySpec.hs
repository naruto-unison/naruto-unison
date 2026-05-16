{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.FamilySpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Konohamaru Sarutobi" do
        useOn Self "Refocus" do
            it "alternates" do
                act
                hasSkill "Unsexy Technique" <$> nUser

        useOn Enemy "Throw a Fit" do
            it "damages target per helpful effect from allies" do
                self $ apply Permanent [Focus]
                as Ally $ everyone $
                    replicateM_ stacks $ apply Permanent [Focus]
                act
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * (10 + 5 * stacks)

        useOn Enemy "Throw a Shuriken" do
            it "damages target per helpful effect from allies" do
                self $ apply Permanent [Focus]
                as Ally $ everyone $
                    replicateM_ stacks $ apply Permanent [Focus]
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 + 10 * stacks

    describeCharacter "Tsume Inuzuka" do
        useOn Enemy "Call Kuromaru" do
            it "damages attackers" do
                act
                withClass NonBane $ as Enemy $ return ()
                turns 4
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10
            it "does not damage bane attackers" do
                act
                withClass Bane $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "alternates" do
                act
                hasSkill "Fierce Bite" <$> nUser

        useOn Enemy "Fierce Bite" do
            it "strengthens user if target dies" do
                act
                as XEnemies kill
                factory
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 10
            it "ignores stuns if target dies" do
                act
                as XEnemies kill
                self $ as XEnemies $ apply Permanent [Stun All]
                userStunned <- Effects.stun <$> nUser
                userStunned `shouldBe` []
            it "makes user immortal if target dies" do
                act
                as XEnemies kill
                self $ as XEnemies kill
                userHealth <- health <$> nUser
                userHealth `shouldBe` 1

        useOn Enemy "Tunneling Fang" do
            it "deals bonus damage during Call Kuromaru" do
                act
                turns 5
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Call Kuromaru"
                act
                turns 5
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 2 * 5

    describeCharacter "Chōza Akimichi" do
        useOn Enemy "Human Boulder" do
            it "prolongs Chain Bind" do
                use "Chain Bind"
                act
                turns 2
                has "Chain Bind" <$> user <*> nTarget

        useOn XAlly "Partial Expansion" do
            it "counters on ally" do
                act
                withClass NonMental $ as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nTarget
        useOn Enemy "Partial Expansion" do
            it "counters against enemy" do
                act
                withClass NonMental $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10


    describeCharacter "Shikaku Nara" do
        useOn Enemy "Shadow Possession" do
            it "alternates" do
                act
                hasSkill "Shadow Dispersion" <$> nUser
            it "deals bonus damage if target has Black Spider Lily" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Black Spider Lily"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "stuns an additional turn if target has Ensnared" do
                tag' "Ensnared" Permanent
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [NonMental]
            it "does not stun an additional turn otherwise" do
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []

        useOn Enemies "Shadow Dispersion" do
            it "does not damage target of Shadow Possession" do
                use "Shadow Possession"
                setHealth 100
                use "Black Spider Lily"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "damages others" do
                use "Shadow Possession"
                setHealth 100
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 20
            it "deals bonus damage if target has Black Spider Lily" do
                use "Black Spider Lily"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30
            it "stuns an additional turn if target has Ensnared" do
                tag' "Ensnared" Permanent
                act
                turns 1
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [NonMental]
            it "does not stun an additional turn otherwise" do
                act
                turns 1
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []

        useOn Enemies "Black Spider Lily" do
            it "tags target if they stun" do
                act
                as Enemy $ apply Permanent [Stun All]
                has "Ensnared" <$> user <*> nTarget
            it "does not tag otherwise" do
                act
                as Enemy $ apply Permanent [Focus]
                not <$> (has "Ensnared" <$> user <*> nTarget)

    describeCharacter "Inoichi Yamanaka" do
        useOn Self "Sensory Radar" do
            it "restores health when enemy acts" do
                damage dmg
                act
                replicateM_ stacks $ as Enemy $ return ()
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 10 * stacks
            it "adds stacks when enemy acts" do
                act
                replicateM_ stacks $ as Enemy $ return ()
                userStacks <- numAnyStacks "Sensory Radar" <$> nUser
                userStacks `shouldBe` stacks
            it "alternates" do
                act
                hasSkill "Sensory Radar: Collate" <$> nUser

        useOn Self "Sensory Radar: Collate" do
            it "gains chakra per Sensory Radar" do
                addStacks "Sensory Radar" 3
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood, Blood, Blood], [])
            it "spends all Sensory Radar" do
                addStacks "Sensory Radar" stacks
                act
                userStacks <- numAnyStacks "Sensory Radar" <$> nUser
                userStacks `shouldBe` 0

        useOn Enemy "Mental Invasion" do
            it "provides invulnerability with mental harm" do
                act
                withClass Mental $ as Self $ return ()
                withClass Mental $ as Self $ return ()
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nUser
            it "does not provide invulnerability otherwise" do
                act
                withClass Physical $ as Self $ return ()
                withClass Physical $ as Self $ return ()
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
