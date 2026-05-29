{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.FamilySpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Konohamaru Sarutobi" do
        useOn Self "Refocus" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Unsexy Technique"

        useOn Enemy "Throw a Fit" do
            it "damages target per helpful effect from allies" do
                targeting Self $ apply Permanent [ Focus ]
                Sim.as Ally $ targeting Everyone $
                    replicateM_ testStacks $ apply Permanent [ Focus ]
                Sim.act
                Sim.turns 5
                targetHealth <- target health
                100 - targetHealth `shouldBe` 3 * (10 + 5 * testStacks)

        useOn Enemy "Throw a Shuriken" do
            it "damages target per helpful effect from allies" do
                targeting Self $ apply Permanent [ Focus ]
                Sim.as Ally $ targeting Everyone $
                    replicateM_ testStacks $ apply Permanent [ Focus ]
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10 + 10 * testStacks

    describeCharacter "Tsume Inuzuka" do
        useOn Enemy "Call Kuromaru" do
            it "damages attackers" do
                Sim.act
                Sim.withClass NonBane $ Sim.as Enemy $ return ()
                Sim.turns 4
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10
            it "does not damage bane attackers" do
                Sim.act
                Sim.withClass Bane $ Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "alternates" do
                Sim.act
                user $ hasSkill "Fierce Bite"

        useOn Enemy "Fierce Bite" do
            it "strengthens user if target dies" do
                Sim.act
                Sim.as XEnemies kill
                factory
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 10
            it "ignores stuns if target dies" do
                Sim.act
                Sim.as XEnemies kill
                targeting Self $ Sim.as XEnemies $ apply Permanent [ Stun All ]
                userStunned <- user Effects.stun
                userStunned `shouldBe` []
            it "makes user immortal if target dies" do
                Sim.act
                Sim.as XEnemies kill
                targeting Self $ Sim.as XEnemies kill
                userHealth <- user health
                userHealth `shouldBe` 1

        useOn Enemy "Tunneling Fang" do
            it "deals bonus damage during Call Kuromaru" do
                Sim.act
                Sim.turns 5
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Call Kuromaru"
                Sim.act
                Sim.turns 5
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 2 * 5

    describeCharacter "Chōza Akimichi" do
        useOn Enemy "Human Boulder" do
            it "prolongs Chain Bind" do
                Sim.use "Chain Bind"
                Sim.act
                Sim.turns 2
                target has "Chain Bind"

        useOn XAlly "Partial Expansion" do
            it "counters on ally" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $
                    apply Permanent [ Reveal ]
                not <$> target (`is` Reveal)
        useOn Enemy "Partial Expansion" do
            it "counters against enemy" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10


    describeCharacter "Shikaku Nara" do
        useOn Enemy "Shadow Possession" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Shadow Dispersion"
            it "deals bonus damage if target has Black Spider Lily" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Black Spider Lily"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "stuns an additional turn if target has Ensnared" do
                tag' "Ensnared" Permanent
                Sim.act
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [ NonMental ]
            it "does not stun an additional turn otherwise" do
                Sim.act
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

        useOn Enemies "Shadow Dispersion" do
            it "does not damage target of Shadow Possession" do
                Sim.use "Shadow Possession"
                setHealth 100
                Sim.use "Black Spider Lily"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "damages others" do
                Sim.use "Shadow Possession"
                setHealth 100
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 20
            it "deals bonus damage if target has Black Spider Lily" do
                Sim.use "Black Spider Lily"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30
            it "stuns an additional turn if target has Ensnared" do
                tag' "Ensnared" Permanent
                Sim.act
                Sim.turns 1
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [NonMental]
            it "does not stun an additional turn otherwise" do
                Sim.act
                Sim.turns 1
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

        useOn Enemies "Black Spider Lily" do
            it "tags target if they stun" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Stun All ]
                target has "Ensnared"
            it "does not tag otherwise" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Focus ]
                not <$> target has "Ensnared"

    describeCharacter "Inoichi Yamanaka" do
        useOn Self "Sensory Radar" do
            it "restores health when enemy acts" do
                damage dmg
                Sim.act
                replicateM_ testStacks $ Sim.as Enemy $ return ()
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 10 * testStacks
            it "adds stacks when enemy acts" do
                Sim.act
                replicateM_ testStacks $ Sim.as Enemy $ return ()
                stacks <- user numStacks "Sensory Radar"
                stacks `shouldBe` testStacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Sensory Radar: Collate"

        useOn Self "Sensory Radar: Collate" do
            it "gains chakra per Sensory Radar" do
                addStacks "Sensory Radar" 3
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([Blood, Blood, Blood], [])
            it "spends all Sensory Radar" do
                addStacks "Sensory Radar" testStacks
                Sim.act
                stacks <- user numStacks "Sensory Radar"
                stacks `shouldBe` 0

        useOn Enemy "Mental Invasion" do
            it "provides invulnerability with mental harm" do
                Sim.act
                Sim.withClass Mental $ Sim.as Self $ return ()
                Sim.withClass Mental $ Sim.as Self $ return ()
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "does not provide invulnerability otherwise" do
                Sim.act
                Sim.withClass Physical $ Sim.as Self $ return ()
                Sim.withClass Physical $ Sim.as Self $ return ()
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
  where
    describeCharacter = describeCategory Original
    dmg = 55
    testStacks = 3
