{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.OrganizationsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Jirōbō" do
        useOn Enemy "Rivalry" do
            it "counters with taunt" do
                Sim.act
                Sim.at XAlly $ Sim.as Enemy do
                    apply Permanent [Reveal]
                    apply Permanent [Reveal]
                not . (`is` Reveal) <$> Sim.targets XAlly
            it "taunts to user" do
                Sim.act
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent [Reveal]
                Sim.as Enemy $ apply Permanent [Reveal]
                user (`is` Reveal)
            it "ends if user uses a skill on a different target" do
                Sim.act
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent [Reveal]
                Sim.at XEnemies  $ Sim.use "Sphere of Graves"
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> Sim.targets XAlly

        useOn Enemy "Earth Dome Prison" do
            it "drains Rival health" do
                Sim.use "Rivalry"
                Sim.as Enemy $ damage dmg
                Sim.as Enemy $ damage dmg
                Sim.act
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 20

        useOn Enemy "Summoning: Earth Prison Golem" do
            it "spends Scattered Rocks" do
                replicateM_ 3  $ Sim.use "Sphere of Graves"
                Sim.act
                userStacks <- user $ numAnyStacks "Scattered Rock"
                userStacks `shouldBe` 1

    describeCharacter "Haku" do
        useOn Enemies "Thousand Needles of Death" do
            it "damages all normally" do
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 10
            it "deals all damage to one target during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30
            it "does not damage others during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 0
            it "stuns if target loses 50 health" do
                self $ apply Permanent [Strengthen [All] Flat 40]
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                self $ apply Permanent [Strengthen [All] Flat 39]
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

        useOn Enemy "Acupuncture" do
            it "is single-target normally" do
                Sim.act
                not . (`is` Silence) <$> Sim.targets XEnemies
            it "targets all during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                (`is` Silence) <$> Sim.targets XEnemies

        useOn Self "Crystal Ice Mirrors" do
            it "regains health from damage" do
                Sim.act
                Sim.as Enemy $ damage dmg
                userDefense <- user totalDefense
                userDefense `shouldBe` dmg - 20

    describeCharacter "Zabuza Momochi" do
        useOn Enemy "Blood Harvest" do
            it "drains into defense" do
                Sim.use "Executioner's Butchering"
                Sim.act
                userDefense <- user totalDefense
                userDefense `shouldBe` 10
            it "also counts Demon Shroud" do
                Sim.use "Demon Shroud"
                Sim.act
                userDefense <- user totalDefense
                userDefense `shouldBe` 10
            it "does not defend more" do
                Sim.use "Executioner's Butchering"
                setHealth stacks
                Sim.act
                userDefense <- user totalDefense
                userDefense `shouldBe` stacks
            it "extends Demon Shroud" do
                Sim.use "Demon Shroud"
                replicateM_ 8 Sim.act
                user $ isChanneling "Demon Shroud"

    describeCharacter "Ameyuri Ringo" do
        useOn Enemies "Lightning Fang" do
            it "refreshes on action" do
                Sim.act
                Sim.turns 1
                Sim.as Enemy $ return ()
                electricDur <- target $ Sim.statusDur "Electricity"
                electricDur `shouldBe` 2
            it "extends duration" do
                replicateM_ stacks Sim.act
                electricDur <- target $ Sim.statusDur "Electricity"
                electricDur `shouldBe` fromIntegral (1 + stacks)
            it "damages on action" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 5
            it "damages others on action" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 5

        useOn Enemy "Depth Charge" do
            it "deals normal damage normally" do
                apply Permanent [Reduce [All] Flat stacks]
                Sim.act
                targetHealth <- target health
                30 - (100 - targetHealth) `shouldBe` stacks
            it "deals affliction damage if target has Electricity" do
                Sim.use "Lightning Fang"
                apply Permanent [Reduce [All] Flat stacks]
                Sim.act
                targetHealth <- target health
                30 - (100 - targetHealth) `shouldBe` 0

        useOn Enemy "Thunder Gate" do
            it "deals additional damage per enemy with Electricity" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Lightning Fang"
                remove "Electricity"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 2 * 10
            it "shortens Electricity" do
                replicateM_ stacks  $ Sim.use "Lightning Fang"
                Sim.act
                electricDur <- target $ Sim.statusDur "Electricity"
                electricDur `shouldBe` fromIntegral stacks - 1

    describeCharacter "Kushimaru Kuriarare" do
        useOn Enemy "Needle Stitching" do
            it "deals bonus damage per target affected" do
                Sim.act
                targetHealth <- target health
                self factory
                enemies Sim.act
                factory
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * 2
            it "extends duration" do
                Sim.act
                Sim.at XEnemies Sim.act
                targetHas "Needle Stitching"
            it "does not overextend duration" do
                Sim.act
                Sim.turns 1
                Sim.at XEnemies Sim.act
                not <$> targetHas "Needle Stitching"
        useOn Enemy "Eviscerate" do
            it "extends Needle Stitching" do
                Sim.use "Needle Stitching"
                Sim.act
                targetHas "Needle Stitching"
            it "extends Wire Crucifixion" do
                Sim.use "Needle Stitching"
                Sim.use "Wire Crucifixion"
                Sim.act
                targetHas "Wire Crucifixion"
        useOn Enemy "Wire Crucifixion" do
            it "only affects enemies affected by [Needle Stitching]" do
                enemies  $ Sim.use "Needle Stitching"
                remove "Needle Stitching"
                Sim.act
                affected <- numAffected "Wire Crucifixion"
                affected `shouldBe` 2

    describeCharacter "Fuguki Suikazan" do
        useOn Enemy "Chakra Weave" do
            it "heals when unharmed" do
                Sim.as Enemy $ damage dmg
                Sim.act
                Sim.turns 1
                Sim.as Enemy $ afflict stacks
                Sim.turns 5
                userHealth <- user health
                dmg + stacks - (100 - userHealth) `shouldBe` 3 * 10

        useOn Enemies "Sharp Hair Spear" do
            it "deals bonus damage during Chakra Weave" do
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                factory
                Sim.use "Chakra Weave"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Jinin Akebino" do
        useOn Enemy "Hammer Bash" do
            it "exposes target during Axe Chop" do
                Sim.use "Axe Chop"
                Sim.act
                Sim.turns 2
                Sim.targetIsExposed

    describeCharacter "Deidara" do
        useOn Enemy "Chakra Clay Trap" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                target $ not . (`is` Reveal)
            it "increases the damage of Detonating Clay" do
                replicateM_ stacks do
                    Sim.act
                    Sim.as Enemy $ return ()
                setHealth 100
                Sim.use "Detonating Clay"
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 5 * stacks

        useOn Enemies "Sonar Bat Bombs" do
            it "increases the damage of Detonating Clay" do
                replicateM_ stacks Sim.act
                setHealth 100
                Sim.use "Detonating Clay"
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 5 * stacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Jellyfish Explosives"

        useOn Enemy "Jellyfish Explosives" do
            it "increases the damage of Detonating Clay" do
                replicateM_ stacks Sim.act
                setHealth 100
                Sim.use "Detonating Clay"
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 10 * stacks

    describeCharacter "Sasori" do
        useOn Enemy "Puppet Manipulation" do
            it "stuns target if health goes at or below 35" do
                setHealth 50
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical, Chakra]
            it "does not stun otherwise" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "deals bonus damage during Chakra Threads" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Chakra Threads"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Nagato" do
        useOn Enemy "Human Path" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Naraka Path"

        useOn Self "Asura Path" do
            it "activates at or above 50 health" do
                setHealth 50
                user $ hasSkill "Asura Path"
        useOn Self "Preta Path" do
            it "activates below 50 health" do
                setHealth 49
                user $ hasSkill "Preta Path"
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    stacks = 3
