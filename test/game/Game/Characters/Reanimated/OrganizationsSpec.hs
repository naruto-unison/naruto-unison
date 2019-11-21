{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.OrganizationsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Jirōbō" do
        useOn Enemy "Rivalry" do
            it "counters with taunt" do
                act
                at XAlly $ as Enemy do
                    apply Permanent [Reveal]
                    apply Permanent [Reveal]
                not . (`is` Reveal) <$> get XAlly
            it "taunts to user" do
                act
                at XAlly $ as Enemy $ apply Permanent [Reveal]
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser
            it "ends if user uses a skill on a different target" do
                act
                at XAlly $ as Enemy $ apply Permanent [Reveal]
                at XEnemies $ use "Sphere of Graves"
                at XAlly $ as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> get XAlly

        useOn Enemy "Earth Dome Prison" do
            it "drains Rival health" do
                use "Rivalry"
                as Enemy $ damage dmg
                as Enemy $ damage dmg
                act
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 20

        useOn Enemy "Summoning: Earth Prison Golem" do
            it "spends Scattered Rocks" do
                replicateM_ 3 $ use "Sphere of Graves"
                act
                userStacks <- numAnyStacks "Scattered Rock" <$> nUser
                userStacks `shouldBe` 1

    describeCharacter "Haku" do
        useOn Enemies "Thousand Needles of Death" do
            it "damages all normally" do
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10
            it "deals all damage to one target during Crystal Ice Mirrors" do
                use "Crystal Ice Mirrors"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30
            it "does not damage others during Crystal Ice Mirrors" do
                use "Crystal Ice Mirrors"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 0
            it "stuns if target loses 50 health" do
                self $ apply Permanent [Strengthen [All] Flat 40]
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                self $ apply Permanent [Strengthen [All] Flat 39]
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []

        useOn Enemy "Acupuncture" do
            it "is single-target normally" do
                act
                not . (`is` Silence) <$> get XEnemies
            it "targets all during Crystal Ice Mirrors" do
                use "Crystal Ice Mirrors"
                act
                (`is` Silence) <$> get XEnemies

        useOn Self "Crystal Ice Mirrors" do
            it "regains health from damage" do
                act
                as Enemy $ damage dmg
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` dmg - 20

    describeCharacter "Zabuza Momochi" do
        useOn Enemy "Blood Harvest" do
            it "drains into defense" do
                use "Executioner's Butchering"
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 10
            it "also counts Demon Shroud" do
                use "Demon Shroud"
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 10
            it "does not defend more" do
                use "Executioner's Butchering"
                setHealth stacks
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` stacks
            it "extends Demon Shroud" do
                use "Demon Shroud"
                replicateM_ 8 act
                isChanneling "Demon Shroud" <$> nUser

    describeCharacter "Ameyuri Ringo" do
        useOn Enemies "Lightning Fang" do
            it "refreshes on action" do
                act
                turns 1
                as Enemy $ return ()
                electricDur <- statusDur "Electricity" <$> nTarget
                electricDur `shouldBe` 2
            it "extends duration" do
                replicateM_ stacks act
                electricDur <- statusDur "Electricity" <$> nTarget
                electricDur `shouldBe` fromIntegral (1 + stacks)
            it "damages on action" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5
            it "damages others on action" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 5

        useOn Enemy "Depth Charge" do
            it "deals normal damage normally" do
                apply Permanent [Reduce [All] Flat stacks]
                act
                targetHealth <- health <$> nTarget
                30 - (100 - targetHealth) `shouldBe` stacks
            it "deals affliction damage if target has Electricity" do
                use "Lightning Fang"
                apply Permanent [Reduce [All] Flat stacks]
                act
                targetHealth <- health <$> nTarget
                30 - (100 - targetHealth) `shouldBe` 0

        useOn Enemy "Thunder Gate" do
            it "deals additional damage per enemy with Electricity" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Lightning Fang"
                remove "Electricity"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 2 * 10
            it "shortens Electricity" do
                replicateM_ stacks $ use "Lightning Fang"
                act
                electricDur <- statusDur "Electricity" <$> nTarget
                electricDur `shouldBe` fromIntegral stacks - 1

    describeCharacter "Kushimaru Kuriarare" do
        useOn Enemy "Needle and Thread" do
            it "deals bonus damage during Stitching Spider" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Stitching Spider"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Fuguki Suikazan" do
        useOn Enemy "Chakra Weave" do
            it "heals when unharmed" do
                as Enemy $ damage dmg
                act
                turns 1
                as Enemy $ afflict stacks
                turns 5
                userHealth <- health <$> nUser
                dmg + stacks - (100 - userHealth) `shouldBe` 3 * 10

        useOn Enemies "Sharp Hair Spear" do
            it "deals bonus damage during Chakra Weave" do
                act
                targetHealth <- health <$> get XEnemies
                factory
                use "Chakra Weave"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Jinin Akebino" do
        useOn Enemy "Hammer Bash" do
            it "exposes target during Axe Chop" do
                use "Axe Chop"
                act
                turns 2
                targetIsExposed

    describeCharacter "Deidara" do
        useOn Enemy "Chakra Clay Trap" do
            it "counters target" do
                act
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "increases the damage of Detonating Clay" do
                replicateM_ stacks do
                    act
                    as Enemy $ return ()
                setHealth 100
                use "Detonating Clay"
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 5 * stacks

        useOn Enemies "Sonar Bat Bombs" do
            it "increases the damage of Detonating Clay" do
                replicateM_ stacks act
                setHealth 100
                use "Detonating Clay"
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 5 * stacks
            it "alternates" do
                act
                hasSkill "Jellyfish Explosives" <$> nUser

        useOn Enemy "Jellyfish Explosives" do
            it "increases the damage of Detonating Clay" do
                replicateM_ stacks act
                setHealth 100
                use "Detonating Clay"
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 10 * stacks

    describeCharacter "Sasori" do
        useOn Enemy "Puppet Manipulation" do
            it "stuns target if health goes at or below 35" do
                setHealth 50
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [Physical, Chakra]
            it "does not stun otherwise" do
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "deals bonus damage during Chakra Threads" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Chakra Threads"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Nagato" do
        useOn Enemy "Human Path" do
            it "alternates" do
                act
                hasSkill "Naraka Path" <$> nUser

        useOn Self "Asura Path" do
            it "activates at or above 50 health" do
                setHealth 50
                hasSkill "Asura Path" <$> nUser
        useOn Self "Preta Path" do
            it "activates below 50 health" do
                setHealth 49
                hasSkill "Preta Path" <$> nUser
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    stacks = 3
