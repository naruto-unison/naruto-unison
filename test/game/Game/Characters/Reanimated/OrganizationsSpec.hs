{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.OrganizationsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Jirōbō" do
        useOn Enemy "Rivalry" do
            it "counters target" do
                Sim.act
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent skillName [Reveal]
                not . (`is` Reveal) <$> Sim.targets XAlly
            it "does not taunt if target is not countered" do
                Sim.act
                Sim.as XEnemies $ return ()
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent skillName [Reveal]
                (`is` Reveal) <$> Sim.targets XAlly
            it "counters with taunt" do
                Sim.act
                Sim.at XAlly do
                    Sim.as Enemy $ return ()
                    Sim.as Enemy $ apply Permanent skillName [Reveal]
                not . (`is` Reveal) <$> Sim.targets XAlly
            it "taunts to user" do
                Sim.act
                Sim.at XAlly $ Sim.as Enemy $ return ()
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                user (`is` Reveal)
            it "ends if user uses a skill on a different target" do
                Sim.act
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent skillName [Reveal]
                Sim.at XEnemies $ Sim.use "Sphere of Graves"
                Sim.at XAlly $ Sim.as Enemy $ apply Permanent skillName [Reveal]
                (`is` Reveal) <$> Sim.targets XAlly

        useOn Enemy "Earth Dome Prison" do
            it "drains Rival health" do
                Sim.use "Rivalry"
                Sim.as Enemy $ damage dmg
                Sim.as Enemy $ damage dmg
                healing <- measureHealingTo Self Sim.act
                healing `shouldBe` 20

        useOn Enemy "Summoning: Earth Prison Golem" do
            it "spends Scattered Rocks" do
                replicateM_ 3  $ Sim.use "Sphere of Graves"
                Sim.act
                stacks <- user amount "Scattered Rock"
                stacks `shouldBe` 1

    describeCharacter "Haku" do
        useOn Enemies "Thousand Needles of Death" do
            it "damages all normally" do
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 10
            it "deals all damage to one target during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30
            it "does not damage others during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 0
            it "stuns if target loses 50 health" do
                targeting Self $ apply Permanent skillName [Strengthen [All] Flat 40]
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                targeting Self $ apply Permanent skillName [Strengthen [All] Flat 39]
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
                defense <- user total defense
                defense `shouldBe` dmg - 20

    describeCharacter "Zabuza Momochi" do
        useOn REnemy "Blood Harvest" do
            it "drains into defense" do
                Sim.use "Executioner's Butchering"
                Sim.act
                defense <- user total defense
                defense `shouldBe` 10
            it "also counts Demon Shroud" do
                Sim.use "Demon Shroud"
                Sim.act
                defense <- user total defense
                defense `shouldBe` 10
            it "does not defend more" do
                Sim.use "Executioner's Butchering"
                setHealth testStacks
                Sim.act
                defense <- user total defense
                defense `shouldBe` testStacks
            it "extends Demon Shroud" do
                Sim.use "Demon Shroud"
                replicateM_ 8 Sim.act
                channeling "Demon Shroud"

    describeCharacter "Ameyuri Ringo" do
        useOn Enemies "Lightning Fang" do
            it "refreshes on action" do
                Sim.act
                Sim.turns 1
                Sim.as Enemy $ return ()
                electricDur <- target $ Sim.statusDur "Electricity"
                electricDur `shouldBe` 2
            it "extends duration" do
                replicateM_ testStacks Sim.act
                electricDur <- target $ Sim.statusDur "Electricity"
                electricDur `shouldBe` fromIntegral (1 + testStacks)
            it "damages on action" do
                Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ return ()
                damaged `shouldBe` 5
            it "damages others on action" do
                Sim.act
                damaged <- measureDamageTo XEnemies $ Sim.as Enemy $ return ()
                damaged `shouldBe` 5

        useOn Enemy "Depth Charge" do
            it "deals normal damage normally" do
                apply Permanent skillName [Reduce [All] Flat testStacks]
                damaged <- measureDamage Sim.act
                30 - damaged `shouldBe` testStacks
            it "deals affliction damage if target has Electricity" do
                Sim.use "Lightning Fang"
                apply Permanent skillName [Reduce [All] Flat testStacks]
                damaged <- measureDamage Sim.act
                30 -  damaged `shouldBe` 0

        useOn Enemy "Thunder Gate" do
            it "deals additional damage per enemy with Electricity" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Lightning Fang"
                remove "Electricity"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 2 * 10
            it "shortens Electricity" do
                replicateM_ testStacks  $ Sim.use "Lightning Fang"
                Sim.act
                electricDur <- target $ Sim.statusDur "Electricity"
                electricDur `shouldBe` fromIntegral testStacks - 1

    describeCharacter "Kushimaru Kuriarare" do
        useOn Enemy "Needle Stitching" do
            it "deals bonus damage per target affected" do
                damagedWithout <- measureDamage Sim.act
                targeting Self factory
                targeting Enemies Sim.act
                factory
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * 2
            it "extends duration" do
                Sim.act
                Sim.at XEnemies Sim.act
                target has "Needle Stitching"
            it "does not overextend duration" do
                Sim.act
                Sim.turns 1
                Sim.at XEnemies Sim.act
                not <$> target has "Needle Stitching"
        useOn Enemy "Eviscerate" do
            it "extends Needle Stitching" do
                Sim.use "Needle Stitching"
                Sim.act
                target has "Needle Stitching"
            it "extends Wire Crucifixion" do
                Sim.use "Needle Stitching"
                Sim.use "Wire Crucifixion"
                Sim.act
                target has "Wire Crucifixion"
        useOn Enemy "Wire Crucifixion" do
            it "only affects enemies affected by [Needle Stitching]" do
                targeting Enemies $ Sim.use "Needle Stitching"
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
                Sim.as Enemy $ afflict testStacks
                Sim.turns 5
                userHealth <- user health
                dmg + testStacks - (100 - userHealth) `shouldBe` 3 * 10

        useOn Enemies "Sharp Hair Spear" do
            it "deals bonus damage during Chakra Weave" do
                damagedWithout <- measureDamage Sim.act
                factory
                Sim.use "Chakra Weave"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5

    describeCharacter "Jinin Akebino" do
        useOn Enemy "Hammer Bash" do
            it "exposes target during Axe Chop" do
                Sim.use "Axe Chop"
                Sim.act
                Sim.turns 2
                target (`is` Expose)

    describeCharacter "Deidara" do
        useOn Enemy "Chakra Clay Trap" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                not <$> target (`is` Reveal)
            it "increases the damage of Detonating Clay" do
                replicateM_ testStacks do
                    Sim.act
                    Sim.as Enemy $ return ()
                setHealth 100
                damaged <- measureDamage $ Sim.use "Detonating Clay"
                damaged `shouldBe` 20 + 5 * testStacks

        useOn Enemies "Sonar Bat Bombs" do
            it "increases the damage of Detonating Clay" do
                replicateM_ testStacks Sim.act
                setHealth 100
                damaged <- measureDamage $ Sim.use "Detonating Clay"
                damaged `shouldBe` 20 + 5 * testStacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Jellyfish Explosives"

        useOn Enemy "Jellyfish Explosives" do
            it "increases the damage of Detonating Clay" do
                replicateM_ testStacks Sim.act
                setHealth 100
                damaged <- measureDamage $ Sim.use "Detonating Clay"
                damaged `shouldBe` 20 + 10 * testStacks

    describeCharacter "Sasori" do
        useOn Enemy "Puppet Manipulation" do
            it "stuns target if health goes at or below 35" do
                setHealth 50
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [ Physical, Chakra ]
            it "does not stun otherwise" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "deals bonus damage during Chakra Threads" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Chakra Threads"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5

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
    testStacks = 3
