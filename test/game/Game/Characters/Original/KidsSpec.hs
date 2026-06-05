{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.KidsSpec (spec) where
import qualified Sim as Sim

import Import

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" do
        useOn Enemy "Naruto Uzumaki Barrage" do
            it "deals bonus damage during Shadow Clones" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Shadow Clones"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10

    describeCharacter "Sakura Haruno" do
        useOn Enemy "KO Punch" do
            it "deals bonus damage during Inner Sakura" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Inner Sakura"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10

    describeCharacter "Sasuke Uchiha" do
        useOn Enemy "Lions Barrage" do
            it "deals bonus damage if target has Sharingan" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Sharingan"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 15

        useOn Enemy "Chidori" do
            it "deals bonus damage if target has Sharingan" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Sharingan"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 25

    describeCharacter "Kiba Inuzuka" do
        useOn Enemy "Wolf Fang" do
            it "deals bonus damage if target has Dynamic Marking" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Dynamic Marking"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5

        useOn Enemies "Two-Headed Wolf" do
            it "deals bonus damage if target has Dynamic Marking" do
                damagedWithout <- measureDamage do
                    Sim.act
                    Sim.turns 4
                factory
                targeting Self factory
                Sim.use "Dynamic Marking"
                damagedWith <- measureDamage do
                    Sim.act
                    Sim.turns 4
                damagedWith - damagedWithout `shouldBe` 3 * 5

    describeCharacter "Shino Aburame" do
        useOn Enemy "Chakra Leech" do
            it "deals bonus damage per target Parasite" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                replicateM_ testStacks $ Sim.use "Parasite"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * testStacks

    describeCharacter "Hinata Hyūga" do
        useOn Enemy "Gentle Fist" do
            it "depletes chakra during Byakugan" do
                gain [Blood, Gen, Nin]
                Sim.use "Byakugan"
                Sim.act
                Sim.turns 4
                chakras <- gameChakras
                chakras `shouldBe` ([], [Nin])

        useOn Enemies "Eight Trigrams Sixty-Four Palms" do
            it "deals bonus damage during Byakugan" do
                damagedWithout <- measureDamageTo XEnemies Sim.act
                Sim.at XEnemies $ setHealth 100
                Sim.use "Byakugan"
                damagedWith <- measureDamageTo XEnemies Sim.act
                damagedWith - damagedWithout `shouldBe` 5

    describeCharacter "Shikamaru Nara" do
        useOn Enemies "Shadow Strangle" do
            it "lasts longer if target has Meditate" do
                Sim.use "Meditate"
                Sim.act
                Sim.turns 1
                target has "Shadow Strangle"
            it "lasts normally otherwise" do
                Sim.act
                Sim.turns 1
                not <$> target has "Shadow Strangle"

        useOn Enemies "Shadow Possession" do
            it "lasts longer if target has Meditate" do
                Sim.use "Meditate"
                Sim.act
                Sim.turns 1
                target has "Shadow Possession"
            it "lasts normally otherwise" do
                Sim.act
                Sim.turns 1
                not <$> (target has "Shadow Possession")

    describeCharacter "Chōji Akimichi" do
        useOn XAllies "Chili Pill" do
            it "damages user each turn" do
                Sim.act
                damaged <- measureDamageTo Self $ Sim.turns 2
                damaged `shouldBe` 2 * 15


        useOn Self "Chakra Wings" do
            it "pauses Chili Pill damage" do
                Sim.use "Chili Pill"
                Sim.act
                setHealth 100
                apply Permanent [Plague]
                damaged <- measureDamage $ Sim.turns 2
                damaged `shouldBe` 0

        useOn Enemy "Butterfly Bombing" do
            it "damages target" do
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 45
            it "executes below health" do
                setHealth 65
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0

    describeCharacter "Ino Yamanaka" do
        useOn Enemy "Mind Transfer" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Art of the Valentine"

    describeCharacter "Rock Lee" do
        useOn Enemy "Ferocious Fist" do
            it "deals bonus damage during Fifth Gate Opening" do
                damagedWithout <- measureDamage do
                    Sim.act
                    Sim.turns 5
                factory
                targeting Self factory
                targeting Self $ tag' "Fifth Gate Opening" Permanent
                damagedWith <- measureDamage do
                    Sim.act
                    Sim.turns 5
                damagedWith - damagedWithout `shouldBe` 3 * 15

        useOn Enemy "Primary Lotus" do
            it "deals bonus damage during Fifth Gate Opening" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Fifth Gate Opening"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 30

        useOn Self "Fifth Gate Opening" do
            it "cannot kill user" do
                setHealth 20
                Sim.act
                userHealth <- user health
                userHealth `shouldBe` 1
            it "alternates" do
                Sim.act
                user $ hasSkill "Hidden Lotus"

        useOn Enemy "Hidden Lotus" do
            it "damages target" do
                apply Permanent [ Reduce [All] Flat testStacks ]
                damaged <- measureDamage Sim.act
                damaged + testStacks `shouldBe` 100

    describeCharacter "Tenten" do
        useOn Enemy "Unsealing Technique" do
            it "adds a bonus stack during Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                stacks <- Sim.at XEnemies $ target numStacks "Unsealing Technique"
                stacks `shouldBe` 2
            it "spends Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                not <$> user has "Rising Twin Dragons"

        useOn Enemies "Rising Dragon Control" do
            it "damages enemies per Unsealing Technique" do
                replicateM_ testStacks $ Sim.use "Unsealing Technique"
                targeting Everyone $ setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 5 + 10 * testStacks
            it "weakens enemies per Unsealing Technique" do
                replicateM_ testStacks  $ Sim.use "Unsealing Technique"
                targeting Everyone $ setHealth 100
                Sim.act
                damaged <- measureDamageTo Self
                         $ Sim.withClass Physical $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 5 + 10 * testStacks
            it "spends Unsealing Technique" do
                replicateM_ testStacks  $ Sim.use "Unsealing Technique"
                targeting Everyone $ setHealth 100
                Sim.act
                not <$> target has "Unsealing Technique"
            it "lasts an additional turn during Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                Sim.turns 1
                damaged <- measureDamageTo Self
                         $ Sim.withClass Physical $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 5
            it "spends Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                not <$> user has "Rising Twin Dragons"

    describeCharacter "Gaara" do
        useOn Enemy "Sand Coffin" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Sand Burial"

        useOn Enemy "Sand Burial" do
            it "kills target" do
                Sim.use "Sand Coffin"
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0

        useOn Self "Sand Clone" do
            it "blocks harmful effects" do
                Sim.as Enemy $ apply Permanent [ Plague ]
                Sim.act
                Sim.as Enemy $ afflict 5
                not <$> user (`is` Plague)
            it "ends with new non-affliction damage" do
                Sim.as Enemy $ apply Permanent [ Plague ]
                Sim.act
                Sim.as Enemy $ damage 5
                user (`is` Plague)

    describeCharacter "Kankurō" do
        useOn Enemy "Puppet Technique" do
            it "increases damage" do
                replicateM_ testStacks Sim.act
                damaged <- measureDamage $ damage dmg
                damaged - dmg `shouldBe` 5 * testStacks
  where
    describeCharacter = describeCategory Original
    dmg = 55
    testStacks = 3
