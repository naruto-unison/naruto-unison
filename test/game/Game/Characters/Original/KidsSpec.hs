{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.KidsSpec (spec) where
import qualified Sim as Sim

import Import

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" do
        useOn Enemy "Naruto Uzumaki Barrage" do
            it "deals bonus damage during Shadow Clones" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Shadow Clones"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Sakura Haruno" do
        useOn Enemy "KO Punch" do
            it "deals bonus damage during Inner Sakura" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Inner Sakura"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Sasuke Uchiha" do
        useOn Enemy "Lions Barrage" do
            it "deals bonus damage if target has Sharingan" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Sharingan"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 15

        useOn Enemy "Chidori" do
            it "deals bonus damage if target has Sharingan" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Sharingan"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 25

    describeCharacter "Kiba Inuzuka" do
        useOn Enemy "Wolf Fang" do
            it "deals bonus damage if target has Dynamic Marking" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Dynamic Marking"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5

        useOn Enemies "Two-Headed Wolf" do
            it "deals bonus damage if target has Dynamic Marking" do
                Sim.act
                Sim.turns 4
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Dynamic Marking"
                Sim.act
                Sim.turns 4
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 3 * 5

    describeCharacter "Shino Aburame" do
        useOn Enemy "Chakra Leech" do
            it "deals bonus damage per target Parasite" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                replicateM_ stacks $ Sim.use "Parasite"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5 * stacks

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
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                Sim.at XEnemies $ setHealth 100
                Sim.use "Byakugan"
                Sim.act
                targetHealth' <- health <$> Sim.targets XEnemies
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Shikamaru Nara" do
        useOn Enemies "Shadow Strangle" do
            it "lasts longer if target has Meditate" do
                Sim.use "Meditate"
                Sim.act
                Sim.turns 1
                targetHas "Shadow Strangle"
            it "lasts normally otherwise" do
                Sim.act
                Sim.turns 1
                not <$> targetHas "Shadow Strangle"

        useOn Enemies "Shadow Possession" do
            it "lasts longer if target has Meditate" do
                Sim.use "Meditate"
                Sim.act
                Sim.turns 1
                targetHas "Shadow Possession"
            it "lasts normally otherwise" do
                Sim.act
                Sim.turns 1
                not <$> (targetHas "Shadow Possession")

    describeCharacter "Chōji Akimichi" do
        useOn Self "Chakra Wings" do
            it "blocks Chili damage" do
                Sim.act
                userHas "unchili"

        useOn XAllies "Chili Pill" do
            it "pauses damage during unchili" do
                Sim.act
                Sim.turns stacks
                targeting Self $ tag' "unchili" Permanent
                Sim.turns 10
                userHealth <- user health
                100 - userHealth `shouldBe` 10 + (stacks + 1) * 15

        useOn Enemy "Butterfly Bombing" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 45
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
                Sim.act
                Sim.turns 5
                targetHealth <- target health
                factory
                targeting Self factory
                targeting Self $ tag' "Fifth Gate Opening" Permanent
                Sim.act
                Sim.turns 5
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 3 * 15

        useOn Enemy "Primary Lotus" do
            it "deals bonus damage during Fifth Gate Opening" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Fifth Gate Opening"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 30

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
                apply Permanent [ Reduce [All] Flat stacks ]
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 100 - stacks

    describeCharacter "Tenten" do
        useOn Enemy "Unsealing Technique" do
            it "adds a bonus stack during Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                numStacks <- Sim.at XEnemies $ targetStacks "Unsealing Technique"
                numStacks `shouldBe` 2
            it "spends Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                not <$> userHas "Rising Twin Dragons"

        useOn Enemies "Rising Dragon Control" do
            it "damages enemies per Unsealing Technique" do
                replicateM_ stacks $ Sim.use "Unsealing Technique"
                targeting Everyone $ setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 5 + 10 * stacks
            it "weakens enemies per Unsealing Technique" do
                replicateM_ stacks  $ Sim.use "Unsealing Technique"
                targeting Everyone $ setHealth 100
                Sim.act
                Sim.withClass Physical $ Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 5 + 10 * stacks
            it "spends Unsealing Technique" do
                replicateM_ stacks  $ Sim.use "Unsealing Technique"
                targeting Everyone $ setHealth 100
                Sim.act
                not <$> targetHas "Unsealing Technique"
            it "lasts an additional turn during Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                Sim.turns 1
                Sim.withClass Physical $ Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 5
            it "spends Rising Twin Dragons" do
                Sim.use "Rising Twin Dragons"
                Sim.act
                not <$> userHas "Rising Twin Dragons"

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
                replicateM_ stacks Sim.act
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 5 * stacks
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
