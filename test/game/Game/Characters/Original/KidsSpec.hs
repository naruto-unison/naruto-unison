{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.KidsSpec (spec) where

import Import

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" do
        useOn Enemy "Naruto Uzumaki Barrage" do
            it "deals bonus damage during Shadow Clones" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Shadow Clones"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Sakura Haruno" do
        useOn Enemy "KO Punch" do
            it "deals bonus damage during Inner Sakura" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Inner Sakura"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Sasuke Uchiha" do
        useOn Enemy "Lions Barrage" do
            it "deals bonus damage if target has Sharingan" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Sharingan"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 15

        useOn Enemy "Chidori" do
            it "deals bonus damage if target has Sharingan" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Sharingan"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 25

    describeCharacter "Kiba Inuzuka" do
        useOn Enemy "Wolf Fang" do
            it "deals bonus damage if target has Dynamic Marking" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Dynamic Marking"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

        useOn Enemies "Two-Headed Wolf" do
            it "deals bonus damage if target has Dynamic Marking" do
                act
                turns 4
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Dynamic Marking"
                act
                turns 4
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 3 * 5

    describeCharacter "Shino Aburame" do
        useOn Enemy "Chakra Leech" do
            it "deals bonus damage per target Parasite" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                replicateM_ stacks $ use "Parasite"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * stacks

    describeCharacter "Hinata Hyūga" do
        useOn Enemy "Gentle Fist" do
            it "depletes chakra during Byakugan" do
                gain [Blood, Gen, Nin]
                use "Byakugan"
                act
                turns 4
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Nin])

        useOn Enemies "Eight Trigrams Sixty-Four Palms" do
            it "deals bonus damage during Byakugan" do
                act
                targetHealth <- health <$> get XEnemies
                at XEnemies $ setHealth 100
                use "Byakugan"
                act
                targetHealth' <- health <$> get XEnemies
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Shikamaru Nara" do
        useOn Enemies "Shadow Strangle" do
            it "lasts longer if target has Meditate" do
                use "Meditate"
                act
                turns 1
                has "Shadow Strangle" <$> user <*> nTarget
            it "lasts normally otherwise" do
                act
                turns 1
                not <$> (has "Shadow Strangle" <$> user <*> nTarget)

        useOn Enemies "Shadow Possession" do
            it "lasts longer if target has Meditate" do
                use "Meditate"
                act
                turns 1
                has "Shadow Possession" <$> user <*> nTarget
            it "lasts normally otherwise" do
                act
                turns 1
                not <$> (has "Shadow Possession" <$> user <*> nTarget)

    describeCharacter "Chōji Akimichi" do
        useOn Self "Chakra Wings" do
            it "blocks Chili damage" do
                act
                hasOwn "unchili" <$> nUser

        useOn XAllies "Chili Pill" do
            it "pauses damage during unchili" do
                act
                turns stacks
                self $ tag' "unchili" Permanent
                turns 10
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` 10 + (stacks + 1) * 15

        useOn Enemy "Butterfly Bombing" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 45
            it "executes below health" do
                setHealth 65
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0

    describeCharacter "Ino Yamanaka" do
        useOn Enemy "Mind Transfer" do
            it "alternates" do
                act
                hasSkill "Art of the Valentine" <$> nUser

    describeCharacter "Rock Lee" do
        useOn Enemy "Ferocious Fist" do
            it "deals bonus damage during Fifth Gate Opening" do
                act
                turns 5
                targetHealth <- health <$> nTarget
                factory
                self factory
                self $ tag' "Fifth Gate Opening" Permanent
                act
                turns 5
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 3 * 15

        useOn Enemy "Primary Lotus" do
            it "deals bonus damage during Fifth Gate Opening" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Fifth Gate Opening"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 30

        useOn Self "Fifth Gate Opening" do
            it "cannot kill user" do
                setHealth 20
                act
                userHealth <- health <$> nUser
                userHealth `shouldBe` 1
            it "alternates" do
                act
                hasSkill "Hidden Lotus" <$> nUser

        useOn Enemy "Hidden Lotus" do
            it "damages target" do
                apply Permanent [Reduce [All] Flat stacks]
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 100 - stacks

    describeCharacter "Tenten" do
        useOn Enemy "Unsealing Technique" do
            it "adds a bonus stack during Rising Twin Dragons" do
                use "Rising Twin Dragons"
                act
                targetStacks <- numAnyStacks "Unsealing Technique"
                                <$> get XEnemies
                targetStacks `shouldBe` 2
            it "spends Rising Twin Dragons" do
                use "Rising Twin Dragons"
                act
                not . hasOwn "Rising Twin Dragons" <$> nUser

        useOn Enemies "Rising Dragon Control" do
            it "damages enemies per Unsealing Technique" do
                replicateM_ stacks $ use "Unsealing Technique"
                everyone $ setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 + 10 * stacks
            it "weakens enemies per Unsealing Technique" do
                replicateM_ stacks $ use "Unsealing Technique"
                everyone $ setHealth 100
                act
                withClass Physical $ as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 5 + 10 * stacks
            it "spends Unsealing Technique" do
                replicateM_ stacks $ use "Unsealing Technique"
                everyone $ setHealth 100
                act
                not <$> (has "Unsealing Technique" <$> user <*> nTarget)
            it "lasts an additional turn during Rising Twin Dragons" do
                use "Rising Twin Dragons"
                act
                turns 1
                withClass Physical $ as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 5
            it "spends Rising Twin Dragons" do
                use "Rising Twin Dragons"
                act
                not . hasOwn "Rising Twin Dragons" <$> nUser

    describeCharacter "Gaara" do
        useOn Enemy "Sand Coffin" do
            it "alternates" do
                act
                hasSkill "Sand Burial" <$> nUser

        useOn Enemy "Sand Burial" do
            it "kills target" do
                use "Sand Coffin"
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0

        focus $ useOn Self "Sand Clone" do
            it "blocks harmful effects" do
                as Enemy $ apply Permanent [Plague]
                act
                as Enemy $ afflict 5
                --not . (`is` Plague) <$> nUser
                hasOwn "Sand Clone" <$> nUser
            it "ends with new non-affliction damage" do
                as Enemy $ apply Permanent [Plague]
                act
                as Enemy $ damage 5
                --(`is` Plague) <$> nUser
                not . hasOwn "Sand Clone" <$> nUser

    describeCharacter "Kankurō" do
        useOn Enemy "Puppet Technique" do
            it "increases damage" do
                replicateM_ stacks act
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 5 * stacks
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
