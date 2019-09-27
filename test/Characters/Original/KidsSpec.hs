{-# LANGUAGE OverloadedLists #-}
module Characters.Original.KidsSpec (spec) where

import TestImport

import qualified Class.Play as P
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" \useOn -> do
        useOn Enemy "Naruto Uzumaki Barrage" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Shadow Clones" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "deals bonus damage during Shadow Clones" $
                    targetHealth - targetHealth' `shouldBe` 10
        useOn Enemy "Rasengan" do
            self $ tag' "Shadow Clones" 0
            act
            targetHealth  <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 45
                it "stuns target" $
                    targetStunned `shouldBe` [All]
        useOn Enemy "Shadow Clones" do
            act
            tagged <- Ninja.hasOwn "Shadow Clones" <$> P.nUser
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "tags user"
                    tagged
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 15

    describeCharacter "Sakura Haruno" \useOn -> do
        useOn Enemy "KO Punch" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            factory
            self $ tag' "Inner Sakura" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "deals bonus damage during Inner Sakura" $
                    targetHealth - targetHealth' `shouldBe` 10
                it "stuns target" $
                    targetStunned `shouldBe` [Mental, Physical]
        useOn Ally "Mystical Palm Healing" do
            damage targetDmg
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "heals target" $
                    targetDmg - (100 - targetHealth) `shouldBe` 25
        useOn Self "Inner Sakura" do
            act
            tagged <- Ninja.hasOwn "Inner Sakura" <$> P.nUser
            enemyTurn do
                damage targetDmg
                apply 0 [Stun All]
            userHealth <- Ninja.health <$> P.nUser
            userStunned <- Effects.stun <$> P.nUser
            return do
                it "tags user"
                    tagged
                it "reduces damage" $
                    100 - userHealth  `shouldBe` targetDmg - 10
                it "ignores non-damage effects" $
                    null userStunned

    describeCharacter "Sasuke Uchiha" \useOn -> do
        useOn Enemy "Lions Barrage" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Sharingan" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage if target has Sharingan" $
                    targetHealth - targetHealth' `shouldBe` 15
        useOn Enemy "Chidori" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Sharingan" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage if target has Sharingan" $
                    targetHealth - targetHealth' `shouldBe` 25
        useOn Enemy "Sharingan" do
            act
            tagged <- Ninja.has "Sharingan" <$> P.user <*> P.nTarget
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            exposed <- targetIsExposed
            return do
                it "tags user"
                    tagged
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 15
                it "exposes target"
                    exposed

    describeCharacter "Kiba Inuzuka" \useOn -> do
        useOn Enemy "Wolf Fang" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Dynamic Marking" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage if target has Dynamic Marking" $
                    targetHealth - targetHealth' `shouldBe` 5
        useOn Enemies "Two-Headed Wolf" do
            act
            enemyTurn $ damage targetDmg
            turns 5
            userHealth   <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            factory
            tag' "Dynamic Marking" 0
            act
            turns 5
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 3 * 15
                it "deals bonus damage if target has Dynamic Marking" $
                    targetHealth - targetHealth' `shouldBe` 3 * 5
                it "reduces damage to user" $
                    100 - userHealth `shouldBe` targetDmg - 15
        useOn Enemy "Dynamic Marking" do
            act
            tagged <- Ninja.has "Dynamic Marking" <$> P.user <*> P.nTarget
            exposed <- targetIsExposed
            return do
                it "tags target"
                    tagged
                it "exposes target"
                    exposed

    describeCharacter "Shino Aburame" \useOn -> do
        useOn Enemy "Chakra Leech" do
            gain [Nin, Tai]
            act
            chakras <- Game.chakra <$> P.game
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            addStacks "Parasite" stacks
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "deals bonus damage per target stack of Parasite" $
                    targetHealth - targetHealth' `shouldBe` 5 * stacks
                it "steals 1 random chakra" $
                    chakras `shouldBe` ([Nin], [Tai])
        useOn Enemy "Parasite" do
            replicateM_ stacks act
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "weakens target damage" $
                  targetDmg - (100 - userHealth) `shouldBe` 5 * stacks
        useOn Allies "Wall of Insects" do
            act
            defense <- totalDefense <$> P.nTarget
            return do
                it "defends user's team" $
                    defense `shouldBe` 20

    describeCharacter "Hinata Hyūga" \useOn -> do
        useOn Enemy "Gentle Fist" do
            gain [Blood, Gen]
            act
            turns 4
            targetHealth <- Ninja.health <$> P.nTarget
            self $ tag' "Byakugan" 0
            gain [Nin, Tai]
            act
            turns 4
            (_, targetChakras) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 20
                it "depletes a random chakra each turn during Byakugan" $
                    targetChakras `shouldBe` [Nin, Tai]
        useOn Enemies "Eight Trigrams Sixty-Four Palms" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            defense <- totalDefense <$> (allyOf =<< P.user)
            factory
            self $ tag' "Byakugan" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15
                it "deals bonus damage during Byakugan" $
                    targetHealth - targetHealth' `shouldBe` 5
                it "defends user's team" $
                    defense `shouldBe` 10
        useOn Self "Byakugan" do
            act
            tagged <- Ninja.hasOwn "Byakugan" <$> P.nUser
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "tags user"
                    tagged
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 15

    describeCharacter "Shikamaru Nara" \useOn -> do
        useOn Enemy "Meditate" do
            act
            tagged <- targetHas "Meditate"
            return do
                it "tags target"
                    tagged
        useOn Enemies "Shadow Strangle" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            exposed <- targetIsExposed
            return do
                it "damages targgets" $
                    100 - targetHealth `shouldBe` 15
                it "exposes enemies"
                    exposed
        useOn Enemies "Shadow Possession" do
            act
            targetStunned <- Effects.stun <$> (allyOf =<< P.target)
            return do
                it "stuns targets" $
                    targetStunned `shouldBe` [NonMental]

    describeCharacter "Chōji Akimichi" \useOn -> do
        useOn Enemy "Obstructing Tackle" do
            act
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 20
        useOn Enemies "Partial Expansion" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 20
        useOn Enemy "Justice Punch" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            exposed <- targetIsExposed
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 25
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "exposes target"
                    exposed
        useOn Enemy "Human Boulder" do
            act
            enemyTurn do
                damage targetDmg
                apply 2 [Stun All]
            userStunned <- Effects.stun <$> P.nUser
            turns 4
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 15
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 15
                it "ignores stuns" $
                    null userStunned
        useOn Enemies "Full Expansion" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            targetStunned <- Effects.stun <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 30
                it "stuns targets" $
                    targetStunned `shouldBe` [All]
        useOn Enemy "Chakra Wings" do
            enemyTurn $ damage targetDmg
            act
            turns 5
            userHealth <- Ninja.health <$> P.nUser
            (userChakras, _) <- Game.chakra <$> P.game
            return do
                it "heals user" $
                    targetDmg - (100 - userHealth) `shouldBe` 3 * 15
                it "bestows chakra" $
                    length (toList userChakras) `shouldBe` 3
        useOn Enemy "Butterfly Bombing" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 45
                it "executes below 20 health" $
                    targetHealth' `shouldBe` 0

    describeCharacter "Ino Yamanaka" \useOn -> do
        useOn Enemy "Mind Destruction" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            exposed <- targetIsExposed
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "stuns target" $
                    targetStunned `shouldBe` [NonMental]
                it "exposes target"
                    exposed
        useOn Enemy "Mind Transfer" do
            act
            targetStunned <- Effects.stun <$> P.nTarget
            exposed <- targetIsExposed
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "exposes target"
                    exposed
        useOn Enemy "Art of the Valentine" do
            tag' "Mind Transfer" 0
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 25
        useOn Enemy "Chakra Hair Trap" do
            act
            enemyTurn $ damage 5
            snared <- Effects.snare <$> P.nTarget
            return do
                it "snares target" $
                    snared == 1

    describeCharacter "Rock Lee" \useOn -> do
        useOn Enemy "Ferocious Fist" do
            act
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Fifth Gate Opening" 0
            act
            turns 5
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 3 * 10
                it "deals bonus damage during Fifth Gate Opening" $
                    targetHealth - targetHealth' `shouldBe` 3 * 15
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 10
        useOn Enemy "Primary Lotus" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Fifth Gate Opening" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage during Fifth Gate Opening" $
                    targetHealth - targetHealth' `shouldBe` 30
        useOn Self "Fifth Gate Opening" do
            act
            tagged <- Ninja.hasOwn "Fifth Gate Opening" <$> P.nUser
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            userHealth <- Ninja.health <$> P.nUser
            act
            userHealth' <- Ninja.health <$> P.nUser
            return do
                it "tags user"
                    tagged
                it "makes user invulnerable" $
                    not harmed
                it "sacrifices health" $
                    100 - userHealth `shouldBe` 50
                it "does not kill user" $
                    userHealth' `shouldBe` 1
        useOn Enemy "Final Lotus" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 100

    describeCharacter "Tenten" \useOn -> do
        useOn Enemy "Unsealing Technique" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            otherEnemyHealth <- Ninja.health <$> (allyOf =<< P.target)
            userStacks <- Ninja.numActive "Unsealing Technique" <$> P.nUser
            self factory
            self $ tag' "Rising Twin Dragons" 0
            act
            userStacks' <- Ninja.numActive "Unsealing Technique" <$> P.nUser
            tagged <- Ninja.hasOwn "Rising Twin Dragons" <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "damages others" $
                    100 - otherEnemyHealth `shouldBe` 10
                it "adds a stack to user" $
                    userStacks `shouldBe` 1
                it "adds a bonus stack during Rising Twin Dragons" $
                    userStacks' - userStacks `shouldBe` 1
                it "expends Rising Twin Dragons" $
                    not tagged
        useOn Enemies "Rising Dragon Control" do
            self do
                addStacks "Unsealing Technique" stacks
                tag' "Rising Twin Dragons" 0
            act
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            tagged <- Ninja.hasOwn "Rising Twin Dragons" <$> P.nUser
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 5 + 10 * stacks
                it "weakens targets" $
                    targetDmg - (100 - userHealth) `shouldBe` 5 + 10 * stacks
                it "expends Rising Twin Dragons" $
                    not tagged
        useOn Self "Rising Twin Dragons" do
            act
            tagged <- Ninja.hasOwn "Rising Twin Dragons" <$> P.nUser
            return do
                it "tags user"
                    tagged

    describeCharacter "Neji Hyūga" \useOn -> do
        useOn Enemy "Gentle Fist" do
            act
            enemyTurn $ damage targetDmg
            turns 4
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 25
                it "weakens target" $
                    targetDmg - (100 - userHealth) `shouldBe` 5
        useOn Enemies "Eight Trigrams Palm Rotation" do
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15
                it "makes user invulnerable" $
                    not harmed
        useOn Enemy "Eight Trigrams Sixty-Four Palms" do
            gain [Nin, Tai]
            act
            targetHealth <- Ninja.health <$> P.nTarget
            (_, targetChakra) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 40
                it "depletes chakra" $
                    targetChakra `shouldBe` [Tai]

    describeCharacter "Gaara" \useOn -> do
        useOn Enemy "Sand Coffin" do
            act
            targetStunned <- Effects.stun <$> P.nTarget
            exposed <- targetIsExposed
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [NonMental]
                it "exposes target"
                    exposed
        useOn Enemy "Sand Burial" do
            tag' "Sand Coffin" 0
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "kills target" $
                    targetHealth `shouldBe` 0
        useOn Self "Sand Clone" do
            act
            enemyTurn $ apply 0 [Plague]
            harmed <- (`is` Plague) <$> P.nUser
            enemyTurn do
                damage targetDmg
                apply 0 [Plague]
            harmed' <- (`is` Plague) <$> P.nUser
            return do
                it "enrages user" $
                    not harmed
                it "breaks upon receiving new damage"
                    harmed'
        useOn Self "Sand Armor" do
            act
            defense <- totalDefense <$> P.nUser
            return do
                it "defends user" $
                    defense `shouldBe` 40
    describeCharacter "Kankurō" \useOn -> do
        useOn Enemy "Iron Maiden" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
        useOn Enemies "Poison Bomb" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 10
        useOn Enemy "Puppet Technique" do
            act
            defense <- totalDefense <$> P.nUser
            damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "strengthens user" $
                    (100 - targetHealth) - targetDmg `shouldBe` 5
                it "defends user" $
                    defense `shouldBe` 15

    describeCharacter "Temari" \useOn -> do
        useOn Enemy "Cyclone Scythe" do
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "makes user invulnerable" $
                    not harmed
        useOn Enemies "Summoning: Blade Dance" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 35
        useOn Enemies "Sandstorm" do
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            turns 1
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "makes user's team invulnerable" $
                    not harmed
                it "weakens targets" $
                    targetDmg - (100 - userHealth) `shouldBe` 15
  where
    describeCharacter = describeCategory Original
    targetDmg = 55
    stacks = 3
