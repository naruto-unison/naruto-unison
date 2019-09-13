{-# LANGUAGE OverloadedLists #-}

module Characters.Original.ExamsSpec (spec) where

import TestImport

import qualified Class.Play as P
import qualified Model.Game as Game
import qualified Model.Ninja as Ninja
import qualified Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Hanabi Hyūga" \useOn -> do
        useOn Enemy "Gentle Fist" do
            act
            enemyTurn $ self $ gain [Blood, Gen]
            turns 4
            targetHealth <- Ninja.health <$> P.nTarget
            (_, targetChakra) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 15
                it "depletes a random chakra when target affects chakra" $
                    targetChakra `shouldBe` [Gen]
        useOn Enemy "Eight Trigrams Palm Rotation" do
            act
            turns 3
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self factory
            act
            enemyTurn $ apply 0 [Stun All]
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 15
                it "stuns target if they stun" $
                    targetStunned `shouldBe` [All]
        useOn Self "Unyielding Tenacity" do
            act
            damage targetDmg
            enemyTurn $ apply 0 [Stun All]
            userStunned <- Effects.stun <$> P.nUser
            enemyTurn $ kill
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "prevents death" $
                    userHealth `shouldBe` 1
                it "ignores stuns" $
                    null userStunned

    describeCharacter "Shigure" \useOn -> do
        useOn Self "Umbrella Toss" do
            act
            userStacks <- Ninja.numActive "Umbrella" <$> P.nUser
            return do
                it "adds stacks to user" $
                    userStacks `shouldBe` 4
        useOn Self "Umbrella Gathering" do
            self $ addStacks "Umbrella" stacks
            act
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            userStacks <- Ninja.numActive "Umbrella" <$> P.nUser
            return do
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 10 * stacks
                it "spends all umbrellas" $
                    userStacks `shouldBe` 0
        useOn Enemies "Senbon Shower" do
            self $ addStacks "Umbrella" stacks
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            userStacks <- Ninja.numActive "Umbrella" <$> P.nUser
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15
                it "spends an umbrella" $
                    stacks - userStacks `shouldBe` 1
        useOn Enemy "Senbon Barrage" do
            self $ addStacks "Umbrella" stacks
            act
            targetHealth <- Ninja.health <$> P.nTarget
            userStacks <- Ninja.numActive "Umbrella" <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15 * stacks
                it "spends all umbrellas" $
                    userStacks `shouldBe` 0

    describeCharacter "Kabuto Yakushi" \useOn -> do
        useOn Enemy "Chakra Scalpel" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            setHealth 100
            targetExhaust <- Effects.exhaust [All] <$> P.nTarget
            damage targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "adds to damage against target" $
                   (100 - targetHealth') - targetDmg `shouldBe` 5
                it "adds a random chakra to target's skills" $
                    toList targetExhaust `shouldBe` [Rand]
        useOn Self "Pre-Healing Technique" do
            let initialHealth = 10
            enemyTurn $ withClass Bane $ apply 0 [Reveal]
            self $ setHealth initialHealth
            act
            uncured <- (`is` Reveal) <$> P.nTarget
            turns 8
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "cures bane" $
                    not uncured
                it "heals user" $
                    userHealth `shouldBe` initialHealth + 5 * 15
        useOn Enemies "Temple of Nirvana" do
            act
            enemyTurn $ damage 5
            enemyStunned <- Effects.stun <$> P.nTarget
            othersStunned <- Effects.stun <$> (allyOf =<< P.target)
            factory
            act
            turns 1
            withClass Physical $ damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "stuns targets" $
                    othersStunned `shouldBe` [All]
                it "adds to damage against targets" $
                    (100 - targetHealth) - targetDmg `shouldBe` 10
                it "does not affect targets who act" $
                    null enemyStunned

    describeCharacter "Dosu Kinuta" \useOn -> do
        useOn Enemy "Resonating Echo Drill" do
            act
            tagged <- Ninja.has "Echoing Sound" <$> P.user <*> P.nTarget
            targetHealth <- Ninja.health <$> P.nTarget
            exposed <- targetIsExposed
            factory
            self $ tag' "Echo Speaker Tuning" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "deals bonus damage during Echo Speaker Tuning" $
                    targetHealth - targetHealth' `shouldBe` 20
                it "exposes target"
                    exposed
                it "tags target"
                    tagged
        useOn Enemy "Sound Manipulation" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            setHealth 100
            damage targetDmg
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Echo Speaker Tuning" 0
            act
            targetHealth'' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 10
                it "deals bonus damage during Echo Speaker Tuning" $
                    targetHealth - targetHealth'' `shouldBe` 10
                it "adds to damage received by target" $
                    (100 - targetHealth') - targetDmg `shouldBe` 5
                it "weakens target" $
                    targetDmg - (100 - userHealth) `shouldBe` 5
        useOn Self "Echo Speaker Tuning" do
            act
            tagged <- Ninja.hasOwn "Echo Speaker Tuning" <$> P.nUser
            return do
                it "tags user"
                    tagged

    describeCharacter "Kin Tsuchi" \useOn -> do
        useOn Enemy "Bell Ring Illusion" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Unnerving Bells" 0
            act
            tagged <- Ninja.hasOwn "Bell Ring Illusion" <$> P.nUser
            targetHealth' <- Ninja.health <$> P.nTarget
            self do
                factory
                tag' "Shadow Senbon" 0
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            return do
                it "tags user"
                    tagged
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
                it "deals bonus damage during Unnerving Bells" $
                    targetHealth - targetHealth' `shouldBe` 25
                it "makes user invulnerable during Shadow Senbon" $
                    not harmed
        useOn Enemy "Shadow Senbon" do
            act
            tagged <- Ninja.hasOwn "Shadow Senbon" <$> P.nUser
            exposed <- targetIsExposed
            factory
            self $ tag' "Unnerving Bells" 0
            act
            targetStunned <- Effects.stun <$> P.nTarget
            factory
            self factory
            self $ tag' "Bell Ring Illusion" 0
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "tags user"
                    tagged
                it "exposes target"
                    exposed
                it "stuns target during Unnerving Bells" $
                    targetStunned `shouldBe` [All]
                it "damages target during Bell Ring Illusion" $
                    100 - targetHealth `shouldBe` 10
        useOn Enemy "Unnerving Bells" do
            gain [Blood, Gen]
            act
            tagged <- Ninja.hasOwn "Unnerving Bells" <$> P.nUser
            (_, targetChakra) <- Game.chakra <$> P.game
            factory
            self $ tag' "Shadow Senbon" 0
            act
            withClass Physical $ damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self do
                factory
                tag' "Bell Ring Illusion" 0
            act
            withClass Chakra $ damage targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "tags user"
                    tagged
                it "depletes 1 random chakra" $
                    targetChakra `shouldBe` [Gen]
                it "adds to damage against target during Shadow Senbon" $
                    (100 - targetHealth) - targetDmg `shouldBe` 15
                it "adds to damage against target during Bell Ring Illusion" $
                    (100 - targetHealth') - targetDmg `shouldBe` 15

    describeCharacter "Zaku Abumi" \useOn -> do
        useOn Enemy "Slicing Sound Wave" do
            act
            tagged <- Ninja.hasOwn "Slicing Sound Wave" <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "tags user"
                    tagged
                it "damages target" $
                    100 - targetHealth `shouldBe` 25
        useOn XAlly "Wall of Air" do
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nTarget
            return do
                it "counters a skill" $
                    not harmed
        useOn Enemies "Supersonic Slicing Wave" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 45

    describeCharacter "Oboro" \useOn -> do
        useOn Enemies "Exploding Kunai" do
            act
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15
        useOn Enemy "Underground Move" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            self $ tag' "Fog Clone" 0
            act
            targetHealth' <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "stuns target" $
                    targetStunned `shouldBe` [Physical, Mental]
                it "targets all during Fog Clone" $
                    100 - targetHealth' `shouldBe` 20
        useOn Enemy "Fog Clone" do
            act
            tagged <- Ninja.hasOwn "Fog Clone" <$> P.nUser
            defense <- totalDefense <$> P.nUser
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            return do
                it "tags user"
                    tagged
                it "defends user" $
                    defense `shouldBe` 30
                it "makes user invulnerable" $
                    not harmed

    describeCharacter "Yoroi Akadō" \useOn -> do
        useOn Enemy "Energy Drain" do
            gain [Blood, Gen]
            enemyTurn $ damage targetDmg
            act
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            damage targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            self factory
            act
            enemyTurn $ damage targetDmg
            userHealth' <- Ninja.health <$> P.nUser
            self $ tag' "Chakra Focus" 0
            act
            (_, targetChakra) <- Game.chakra <$> P.game
            return do
                it "steals health" $
                    100 - targetHealth `shouldBe` 20
                it "heals user" $
                    targetDmg - (100 - userHealth) `shouldBe` 20
                it "strengthens user" $
                    (100 - targetHealth') - targetDmg `shouldBe` 5
                it "weakens target" $
                    targetDmg - (100 - userHealth') `shouldBe` 5
                it "steals a random chakra during Chakra Focus" $
                    targetChakra `shouldBe` [Gen]
        useOn Enemy "Draining Assault" do
            gain [Blood, Gen, Nin, Tai]
            act
            enemyTurn $ damage targetDmg
            turns 5
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Chakra Focus" 0
            act
            factory
            damage targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            turns 5
            (_, targetChakra) <- Game.chakra <$> P.game
            return do
                it "steals health" $
                    100 - targetHealth `shouldBe` 3 * 15
                it "strengthens user" $
                    (100 - targetHealth') - targetDmg `shouldBe` 5
                it "weakens enemy" $
                    targetDmg - (100 - userHealth) `shouldBe` 15 * 2 + 5
                it "steals a random chakra each turn during Chakra Focus" $
                    targetChakra `shouldBe` [Tai]
        useOn Self "Chakra Focus" do
            act
            tagged <- Ninja.hasOwn "Chakra Focus" <$> P.nUser
            return do
                it "tags user"
                    tagged

    describeCharacter "Misumi Tsurugi" \useOn -> do
        useOn Ally "Flexible Twisting Joints" do
            act
            enemyTurn $ damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "reduces damage" $
                    100 - targetHealth `shouldBe` targetDmg - 15
        useOn Enemy "Flexible Twisting Joints" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
        useOn Enemy "Soft Physique Modification" do
            act
            exposed <- targetIsExposed
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "exposes target"
                    exposed
                it "transfers harm from user" $
                    100 - userHealth `shouldBe` 0
                it "tranfers harm to target" $
                    100 - targetHealth `shouldBe` targetDmg
        useOn Enemy "Tighten Joints" do
            act
            defense <- totalDefense <$> P.nUser
            tag' "Soft Physique Modification" 0
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "defends user" $
                    defense `shouldBe` 15
                it "damages target of Soft Physique Modification" $
                    100 - targetHealth `shouldBe` 20
                it "stuns target of Soft Physique Modification" $
                    targetStunned `shouldBe` [All]
  where
    describeCharacter = describeCategory Original
    targetDmg = 55
    stacks = 3
