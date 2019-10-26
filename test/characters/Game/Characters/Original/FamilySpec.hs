{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.FamilySpec (spec) where

import Import

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "Konohamaru Sarutobi" \useOn -> do
        useOn Self "Refocus" do
            apply 0 [Reduce [All] Flat stacks]
            as Ally $ apply 0 [Reduce [All] Flat stacks]
            act
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            factory
            act
            self $ defend 0 stacks
            as Ally $ defend 0 stacks
            defense <- Ninja.totalDefense <$> P.nUser
            factory
            as Enemy $ damage targetDmg
            act
            self $ heal stacks
            as Ally $ heal stacks
            userHealth' <- Ninja.health <$> P.nUser
            return do
                it "boosts status effects from allies" $
                    targetDmg - (100 - userHealth) `shouldBe` 3 * stacks
                it "boosts defense from allies" $
                    defense `shouldBe` 3 * stacks
                it "boosts healing from allies" $
                    targetDmg - (100 - userHealth') `shouldBe` 3 * stacks

        useOn Enemy "Unsexy Technique" do
            act
            exposed <- targetIsExposed
            effects <- Ninja.effects <$> P.nTarget
            apply 0 [Reduce [All] Flat 10, Pierce]
            effects' <- Ninja.effects <$> P.nTarget
            trap 0 (CounterAll All) $ return ()
            damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "exposes target" $
                    exposed
                it "blocks damage reduction and helpful effects" $
                    effects' `shouldBe` effects
                it "negates counters" $
                    100 - targetHealth `shouldBe` targetDmg

        useOn Enemy "Throw a Fit" do
            act
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            as Ally $ everyone $ replicateM_ stacks $ apply 0 [Focus]
            act
            turns 5
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 3 * 10
                it "deals bonus damge per helpful status" $
                    targetHealth - targetHealth' `shouldBe` 3 * 5 * stacks

        useOn Enemy "Throw a Shuriken" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            as Ally $ everyone $ replicateM_ stacks $ apply 0 [Focus]
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 10
                it "deals bonus damge per helpful status" $
                    targetHealth - targetHealth' `shouldBe` 10 * stacks

    describeCharacter "Hiashi Hyūga" \useOn -> do
        useOn Enemy "Gentle Fist" do
            gain [Blood, Gen, Nin]
            act
            turns 3
            targetHealth <- Ninja.health <$> P.nTarget
            (_, chakra) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 20
                it "depletes chakra" $
                    toList chakra `shouldBe` [Nin]

        useOn Enemy "Eight Trigrams Palm Rotation" do
            act
            turns 3
            targetHealth <- Ninja.health <$> P.nTarget
            otherHealth <- Ninja.health <$> get XEnemies
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 15
                it "damages others" $
                    100 - otherHealth `shouldBe` 2 * 10

        useOn Enemy "Eight Trigrams Air Palm Wall" do
            self act
            as Enemy $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            reflected <- (`is` Reveal) <$> P.nTarget
            return do
                it "reflects a skill..." $
                    not harmed
                it "...onto its user"
                    reflected

    describeCharacter "Tsume Inuzuka" \useOn -> do
        useOn Enemy "Call Kuromaru" do
            act
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "reduces damage" $
                    targetDmg - (100 - userHealth) `shouldBe` 10
                it "damages attackers" $
                    100 - targetHealth `shouldBe` 10

        useOn Enemy "Fierce Bite" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            damage targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            self factory
            setHealth 10
            as Enemy $ apply 0 [Stun All]
            act
            factory
            damage targetDmg
            targetHealth'' <- Ninja.health <$> P.nTarget
            userStunned <- Effects.stun <$> P.nUser
            as Enemy kill
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 25
                it "does not activate if target does not die" $
                    targetDmg - (100 - targetHealth') `shouldBe` 0
                it "ignores stuns" $
                    userStunned `shouldBe` []
                it "increases damage" $
                    (100 - targetHealth'') - targetDmg `shouldBe` 10
                it "makes user unkillable" $
                    userHealth `shouldBe` 1

        useOn Enemy "Tunneling Fang" do
            act
            as Enemy $ apply 5 [Stun All]
            turns $ 5 - 2
            userStunned <- Effects.stun <$> P.nUser
            turns 1
            userStunned' <- Effects.stun <$> P.nUser
            factory
            self factory
            act
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Call Kuromaru" 0
            act
            turns 5
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "throttles stuns" $
                    userStunned' `shouldBe` []
                it "does not remove stuns" $
                    userStunned `shouldBe` []
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 15
                it "deals bonus damage during Call Kuromaru" $
                    targetHealth - targetHealth' `shouldBe` 2 * 5

        useOn Allies "Light Bomb" do
            act
            withClass NonBane $ as Enemy $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nTarget
            factory
            act
            withClass Bane $ as Enemy $ apply 0 [Reveal]
            harmed' <- (`is` Reveal) <$> P.nTarget
            return do
                it "makes targets invulnerable to non-bane" $
                    not harmed
                it "does not make targets invulnerable to other skills"
                    harmed'

    describeCharacter "Chōza Akimichi" \useOn -> do
        useOn Enemy "Chain Bind" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            defense <- Ninja.totalDefense <$> get XAlly
            self demolishAll
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 5
                it "defends team" $
                    defense `shouldBe` 5
                it "weakens target" $
                    targetDmg - (100 - userHealth) `shouldBe` 10

        useOn Enemy "Human Boulder" do
            tag' "Chain Bind" 1
            act
            turns 1
            tag' "Chain Bind" 1
            turns 2
            defense <- Ninja.totalDefense <$> get XAlly
            prolonged <- Ninja.numStacks "Chain Bind" <$> P.user <*> P.nTarget
            factory
            self factory
            act
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 3 * 15
                it "defends team" $
                    defense `shouldBe` 10
                it "prolongs Chain Bind" $
                    prolonged `shouldBe` 1

        useOn Enemy "Partial Expansion" do
            act
            withClass NonMental $ as Enemy $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "counters target" $
                    not harmed
                it "damages target if countered" $
                    100 - targetHealth `shouldBe` 10

        useOn Ally "Partial Expansion" do
            act
            withClass NonMental $ as Enemy $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nTarget
            targetHealth <- Ninja.health <$> get Enemy
            return do
                it "counters target" $
                    not harmed
                it "damages countered target" $
                    100 - targetHealth `shouldBe` 10

    describeCharacter "Shikaku Nara" \useOn -> do
        useOn Enemy "Shadow Possession" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            factory
            tag' "Black Spider Lily" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            tag' "Ensnared" 0
            act
            turns 1
            targetStunned' <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "stuns target" $
                    targetStunned `shouldBe` [NonMental]
                it "deals bonus damage if target has Black Spider Lily" $
                    targetHealth - targetHealth' `shouldBe` 10
                it "stuns 1 turn extra with Ensnared" $
                    targetStunned' `shouldBe` [NonMental]

        useOn Enemy "Shadow Dispersion" do
            tag' "Shadow Possession" 0
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            otherHealth <- Ninja.health <$> get XEnemies
            otherStunned <- Effects.stun <$> get XEnemies
            factory
            tag' "Black Spider Lily" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            tag' "Ensnared" 0
            act
            turns 1
            targetStunned' <- Effects.stun <$> P.nTarget
            return do
              it "does not damage target" $
                  100 - targetHealth `shouldBe` 0
              it "does not stun target" $
                  targetStunned `shouldBe` []
              it "damages others" $
                  100 - otherHealth `shouldBe` 20
              it "stuns others" $
                  otherStunned `shouldBe` [NonMental]
              it "deals bonus damage if target has Black Spider Lily" $
                  otherHealth - targetHealth' `shouldBe` 10
              it "stuns 1 turn extra with Ensnared" $
                  targetStunned' `shouldBe` [NonMental]

        useOn Enemies "Black Spider Lily" do
            act
            has <- Ninja.has "Black Spider Lily" <$> P.user <*> get XEnemies
            as Enemy $ apply 1 [Stun All]
            as XEnemies $ damage 5
            hasEnemy <- Ninja.has "Ensnared" <$> P.user <*> get Enemy
            hasXEnemy <- Ninja.has "Ensnared" <$> P.user <*> get XEnemies
            return do
                it "tags targets"
                    has
                it "ensnares if stun"
                    hasEnemy
                it "does not ensnare otherwise" $
                    not hasXEnemy

        useOn Ally "Problem Analysis" do
            act
            as Enemy $ pierce targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            defense <- Ninja.totalDefense <$> P.nTarget
            factory
            act
            as Enemy $ afflict targetDmg
            defense' <- Ninja.totalDefense <$> P.nTarget
            return do
                it "absorbs non-affliction damage" $
                    100 - targetHealth `shouldBe` 0
                it "converts damage into defense" $
                    defense `shouldBe` targetDmg
                it "does not convert affliction damage" $
                    defense' `shouldBe` 0

    describeCharacter "Inoichi Yamanaka" \useOn -> do
        useOn Enemy "Psycho Mind Transmission" do
            act
            as Enemy $ self $ trap 0 (Counter All) $ return ()
            hasCounter <- Ninja.hasTrap "Psycho " <$> P.target <*> P.nTarget
            turns 3
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 2 * 20
                it "prevents target from applying counters" $
                    not hasCounter

        useOn Enemy "Sensory Radar" do
            as Enemy $ damage targetDmg
            act
            replicateM_ stacks $ as Enemy $ return ()
            userHealth <- Ninja.health <$> P.nUser
            userStacks <- Ninja.numStacks "Sensory Radar" <$> P.user <*> P.nUser
            return do
                it "heals user when enemy harms any" $
                    targetDmg - (100 - userHealth) `shouldBe` 10 * stacks
                it "gains stacks when enemy harms any" $
                    userStacks `shouldBe` 3

        useOn Self "Sensory Radar: Collate" do
            addStacks "Sensory Radar" stacks
            act
            (chakra, _) <- Game.chakra <$> P.game
            userStacks <- Ninja.numStacks "Sensory Radar" <$> P.user <*> P.nUser
            return do
                it "spends all stacks" $
                    userStacks `shouldBe` 0
                it "gains chakra per stack" $
                    toList chakra `shouldBe` replicate stacks Blood

        useOn Enemy "Mental Invasion" do
            act
            as Enemy $ self $ apply stacks [Invulnerable All]
            turns $ stacks - 2
            as XAlly $ apply 0 [Reveal]
            targetHarmed <- (`is` Reveal) <$> P.nTarget
            turns 1
            as XAlly $ apply 0 [Reveal]
            targetHarmed' <- (`is` Reveal) <$> P.nTarget
            factory
            self factory
            act
            as Enemy $ apply 0 [Reveal]
            userHarmed <- (`is` Reveal) <$> P.nUser
            withClass Mental $ as Self $ return ()
            as Enemy $ apply 0 [Seal]
            userHarmed' <- (`is` Seal) <$> P.nUser
            return do
                it "does not remove invulnerability" $
                    not targetHarmed
                it "does shorten durability"
                    targetHarmed'
                it "does not immediately make user invulnerable"
                    userHarmed
                it "does make user invulnerable with using mental harm" $
                    not userHarmed'
  where
    describeCharacter = describeCategory Original
    stacks = 3
    targetDmg = 55
