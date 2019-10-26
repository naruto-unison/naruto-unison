{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.VersionsSpec (spec) where

import Import

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "One-Tailed Naruto" \useOn -> do
        useOn Enemy "Tailed Beast Rasengan" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            userHealth <- Ninja.health <$> P.nUser
            factory
            channel "Tailed Beast Chakra Arms"
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            self factory
            channel "Inner Chakra Mode"
            act
            targetHealth'' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 35
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "damages user" $
                    100 - userHealth `shouldBe` 5
                it "deals bonus damage during Tailed Beast Chakra Arms" $
                    targetHealth - targetHealth' `shouldBe` 10
                it "deals less damage during Inner Chakra Mode" $
                    targetHealth'' - targetHealth `shouldBe` 10

        useOn Enemies "Tailed Beast Chakra Arms" do
            act
            turns 5
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15 * 3

        useOn Self "Inner Chakra Mode" do
            setHealth stacks
            act
            turns 9
            userHealth <- Ninja.health <$> P.nUser
            factory
            act
            enemyTurn $ damage targetDmg
            userHealth' <- Ninja.health <$> P.nUser
            return do
                it "heals user" $
                    userHealth `shouldBe` stacks + 15 * 5
                it "reduces damage" $
                    targetDmg - (100 - userHealth') `shouldBe` 10

    describeCharacter "Curse Mark Sasuke" \useOn -> do
        useOn Enemy "Chidori" do
            act
            enemyTurn $ damage targetDmg
            enemyHealth <- Ninja.health <$> P.nTarget
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "damages target" $
                    100 - enemyHealth `shouldBe` 45
                it "weakens target" $
                    targetDmg - (100 - userHealth) `shouldBe` 20

        useOn Enemy "Dark Void" do
            act
            targetStunned <- Effects.stun <$> P.nTarget
            targetHealth <- Ninja.health <$> P.nTarget
            isInvuln <- Effects.invulnerable <$> P.nTarget
            isAlone <- (`is` Alone) <$> P.nTarget
            turns 3
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "makes target invulnerable" $
                    isInvuln `shouldBe` [All]
                it "isolates target"
                    isAlone
                it "deals no damage initially" $
                    targetHealth `shouldBe` 100
                it "damages target after 2 turns" $
                    100 - targetHealth' `shouldBe` 55

        useOn Self "Curse Mark" do
            act
            userHealth <- Ninja.health <$> P.nUser
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            return do
                it "damages user" $
                    100 - userHealth `shouldBe` 20
                it "makes user invulnerable" $
                    not harmed

    describeCharacter "Drunken Lee" \useOn -> do
        useOn REnemy "Unpredictable Assault" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            self factory
            channel "Drunken Fist"
            act
            targetHealth'' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "increases in damage very time" $
                    targetHealth - targetHealth' `shouldBe` 5
                it "deals bonus damage during Drunken Fist" $
                    targetHealth - targetHealth'' `shouldBe` 5

        useOn Ally "Drunken Counter" do
            act
            usedSkill <- Ninja.hasOwn "Unpredictable Assault" <$> P.nUser
            withClass Physical $ enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nTarget
            usedSkill' <- Ninja.hasOwn "Unpredictable Assault" <$> P.nUser
            return do
                it "counters on ally" $
                    not harmed
                it "uses Unpredictable Assault when countered"
                    usedSkill'
                it "does not use Unpredictable Assault if not countered" $
                    not usedSkill

        useOn Enemy "Drunken Fist" do
            enemyTurn $ apply 0 [Plague]
            act
            harmed <- (`is` Plague) <$> P.nUser
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "ignores harmful effects" $
                    not harmed
                it "damages target" $
                    100 - targetHealth `shouldBe` 15 * 3

    describeCharacter "Shukaku Gaara" \useOn -> do
        useOn Enemy "Desert Hand" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            defense <- Ninja.totalDefense <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "defends user" $
                    defense `shouldBe` 10

        useOn Enemy "Monstrous Sand Arm" do
            act
            turns stacks
            targetHealth <- Ninja.health <$> P.nTarget
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            turns stacks
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 10 * (stacks + 1)
                it "counters target" $
                    not harmed
                it "ends when countered" $
                    targetHealth - targetHealth' `shouldBe` 0

        useOn Self "Sand Transformation" do
            act
            turns 7
            defense <- Ninja.totalDefense <$> P.nUser
            has <- Ninja.hasOwn "Tailed Beast Form" <$> P.nUser
            return do
                it "defends user" $
                    defense `shouldBe` 10 * 5
                it "transforms user"
                    has

        useOn Enemy "Wind Bullet" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 60

        useOn Self "Shukaku Full Release" do
            act
            damage $ targetDmg `quot` 3
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "doubles damage" $
                    100 - targetHealth `shouldBe` 2 * (targetDmg `quot` 3)

    describeCharacter "Rehabilitated Gaara" \useOn -> do
        useOn Enemy "Sand Shower" do
            act
            defense <- Ninja.totalDefense <$> P.nUser
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "defends user" $
                    defense `shouldBe` 35
                it "damages target" $
                    100 - targetHealth `shouldBe` 15 * 3

        useOn Enemies "Sand Burial Prison" do
            act
            withClass NonMental $ enemyTurn $ return ()
            targetExhausted <- Effects.exhaust [NonMental] <$> P.nTarget
            otherExhausted <- Effects.exhaust [NonMental] <$> (allyOf =<< P.target)
            return do
                it "exhausts enemies" $
                    otherExhausted `shouldBe` [Rand]
                it "ends if target uses new skill" $
                    targetExhausted `shouldBe` 0

        useOn Enemies "Giant Sand Burial" do
            defend 0 100
            tag' "Sand Burial Prison" 0
            act
            defense <- Ninja.totalDefense <$> P.nTarget
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "demolishes destructible defense" $
                    defense `shouldBe` 0
                it "damages target" $
                    100 - targetHealth `shouldBe` 40

        useOn Enemies "Sand Tsunami" do
            act
            turns 8
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            factory
            self factory
            act
            cancelChannel "Sand Tsunami"
            self $ defend 0 targetDmg
            enemyTurn $ self $ defend 0 targetDmg
            userDefense <- Ninja.totalDefense <$> P.nUser
            targetDefense <- Ninja.totalDefense <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 4 * 15
                it "adds to team defense" $
                    userDefense - targetDmg `shouldBe` 10
                it "reduces enemy defense" $
                    targetDmg - targetDefense `shouldBe` 10
  where
    describeCharacter = describeCategory Original
    stacks = 3
    targetDmg = 55
