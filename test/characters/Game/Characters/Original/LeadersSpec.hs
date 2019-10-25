{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.LeadersSpec (spec) where

import Import

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "Orochimaru" \useOn -> do
        useOn Enemy "Kusanagi" do
            enemyTurn $ barrier 0 30
            defend 0 20
            act
            defense <- Ninja.totalDefense <$> P.nTarget
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "demolishes" $
                    defense `shouldBe` 0
                it "damages target" $
                    100 - targetHealth `shouldBe` 25

        useOn Enemy "Curse Mark" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            (_, chakra) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
                it "grants chakra" $
                    toList chakra `shouldBe` [Blood]

        useOn Ally "Curse Mark" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            (chakra, _) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
                it "grants chakra" $
                    toList chakra `shouldBe` [Blood]

        useOn Enemy "Major Summoning: Manda" do
            act
            turns stacks
            targetHealth <- Ninja.health <$> P.nTarget
            (chakra, _) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 45
                it "grants chakra every turn" $
                    toList chakra `shouldBe` replicate (stacks + 1) Blood

        useOn Enemy "Paralyzing Bite" do
            act
            targetStunned <- Effects.stun <$> P.nTarget
            targetHealth <- Ninja.health <$> P.nTarget
            turns 1
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "deals no damage initially..." $
                    100 - targetHealth `shouldBe` 0
                it "...but damages target after 1 turn" $
                    targetHealth - targetHealth' `shouldBe` 25

    describeCharacter "Jiraiya" \useOn -> do
        useOn Enemy "Giant Flame Bomb" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            allyHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "damages others" $
                    100 - allyHealth `shouldBe` 10

        useOn Enemy "Summoning: Toad Mouth Trap" do
            act
            exposed <- targetIsExposed
            enemyTurn $ damage targetDmg
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            factory
            self factory
            act
            enemyTurn $ afflict 5
            enemyTurn $ apply 0 [Reveal]
            harmed' <- (`is` Reveal) <$> P.nUser
            return do
                it "exposes target"
                    exposed
                it "makes allies invulnerable when damaged" $
                    not harmed
                it "...except with affliction damage"
                    harmed'

        useOn Enemy "Major Summoning: Gamabunta" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            turns 10
            afflict targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 25
                it "weakens to affliction" $
                    (targetHealth - targetHealth') - targetDmg `shouldBe` 5

        useOn Enemies "Toad Oil Bomb" do
            act
            turns 5
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15 * 2

    describeCharacter "Tsunade" \useOn -> do
        useOn Enemy "Heavenly Kick of Pain" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "stuns target" $
                    targetStunned `shouldBe` [Physical, Mental]

        useOn Self "Mitotic Regeneration" do
            setHealth 2
            enemyTurn $ apply 0 [Expose]
            act
            userHealth <- Ninja.health <$> P.nUser
            userHarmed <- (`is` Expose) <$> P.nUser
            return do
                it "heals user" $
                    userHealth `shouldBe` 100
                it "cures harmful effects" $
                    not userHarmed

        useOn Allies "Major Summoning: Katsuyu" do
            enemyTurn $ damage targetDmg
            act
            targetHealth <- Ninja.health <$> P.nTarget
            setHealth 100
            enemyTurn $ damage targetDmg
            turns stacks
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "heals targets" $
                    targetDmg - (100 - targetHealth) `shouldBe` 40 + 5
                it "heals every turn" $
                    targetDmg - (100 - targetHealth') `shouldBe` 5 * stacks

        useOn Allies "Slug Division" do
            enemyTurn $ damage targetDmg
            act
            targetHealth <- Ninja.health <$> P.nTarget
            defense <- Ninja.totalDefense <$> P.nTarget
            return do
                it "heals targets" $
                    targetDmg - (100 - targetHealth) `shouldBe` 10
                it "defends targets" $
                    defense `shouldBe` 10

    describeCharacter "Hiruzen Sarutobi" \useOn -> do
        useOn Enemy "Dragon Flame Bomb" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            afflict targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "weakens target to affliction" $
                    (targetHealth - targetHealth') - targetDmg `shouldBe` 10

        useOn Enemy "Reaper Death Seal" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            userHealth <- Ninja.health <$> P.nUser
            targetStunned <- Effects.stun <$> P.nTarget
            userStunned <- Effects.stun <$> P.nUser
            targetExposed <- targetIsExposed
            self factory
            targetStunned' <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 40
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "exposes target"
                    targetExposed
                it "damages user" $
                    100 - userHealth `shouldBe` 20
                it "stuns user" $
                    userStunned `shouldBe` [All]
                it "resets on death" $
                    targetStunned' `shouldBe` []
  where
    describeCharacter = describeCategory Original
    stacks = 3
    targetDmg = 55
