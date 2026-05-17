{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.LeadersSpec (spec) where
import qualified Sim as Sim

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Orochimaru" do
        useOn Ally "Curse Mark" do
            it "grants chakra" do
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [])
            it "sacrifices health" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15
        useOn Enemy "Curse Mark" do
            it "grants chakra" do
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Blood])
            it "sacrifices health" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15

        useOn Enemy "Major Summoning: Manda" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Paralyzing Bite"

        useOn Enemy "Paralyzing Bite" do
            it "stuns target" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "deals no damage initially" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "damages after 1 turn" do
                Sim.act
                Sim.turns 1
                targetHealth <- target health
                100 - targetHealth `shouldBe` 25

    describeCharacter "Jiraiya" do
        useOn Enemies "Summoning: Toad Mouth Trap" do
            it "grants invulnerability if damaged" do
                Sim.act
                Sim.as Enemy $ damage 5
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "does not affect affliction damage" do
                Sim.act
                Sim.as Enemy $ afflict 5
                Sim.as Enemy $ apply Permanent [Reveal]
                user (`is` Reveal)

        useOn Enemies "Major Summoning: Gamabunta" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Toad Oil Bomb"

    describeCharacter "Tsunade" do
        useOn Allies "Major Summoning: Katsuyu" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Slug Division"

    describeCharacter "Hiruzen Sarutobi" do
        useOn Enemy "Reaper Death Seal" do
            it "stuns target" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "stuns user" do
                Sim.act
                userStunned <- user Effects.stun
                userStunned `shouldBe` [All]
            it "damages target every turn" do
                Sim.act
                Sim.turns 1
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * 40
            it "damages user every turn" do
                Sim.act
                Sim.turns 1
                userHealth <- user health
                100 - userHealth `shouldBe` 2 * 20
            it "ends if Hiruzen dies" do
                Sim.act
                self $ Sim.as XEnemies kill
                targetHealth <- user health
                targetHealth `shouldBe` 0

        useOn Allies "Major Summoning: Enma" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Adamantine Prison"
  where
    describeCharacter = describeCategory Original
