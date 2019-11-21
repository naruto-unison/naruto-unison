{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.LeadersSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Orochimaru" do
        useOn Ally "Curse Mark" do
            it "grants chakra" do
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [])
            it "sacrifices health" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15
        useOn Enemy "Curse Mark" do
            it "grants chakra" do
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Blood])
            it "sacrifices health" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15

        useOn Enemy "Major Summoning: Manda" do
            it "alternates" do
                act
                hasSkill "Paralyzing Bite" <$> nUser

        useOn Enemy "Paralyzing Bite" do
            it "stuns target" do
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "deals no damage initially" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "damages after 1 turn" do
                act
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 25

    describeCharacter "Jiraiya" do
        useOn Enemies "Summoning: Toad Mouth Trap" do
            it "grants invulnerability if damaged" do
                act
                as Enemy $ damage 5
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nUser
            it "does not affect affliction damage" do
                act
                as Enemy $ afflict 5
                as Enemy $ apply Permanent [Reveal]
                (`is` Reveal) <$> nUser

        useOn Enemies "Major Summoning: Gamabunta" do
            it "alternates" do
                act
                hasSkill "Toad Oil Bomb" <$> nUser

    describeCharacter "Tsunade" do
        useOn Allies "Major Summoning: Katsuyu" do
            it "alternates" do
                act
                hasSkill "Slug Division" <$> nUser

    describeCharacter "Hiruzen Sarutobi" do
        useOn Enemy "Reaper Death Seal" do
            it "stuns target" do
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "stuns user" do
                act
                userStunned <- Effects.stun <$> nUser
                userStunned `shouldBe` [All]
            it "damages target every turn" do
                act
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * 40
            it "damages user every turn" do
                act
                turns 1
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` 2 * 20
            it "ends if Hiruzen dies" do
                act
                self $ as XEnemies kill
                targetHealth <- health <$> nUser
                targetHealth `shouldBe` 0

        useOn Allies "Major Summoning: Enma" do
            it "alternates" do
                act
                hasSkill "Adamantine Prison" <$> nUser
  where
    describeCharacter = describeCategory Original
