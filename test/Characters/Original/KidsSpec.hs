module Characters.Original.KidsSpec (spec) where

import TestImport

import Test.Hspec

import qualified Class.Play as P
import qualified Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" \useOn -> do
        useOn Enemy "Naruto Uzumaki Barrage" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            self $ tag' "Shadow Clones" 4
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "deals bonus damage during Shadow Clones" $
                    targetHealth - targetHealth' `shouldBe` 30
        useOn Enemy "Rasengan" do
            self $ tag' "Shadow Clones" 4
            act
            targetHealth  <- Ninja.health <$> P.nTarget
            targetStunned <- (`is` Stun All) <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 45
                it "stuns target"
                    targetStunned
        useOn Enemy "Shadow Clones" do
            let targetDmg = 50
            act
            targetTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "reduces damage" $
                    100 - userHealth `shouldBe` targetDmg - 15

    describeCharacter "Kiba Inuzuka" \useOn -> do
        useOn Enemy "Two-Headed Wolf" do
            let targetDmg = 30
            act
            targetTurn $ damage targetDmg
            turns 4
            userHealth   <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 3 * 15
                it "reduces damage to user" $
                    100 - userHealth `shouldBe` targetDmg - 15
  where
    describeCharacter = describeCategory Original
