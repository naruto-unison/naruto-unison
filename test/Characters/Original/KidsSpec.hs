module Characters.Original.KidsSpec (spec) where

import TestImport

import Test.Hspec

import qualified Class.Play as P
import qualified Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" \useOn -> do
        let maxClones      = 5
            getClonesSpent = (maxClones -) . Ninja.numActive "Shadow Clone"
                             <$> P.nUser
        useOn Enemy "Naruto Uzumaki Barrage" do
            self $ addStacks "Shadow Clone" maxClones
            act
            clonesSpent  <- getClonesSpent
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "spends 1 clone" $
                    clonesSpent `shouldBe` 1
                it "damages target" $
                    100 - targetHealth `shouldBe` 
                        10 + 5 * maxClones + 5 * clonesSpent
        useOn Enemy "Rasengan" do
            self $ addStacks "Shadow Clone" maxClones
            act
            clonesSpent   <- getClonesSpent
            targetHealth  <- Ninja.health <$> P.nTarget
            targetStunned <- (`is` Stun All) <$> P.nTarget
            return do
                it "spends 2 clones" $
                    clonesSpent `shouldBe` 2
                it "damages target" $
                    100 - targetHealth `shouldBe` 30 + 5 * clonesSpent
                it "stuns target"
                    targetStunned
        useOn Enemy "Shadow Clones" do
            let targetDmg = 50
            act
            userStacks <- Ninja.numActive "Shadow Clone" <$> P.nUser
            targetTurn $ damage targetDmg
            turns 1
            targetTurn $ damage targetDmg
            turns 1
            targetHealth   <- Ninja.health <$> P.nTarget
            userHealth     <- Ninja.health <$> P.nUser
            userStacks'    <- Ninja.numActive "Shadow Clone" <$> P.nUser
            let clonesSpent = userStacks - userStacks'
            return do
                it "grants 6 clones" $
                    userStacks `shouldBe` 6
                it "damages target when damaged" $
                    100 - targetHealth `shouldBe` 5 * clonesSpent
                it "spends 1 clone per damaged" $
                    clonesSpent `shouldBe` 2
                it "reduces damage" $
                    100 - userHealth `shouldBe` 
                        (targetDmg - userStacks * 5)
                        + (targetDmg - (userStacks - 1) * 5)

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
