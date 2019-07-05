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
                    100 - targetHealth `shouldBe` 10 + 5 * clonesSpent
        useOn Enemy "Rasengan" do
            self $ addStacks "Shadow Clone" maxClones
            act
            clonesSpent   <- getClonesSpent
            targetHealth  <- Ninja.health <$> P.nTarget
            targetStunned <- Ninja.is (Stun All) <$> P.nTarget
            return do
                it "spends 2 clones" $
                    clonesSpent `shouldBe` 2
                it "damages target" $
                    100 - targetHealth `shouldBe` 30 + 5 * clonesSpent
                it "stuns target"
                    targetStunned
        useOn Enemy "Shadow Clones" do
            act
            userStacks <- Ninja.numActive "Shadow Clone" <$> P.nUser
            targetTurn $ damage 50
            turns 1
            targetTurn $ damage 50
            turns 1
            targetHealth   <- Ninja.health <$> P.nTarget
            userHealth     <- Ninja.health <$> P.nUser
            userStacks'    <- Ninja.numActive "Shadow Clone" <$> P.nUser
            let clonesSpent = userStacks' - userStacks
            return do
                it "grants 5 clones" $
                    userStacks `shouldBe` 6
                it "damages target when damaged" $
                    100 - targetHealth `shouldBe` 5 * clonesSpent
                it "spends 1 clone per damaged" $
                    clonesSpent `shouldBe` 2
                it "reduces damage" $
                    100 - userHealth `shouldBe` (50 - userStacks * 5)
                                              + (50 - (userStacks - 1) * 5)

    describeCharacter "Kiba Inuzuka" \useOn -> do
        useOn Enemy "Two-Headed Wolf" do
            act
            targetTurn $ damage 30
            turns 4
            userHealth   <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 3 * 15
                it "reduces damage to user" $
                    100 - userHealth `shouldBe` 15
  where
    describeCharacter = describeCategory Original
