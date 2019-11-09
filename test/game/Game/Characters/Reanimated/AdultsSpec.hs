{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.AdultsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Pakura" do
        useOn Enemy "Scorch Style" do
            it "harms harm" do
                act
                self $ as XEnemies $ return ()
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Super Steam Kill" do
            it "damages target" do
                apply 0 [AntiChannel]
                use "Scorch Style"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 40

    describeCharacter "Gari" do
        useOn Enemy "Exploding Palm" do
            it "deals stacking damage" do
                replicateM_ stacks act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 * stacks

        useOn Enemy "Ground Pound" do
            it "damages on harm" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 25
            it "reduces damage" do
                act
                as Enemy $ return ()
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` dmg `quot` 4
            it "heals user" do
                as Enemy $ damage dmg
                act
                as Enemy $ return ()
                turns 3
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 2 * 15

    describeCharacter "Ginkaku" do
        useOn Enemy "Seven Stars Blade" do
            it "adds Spirit Words" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Spirit Word" <$> nTarget
                targetStacks `shouldBe` stacks

        useOn Enemy "Amber Purification Jar" do
            it "adds Spirit Words" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Spirit Word" <$> nTarget
                targetStacks `shouldBe` stacks

        useOn Enemy "Benihisago" do
            it "damages target per stack" do
                as XEnemies $ addStacks "Spirit Word" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 + 5 * stacks
            it "adds Spirit Words" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Spirit Word" <$> nTarget
                targetStacks `shouldBe` stacks
            it "increases Scroll of Fire" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Scroll of Fire" <$> nTarget
                targetStacks `shouldBe` stacks

    describeCharacter "Kinkaku" do
        useOn Enemy "Leaf Fan" do
            it "adds Spirit Words" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Spirit Word" <$> nTarget
                targetStacks `shouldBe` stacks

        useOn Enemy "Gold Rope" do
            it "damages on harm stacking" do
                replicateM_ 2 act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * 35
            it "adds Spirit Words" do
                replicateM_ stacks act
                as Enemy $ return ()
                targetStacks <- numAnyStacks "Spirit Word" <$> nTarget
                targetStacks `shouldBe` stacks

        useOn Enemies "Scroll of Fire" do
            it "adds Spirit Words" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Spirit Word" <$> nTarget
                targetStacks `shouldBe` stacks
            it "deals bonus damage from stacks" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                as XEnemies $ addStacks "Scroll of Fire" stacks
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * stacks

    describeCharacter "Toroi" do
        useOn Enemy "Demon Wind Shuriken" do
            it "deals bonus damage per Conserving Bee Twin Blades" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Magnetic Field"
                replicateM_ stacks $ use "Conserving Bee Twin Blades"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * stacks
            it "deals bonus damage per Magnetic Current" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                replicateM_ stacks $ use "Magnetic Current"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * stacks
            it "causes harmers to ignore healing" do
                act
                as Enemy $ return ()
                heal 100
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20

        useOn Self "Magnetic Field" do
            it "alternates" do
                act
                hasSkill "Conserving Bee Twin Blades" <$> nUser

        useOn Enemy "Conserving Bee Twin Blades" do
            it "deals bonus damage per Magnetic Current" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                replicateM_ stacks $ use "Magnetic Current"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * stacks

        useOn Enemies "Magnetic Current" do
            it "deals bonus damage per Conserving Bee Twin Blades" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Magnetic Field"
                replicateM_ stacks $ use "Conserving Bee Twin Blades"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * stacks

    describeCharacter "Fukai" do
        useOn Enemy "Tailed Beast Bomb Barrage" do
            it "damages targets per Chakra Arms" do
                replicateM_ stacks $ use "Chakra Arms"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 + 5 * stacks

        useOn Enemy "Lariat" do
            it "damages targets per Chakra Arms" do
                replicateM_ stacks $ use "Chakra Arms"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 5 * stacks

    describeCharacter "Chiyo" do
        useOn Enemy "Possum" do
            it "counters target" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "stuns countered" do
                act
                as Enemy $ return ()
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [Physical]

    describeCharacter "Chūkichi" do
        useOn Enemy "Silent Killing" do
            it "deals bonus damage during Hidden Frost" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Hidden Frost"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    stacks = 3
