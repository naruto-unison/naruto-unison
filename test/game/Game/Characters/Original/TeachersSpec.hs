{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.TeachersSpec (spec) where

import Import
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Iruka Umino" do
        useOn Enemy "Shuriken Throw" do
            it "deals bonus damage per health lost" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                targeting Self . damage $ 25 * testStacks
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10 * testStacks

        useOn Enemy "Capture and Arrest" do
            it "does not damage normally" do
                Sim.act
                Sim.turns 1
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "damages target if they harm" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                targetHealth <- target health
                100 - targetHealth `shouldBe` 40
            it "makes target vulnerable if they harm" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                setHealth 100
                Sim.withClass Physical $ damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 25

    describeCharacter "Mizuki" do
        useOn Enemy "Kunai Assault" do
            it "takes turns normally" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15
            it "damages instantly during Successful Ambush" do
                targeting Self $ tag' "Successful Ambush" Permanent
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30

        useOn Enemy "Execution Shuriken" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10
            it "deals bonus damage per target health lost" do
                damage (20 * 2)
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 * 2 + 10 + 2 * 10
            it "deals bonus damage during Successful Ambush" do
                targeting Self $ tag' "Successful Ambush" Permanent
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10 + 30

        useOn Self "Genjutsu Ambush Tactics" do
            it "does not make invulnerable instantly" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "does not make invulnerable if harmed" do
                Sim.act
                Sim.as Enemy $ damage dmg
                Sim.turns 1
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "makes invulnerable if not harmed" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Plague ]
                Sim.turns 1
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "tags user if not harmed" do
                Sim.act
                Sim.turns 1
                user has "Successful Ambush"

    describeCharacter "Anko Mitarashi" do
        useOn Enemy "Dual Pin" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Twin Snake Sacrifice"

        useOn Enemy "Twin Snake Sacrifice" do
            it "kills target" do
                Sim.use "Dual Pin"
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "kills user" do
                Sim.use "Dual Pin"
                targeting Self $ apply Permanent [ Endure ]
                Sim.act
                userHealth <- user health
                userHealth `shouldBe` 0

    describeCharacter "Kakashi Hatake" do
        useOn Enemy "Lightning Blade" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 50
            it "kills if target has Summoning: Ninja Hounds" do
                Sim.use "Summoning: Ninja Hounds"
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0

    describeCharacter "Kurenai Yuhi" do
        useOn Enemy "Demonic Illusion: Entrap" do
            it "adds stacks" do
                replicateM_ testStacks Sim.act
                stacks <- user numStacks "Illusion"
                stacks `shouldBe` 3

        useOn Self "Illusory Tree Meld" do
            it "adds destructible defense per Illusion" do
                targeting Self $ addStacks "Illusion" testStacks
                Sim.act
                defense <- user totalDefense
                defense `shouldBe` 10 + 5 * testStacks

        useOn Enemy "Demonic Illusion: Sylvan Fetters" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Sylvan Fetters Attack"

    describeCharacter "Asuma Sarutobi" do
        useOn Enemies "Flying Swallow" do
            it "alternates self" do
                Sim.act
                user $ hasSkill "Finishing Blow"
            it "alternates other" do
                Sim.act
                user $ hasSkill "Flying Kick"
            it "lasts longer per Sharpen Blades" do
                replicateM_ testStacks $ Sim.use "Sharpen Blades"
                Sim.act
                Sim.turns $ 5 + testStacks
                targetHealth <- target health
                100 - targetHealth `shouldBe` (2 + testStacks) * 15

        useOn Ally "Self-Sacrifice" do
            it "redirects from ally" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> target (`is` Reveal)
            it "redirects to user" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)

    describeCharacter "Might Guy" do
        useOn Self "Sixth Gate Opening" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Severe Leaf Hurricane"

        useOn Enemy "Counter Punch" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "damages countered target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30

    describeCharacter "Baki" do
        useOn Ally "Flak Jacket" do
            it "defends target" do
                Sim.act
                targetDefense <- target totalDefense
                targetDefense `shouldBe` 50
            it "protects target from effects" do
                Sim.as Enemy $ apply Permanent [ Plague ]
                Sim.act
                not <$> target (`is` Plague)
            it "ends when defense is destroyed" do
                Sim.as Enemy $ apply Permanent [ Plague ]
                Sim.act
                Sim.as Enemy demolishAll
                target (`is` Plague)
  where
    describeCharacter = describeCategory Original
    dmg = 55
    testStacks = 3
