{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.OrganizationsSpec (spec) where

import Import

import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Shisui Uchiha" do
        useOn Self "Susanoo" do
            it "adds stacks" do
                Sim.act
                Sim.turns stacks
                numStacks <- userStacks "Susanoo"
                numStacks `shouldBe` 1 + stacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Tsukumo"
            it "clears stacks when broken" do
                Sim.act
                Sim.turns stacks
                Sim.as Enemy demolishAll
                defense <- user totalDefense
                defense `shouldBe` 0

        useOn Self "Teleportation Technique" do
            it "damages harm" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 15

        useOn Enemy "Kotoamatsukami" do
            it "depletes on harm" do
                gain [Blood, Gen]
                Sim.act
                Sim.as Enemy $ return ()
                chakras <- gameChakras
                chakras `shouldBe` ([], [Gen])

    describeCharacter "Yamato" do
        useOn Ally "Wood Clone" do
            it "counters on target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                target $ not . (`is` Reveal)
            it "counters with defense" do
                Sim.act
                Sim.as Enemy $ return ()
                targetDefense <- target totalDefense
                targetDefense `shouldBe` 20
            it "damages countered" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 20
            it "recharges if countered" do
                Sim.use "Tenth Edict on Enlightenment"
                Sim.act
                Sim.as Enemy $ return ()
                userCharges <- user charges
                userCharges `shouldBe` mempty
            it "does not recharge otherwise" do
                Sim.use "Tenth Edict on Enlightenment"
                Sim.act
                Sim.turns 1
                userCharges <- user charges
                userCharges `shouldNotBe` mempty

        useOn Ally "Four-Pillar Architecture" do
            it "recharges" do
                Sim.use "Tenth Edict on Enlightenment"
                Sim.act
                userCharges <- user charges
                userCharges `shouldBe` mempty
        useOn Enemy "Four-Pillar Architecture" do
            it "recharges" do
                Sim.use "Tenth Edict on Enlightenment"
                Sim.act
                userCharges <- user charges
                userCharges `shouldBe` mempty

    describeCharacter "Torune Aburame" do
        useOn Enemy "Nano-Sized Venom Beetles" do
            it "does not defend user again" do
                Sim.act
                defense <- user totalDefense
                Sim.act
                defense' <- user totalDefense
                defense' `shouldBe` defense
            it "applies a Venom Beetle" do
                Sim.act
                targetHas "Venom Beetle"
            it "applies a Venom Beetle to destroyer of defense" do
                Sim.act
                Sim.as Enemy demolishAll
                numStacks <- targetStacks "Venom Beetle"
                numStacks `shouldBe` 2

        useOn Enemies "Jar of Poison" do
            it "does not defend user again" do
                Sim.act
                defense <- user totalDefense
                Sim.act
                defense' <- user totalDefense
                defense' `shouldBe` defense
            it "applies a Venom Beetle to targets" do
                Sim.act
                Sim.at XEnemies $ targetHas "Venom Beetle"
            it "applies a Venom Beetle to destroyer of defense" do
                Sim.act
                Sim.as Enemy demolishAll
                numStacks <- targetStacks "Venom Beetle"
                numStacks `shouldBe` 2

        useOn Enemy "Venom Explosion" do
            it "depletes chakra per Venom Beetle" do
                gain [Blood, Gen, Nin, Tai]
                addStacks "Venom Beetle" 2
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Nin, Tai])

    describeCharacter "Fū Yamanaka" do
        useOn Enemy "Tantō Slash" do
            it "deals bonus damage if target has Mind Transfer" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Mind Transfer"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 15

        useOn Ally "Mind Transfer Puppet Curse" do
            it "counters on target" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ apply Permanent [Reveal]
                target $ not . (`is` Reveal)
            it "teaches countered" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                hasSkill "Puppet Curse: Attack" <$> Sim.targets Enemy
            it "teaches countered B" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                hasSkill "Puppet Curse: Defend" <$> Sim.targets Enemy
            it "teaches user" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                user $ hasSkill "Unnamed"

    describeCharacter "Sasuke Uchiha" do
        useOn Enemy "Chidori Stream" do
            it "counters enemies" do
                Sim.act
                Sim.withClass NonMental $ Sim.as XEnemies $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "damages countered" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemies $ apply Permanent [Reveal]
                targetHealth <- health <$> Sim.targets Enemies
                100 - targetHealth `shouldBe` 10
            it "alternates" do
                Sim.act
                user $ hasSkill "Kusanagi"

        useOn Enemy "Dragon Flame" do
            it "damages attackers" do
                Sim.act
                setHealth 100
                Sim.as Enemy $ apply Permanent [Reveal]
                targetHealth <- target health
                100 - targetHealth `shouldBe` 5

        useOn Enemy "Kirin" do
            it "cannot be used without Dragon Flame" do
                Sim.as Enemy $ return ()
                Sim.use "Kirin"
                targetHealth <- target health
                targetHealth `shouldBe` 100
            it "can be used after Dragon Flame" do
                apply Permanent [AntiChannel]
                Sim.use "Dragon Flame"
                Sim.as Enemy $ return ()
                setHealth 100
                Sim.use "Kirin"
                targetHealth <- target health
                targetHealth `shouldNotBe` 100
            it "can only be used once after Dragon Flame" do
                apply Permanent [AntiChannel]
                Sim.use "Dragon Flame"
                setHealth 100
                Sim.as Enemy $ return ()
                Sim.use "Kirin"
                Sim.as Enemy $ return ()
                setHealth 100
                Sim.use "Kirin"
                targetHealth <- target health
                targetHealth `shouldBe` 100
  where
    describeCharacter = describeCategory Shippuden
    stacks = 3
