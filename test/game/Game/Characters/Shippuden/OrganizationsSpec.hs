{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.OrganizationsSpec (spec) where

import Import

spec :: Spec
spec = parallel do
    describeCharacter "Shisui Uchiha" do
        useOn Self "Susanoo" do
            it "adds stacks" do
                act
                turns stacks
                userStacks <- numAnyStacks "Susanoo" <$> nUser
                userStacks `shouldBe` 1 + stacks
            it "alternates" do
                act
                hasSkill "Tsukumo" <$> nUser
            it "clears stacks when broken" do
                act
                turns stacks
                as Enemy demolishAll
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 0

        useOn Self "Teleportation Technique" do
            it "damages harm" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 15

        useOn Enemy "Kotoamatsukami" do
            it "depletes on harm" do
                gain [Blood, Gen]
                act
                as Enemy $ return ()
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Gen])

    describeCharacter "Yamato" do
        useOn Ally "Wood Clone" do
            it "counters on target" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "counters with defense" do
                act
                as Enemy $ return ()
                targetDefense <- totalDefense <$> nTarget
                targetDefense `shouldBe` 20
            it "damages countered" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 20
            it "recharges if countered" do
                use "Tenth Edict on Enlightenment"
                act
                as Enemy $ return ()
                userCharges <- charges <$> nUser
                userCharges `shouldBe` mempty
            it "does not recharge otherwise" do
                use "Tenth Edict on Enlightenment"
                act
                turns 1
                userCharges <- charges <$> nUser
                userCharges `shouldNotBe` mempty

        useOn Ally "Four-Pillar Architecture" do
            it "recharges" do
                use "Tenth Edict on Enlightenment"
                act
                userCharges <- charges <$> nUser
                userCharges `shouldBe` mempty
        useOn Enemy "Four-Pillar Architecture" do
            it "recharges" do
                use "Tenth Edict on Enlightenment"
                act
                userCharges <- charges <$> nUser
                userCharges `shouldBe` mempty

    describeCharacter "Torune Aburame" do
        useOn Enemy "Nano-Sized Venom Beetles" do
            it "does not defend user again" do
                act
                userDefense <- totalDefense <$> nUser
                act
                userDefense' <- totalDefense <$> nUser
                userDefense' `shouldBe` userDefense
            it "applies a Venom Beetle" do
                act
                has "Venom Beetle" <$> user <*> nTarget
            it "applies a Venom Beetle to destroyer of defense" do
                act
                as Enemy demolishAll
                numStacks <- numAnyStacks "Venom Beetle" <$> nTarget
                numStacks `shouldBe` 2

        useOn Enemies "Jar of Poison" do
            it "does not defend user again" do
                act
                userDefense <- totalDefense <$> nUser
                act
                userDefense' <- totalDefense <$> nUser
                userDefense' `shouldBe` userDefense
            it "applies a Venom Beetle to targets" do
                act
                has "Venom Beetle" <$> user <*> get XEnemies
            it "applies a Venom Beetle to destroyer of defense" do
                act
                as Enemy demolishAll
                numStacks <- numAnyStacks "Venom Beetle" <$> nTarget
                numStacks `shouldBe` 2

        useOn Enemy "Venom Explosion" do
            it "depletes chakra per Venom Beetle" do
                gain [Blood, Gen, Nin, Tai]
                addStacks "Venom Beetle" 2
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Nin, Tai])

    describeCharacter "Fū Yamanaka" do
        useOn Enemy "Tantō Slash" do
            it "deals bonus damage if target has Mind Transfer" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Mind Transfer"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 15

        useOn Ally "Mind Transfer Puppet Curse" do
            it "counters on target" do
                act
                withClass NonMental $ as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "teaches countered" do
                act
                withClass NonMental $ as Enemy $ return ()
                hasSkill "Puppet Curse: Attack" <$> get Enemy
            it "teaches countered B" do
                act
                withClass NonMental $ as Enemy $ return ()
                hasSkill "Puppet Curse: Defend" <$> get Enemy
            it "teaches user" do
                act
                withClass NonMental $ as Enemy $ return ()
                hasSkill "Unnamed" <$> nUser

    describeCharacter "Sasuke Uchiha" do
        useOn Enemy "Chidori Stream" do
            it "counters enemies" do
                act
                withClass NonMental $ as XEnemies $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered" do
                act
                withClass NonMental $ as Enemies $ apply 0 [Reveal]
                targetHealth <- health <$> get Enemies
                100 - targetHealth `shouldBe` 10
            it "alternates" do
                act
                hasSkill "Kusanagi" <$> nUser

        useOn Enemy "Dragon Flame" do
            it "damages attackers" do
                act
                setHealth 100
                as Enemy $ apply 0 [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5

        useOn Enemy "Kirin" do
            it "cannot be used without Dragon Flame" do
                as Enemy $ return ()
                use "Kirin"
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 100
            it "can be used after Dragon Flame" do
                apply 0 [AntiChannel]
                use "Dragon Flame"
                as Enemy $ return ()
                setHealth 100
                use "Kirin"
                targetHealth <- health <$> nTarget
                targetHealth `shouldNotBe` 100
            it "can only be used once after Dragon Flame" do
                apply 0 [AntiChannel]
                use "Dragon Flame"
                setHealth 100
                as Enemy $ return ()
                use "Kirin"
                as Enemy $ return ()
                setHealth 100
                use "Kirin"
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 100
  where
    describeCharacter = describeCategory Shippuden
    stacks = 3
