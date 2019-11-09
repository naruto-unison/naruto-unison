{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.FlashbacksSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Kushina Uzumaki" do
        useOn Enemy "Life Link" do
            it "kills target if user dies" do
                act
                apply 0 [Endure, Invulnerable All, Nullify, Reflect]
                self kill
                turns 1
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "kills user if target dies" do
                act
                self $ apply 0 [Endure, Invulnerable All, Nullify, Reflect]
                as Self kill
                turns 1
                userHealth <- health <$> nUser
                userHealth `shouldBe` 0
            it "alternates" do
                act
                hasSkill "Life Transfer" <$> nUser

        useOn Enemy "Adamantine Sealing Chains" do
            it "purges helpful effects" do
                apply 10 [Build stacks]
                as Enemy $ self $ apply 10 [Build stacks]
                as XEnemies $ apply 10 [Build stacks]
                act
                targetBuild <- Effects.build <$> nTarget
                targetBuild `shouldBe` 0

    describeCharacter "Minato Namikaze" do
        useOn Enemy "Flying Raijin" do
            it "tags enemy during Space-Time Marking" do
                use "Space-Time Marking"
                act
                has "Space-Time Marking" <$> user <*> nTarget
            it "deals bonus damage with Space-Time Marking" do
                everyone $ tag' "Space-Time Marking" 0
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 + 30
            it "damages all with Space-Time Marking" do
                everyone $ tag' "Space-Time Marking" 0
                remove "Space-Time Marking"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 30
        useOn Ally "Flying Raijin" do
            it "tags enemy during Space-Time Marking" do
                use "Space-Time Marking"
                act
                has "Space-Time Marking" <$> user <*> nTarget
            it "makes all invulnerable with Space-Time Marking" do
                everyone $ tag' "Space-Time Marking" 0
                remove "Space-Time Marking"
                act
                targetInvuln <- Effects.invulnerable <$> get XAlly
                targetInvuln `shouldBe` [All]

    describeCharacter "Hashirama Senju" do
        useOn Enemy "Wood Golem" do
            it "lasts 1 additional turn during Veritable 1000-Armed Kannon" do
                use "Veritable 1000-Armed Kannon"
                act
                turns 2
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser

    describeCharacter "Young Kakashi" do
        useOn Enemy "White Light Blade" do
            it "stuns if user has Sharingan Stun" do
                self $ tag' "Sharingan Stun" 0
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

        useOn Enemy "Lightning Blade" do
            it "stuns if user has Sharingan Stun" do
                self $ tag' "Sharingan Stun" 0
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

        useOn Enemy "Sharingan" do
            it "gains chakra on chakra gain" do
                act
                as Enemy $ self $ gain [Nin]
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [Nin])
            it "gains chakra on chakra deplete" do
                act
                as Enemy $ deplete 1
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [])
            it "gains chakra on chakra steal" do
                act
                as Enemy $ absorb 1
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [])
            it "gains no chakra otherwise" do
                act
                as Enemy $ return ()
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [])
            it "stuns if enemy stuns" do
                act
                as Enemy $ apply 0 [Stun Physical]
                hasOwn "Sharingan Stun" <$> nUser
            it "stuns if enemy disables" do
                act
                as Enemy $ apply 0 [Disable Counters]
                hasOwn "Sharingan Stun" <$> nUser
            it "does not stun otherwise" do
                act
                as Enemy $ apply 0 [Throttle 1 Counters]
                not . hasOwn "Sharingan Stun" <$> nUser
            it "strengthens if target damages" do
                self $ apply 0 [Reduce [All] Flat 5]
                act
                as Enemy $ damage 6
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 10
            it "does not strengthen otherwise" do
                self $ apply 0 [Reduce [All] Flat 5]
                act
                as Enemy $ damage 5
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 0
            it "stuns if user has Sharingan Stun" do
                self $ tag' "Sharingan Stun" 0
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

    describeCharacter "Rin Nohara" do
        useOn Enemy "Pit Trap" do
            it "damages target" do
                act
                turns 2
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15
            it "deals bonus damage if target acts" do
                act
                as Enemy $ return ()
                turns 2
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 15

    describeCharacter "Obito Uchiha" do
        useOn Enemy "Piercing Stab" do
            it "deals bonus damage during Sharingan" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Sharingan"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

        useOn Ally "Sharingan" do
            it "reduces damage if user dies" do
                act
                as Self $ self kill
                as Enemy $ damage dmg
                targetHealth <- health <$> nTarget
                dmg - (100 - targetHealth) `shouldBe` 5
            it "does not reduce damage otherwise" do
                act
                as Enemy $ damage dmg
                targetHealth <- health <$> nTarget
                dmg - (100 - targetHealth) `shouldBe` 0

    describeCharacter "Masked Man" do
        useOn Enemy "Kamui Banishment" do
            it "deals bonus damage if target has Kamui Chain Combo" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Kamui Chain Combo"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20
            it "lasts an additional turn if target has Kamui Chain Combo" do
                use "Kamui Chain Combo"
                act
                turns 1
                as XEnemies $ apply 0 [Focus]
                not . (`is` Focus) <$> nTarget
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
