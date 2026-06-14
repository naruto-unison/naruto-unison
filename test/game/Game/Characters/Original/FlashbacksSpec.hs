{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.FlashbacksSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Kushina Uzumaki" do
        useOn Enemy "Life Link" do
            it "kills target if user dies" do
                Sim.act
                apply Permanent skillName [ Endure
                                , Invulnerable All
                                , Nullify
                                , Reflect
                                ]
                targeting Self kill
                Sim.turns 1
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "kills user if target dies" do
                Sim.act
                targeting Self $ apply Permanent skillName [ Endure
                                                 , Invulnerable All
                                                 , Nullify
                                                 , Reflect
                                                 ]
                Sim.as Self kill
                Sim.turns 1
                userHealth <- user health
                userHealth `shouldBe` 0
            it "alternates" do
                Sim.act
                user $ hasSkill "Life Transfer"

        useOn Enemy "Adamantine Sealing Chains" do
            it "purges helpful effects" do
                apply 10 skillName [Build testStacks]
                Sim.as Enemy $ targeting Self $ apply 10 skillName [Build testStacks]
                Sim.as XEnemies $ apply 10 skillName [Build testStacks]
                Sim.act
                targetBuild <- target $ Effects.build
                targetBuild `shouldBe` 0

    describeCharacter "Minato Namikaze" do
        useOn Enemy "Flying Raijin" do
            it "tags enemy during Space-Time Marking" do
                Sim.use "Space-Time Marking"
                Sim.act
                target has "Space-Time Marking"
            it "deals bonus damage with Space-Time Marking" do
                targeting Everyone $ tag Permanent "Space-Time Marking"
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30 + 30
            it "damages all with Space-Time Marking" do
                targeting Everyone $ tag Permanent "Space-Time Marking"
                remove "Space-Time Marking"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 30
        useOn Ally "Flying Raijin" do
            it "tags enemy during Space-Time Marking" do
                Sim.use "Space-Time Marking"
                Sim.act
                target has "Space-Time Marking"
            it "makes all invulnerable with Space-Time Marking" do
                targeting Everyone $ tag Permanent "Space-Time Marking"
                remove "Space-Time Marking"
                Sim.act
                targetInvuln <- Effects.invulnerable <$> Sim.targets XAlly
                targetInvuln `shouldBe` [All]

    describeCharacter "Hashirama Senju" do
        useOn Enemy "Wood Golem" do
            it "lasts 1 additional turn during Veritable 1000-Armed Kannon" do
                Sim.use "Veritable 1000-Armed Kannon"
                Sim.act
                Sim.turns 2
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                not <$> user (`is` Reveal)

    describeCharacter "Young Kakashi" do
        useOn Enemy "White Light Blade" do
            it "stuns if user has Sharingan Stun" do
                targeting Self $ tag Permanent "Sharingan Stun"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

        useOn Enemy "Lightning Blade" do
            it "stuns if user has Sharingan Stun" do
                targeting Self $ tag Permanent "Sharingan Stun"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

        useOn Enemy "Sharingan" do
            it "gains chakra on chakra gain" do
                Sim.act
                Sim.as Enemy $ targeting Self $ gain [Nin]
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [Nin])
            it "gains chakra on chakra deplete" do
                Sim.act
                Sim.as Enemy $ deplete 1
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [])
            it "gains chakra on chakra steal" do
                Sim.act
                Sim.as Enemy $ absorb 1
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [])
            it "gains no chakra otherwise" do
                Sim.act
                Sim.as Enemy $ return ()
                chakras <- gameChakras
                chakras `shouldBe` ([], [])
            it "stuns if enemy stuns" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Stun Physical]
                user has "Sharingan Stun"
            it "stuns if enemy disables" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Disable Counters]
                user has "Sharingan Stun"
            it "does not stun otherwise" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Throttle 1 Counters]
                not <$> user has "Sharingan Stun"
            it "strengthens if target damages" do
                targeting Self $ apply Permanent skillName [Reduce [All] Flat 5]
                Sim.act
                Sim.as Enemy $ damage 6
                damaged <- measureDamage $ damage dmg
                damaged - dmg `shouldBe` 10
            it "does not strengthen otherwise" do
                targeting Self $ apply Permanent skillName [Reduce [All] Flat 5]
                Sim.act
                Sim.as Enemy $ damage 5
                damaged <- measureDamage $ damage dmg
                damaged - dmg `shouldBe` 0
            it "stuns if user has Sharingan Stun" do
                targeting Self $ tag Permanent "Sharingan Stun"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

    describeCharacter "Rin Nohara" do
        useOn Enemy "Pit Trap" do
            it "damages target" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.turns 2
                damaged `shouldBe` 15
            it "deals bonus damage if target acts" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.as Enemy $ return ()
                    Sim.turns 2
                damaged `shouldBe` 15 + 15

    describeCharacter "Obito Uchiha" do
        useOn Enemy "Piercing Stab" do
            it "deals bonus damage during Sharingan" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Sharingan"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10

        useOn Ally "Sharingan" do
            it "reduces damage if user dies" do
                Sim.act
                Sim.as Self $ targeting Self kill
                damaged <- measureDamage $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 5
            it "does not reduce damage otherwise" do
                Sim.act
                damaged <- measureDamage $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 0

    describeCharacter "Masked Man" do
        useOn Enemy "Kamui Banishment" do
            it "deals bonus damage if target has Kusari Chains" do
                damageWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Kusari Chains"
                damageWith <- measureDamage Sim.act
                damageWith - damageWithout `shouldBe` 20
            it "lasts an additional turn if target has Kusari Chains" do
                Sim.use "Kusari Chains"
                Sim.act
                Sim.turns 1
                Sim.as XEnemies $ apply Permanent skillName [Focus]
                not <$> target (`is` Focus)

        useOn Self "Kamui Phase" do
            it "works on its own" do
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                not <$> user (`is` Reveal)
            it "does not work after Kusari Chains" do
                Sim.use "Kusari Chains"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                user (`is` Reveal)
            it "does not work after Kamui Banishment" do
                Sim.use "Kamui Banishment"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                user (`is` Reveal)
            it "does not work after Major Summoning: Kurama" do
                Sim.use "Major Summoning: Kurama"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                user (`is` Reveal)
            it "does not work after itself" do
                Sim.use "Kamui Phase"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                user (`is` Reveal)
  where
    describeCharacter = describeCategory Original
    dmg = 55
    testStacks = 3
