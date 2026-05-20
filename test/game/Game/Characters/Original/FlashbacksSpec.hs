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
                apply Permanent [ Endure
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
                targeting Self $ apply Permanent [ Endure
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
                apply 10 [ Build stacks ]
                Sim.as Enemy $ targeting Self $ apply 10 [ Build stacks ]
                Sim.as XEnemies $ apply 10 [ Build stacks ]
                Sim.act
                targetBuild <- target $ Effects.build
                targetBuild `shouldBe` 0

    describeCharacter "Minato Namikaze" do
        useOn Enemy "Flying Raijin" do
            it "tags enemy during Space-Time Marking" do
                Sim.use "Space-Time Marking"
                Sim.act
                targetHas "Space-Time Marking"
            it "deals bonus damage with Space-Time Marking" do
                targeting Everyone $ tag' "Space-Time Marking" Permanent
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30 + 30
            it "damages all with Space-Time Marking" do
                targeting Everyone $ tag' "Space-Time Marking" Permanent
                remove "Space-Time Marking"
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 30
        useOn Ally "Flying Raijin" do
            it "tags enemy during Space-Time Marking" do
                Sim.use "Space-Time Marking"
                Sim.act
                targetHas "Space-Time Marking"
            it "makes all invulnerable with Space-Time Marking" do
                targeting Everyone $ tag' "Space-Time Marking" Permanent
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
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)

    describeCharacter "Young Kakashi" do
        useOn Enemy "White Light Blade" do
            it "stuns if user has Sharingan Stun" do
                targeting Self $ tag' "Sharingan Stun" Permanent
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

        useOn Enemy "Lightning Blade" do
            it "stuns if user has Sharingan Stun" do
                targeting Self $ tag' "Sharingan Stun" Permanent
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
                Sim.as Enemy $ apply Permanent [ Stun Physical ]
                userHas "Sharingan Stun"
            it "stuns if enemy disables" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Disable Counters ]
                userHas "Sharingan Stun"
            it "does not stun otherwise" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Throttle 1 Counters ]
                not <$> userHas "Sharingan Stun"
            it "strengthens if target damages" do
                targeting Self $ apply Permanent [ Reduce [All] Flat 5 ]
                Sim.act
                Sim.as Enemy $ damage 6
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 10
            it "does not strengthen otherwise" do
                targeting Self $ apply Permanent [ Reduce [All] Flat 5 ]
                Sim.act
                Sim.as Enemy $ damage 5
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 0
            it "stuns if user has Sharingan Stun" do
                targeting Self $ tag' "Sharingan Stun" Permanent
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

    describeCharacter "Rin Nohara" do
        useOn Enemy "Pit Trap" do
            it "damages target" do
                Sim.act
                Sim.turns 2
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15
            it "deals bonus damage if target acts" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.turns 2
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15 + 15

    describeCharacter "Obito Uchiha" do
        useOn Enemy "Piercing Stab" do
            it "deals bonus damage during Sharingan" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Sharingan"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

        useOn Ally "Sharingan" do
            it "reduces damage if user dies" do
                Sim.act
                Sim.as Self $ targeting Self kill
                Sim.as Enemy $ damage dmg
                targetHealth <- target health
                dmg - (100 - targetHealth) `shouldBe` 5
            it "does not reduce damage otherwise" do
                Sim.act
                Sim.as Enemy $ damage dmg
                targetHealth <- target health
                dmg - (100 - targetHealth) `shouldBe` 0

    describeCharacter "Masked Man" do
        useOn Enemy "Kamui Banishment" do
            it "deals bonus damage if target has Kusari Chains" do
                Sim.act
                targetHealth <- target health
                factory
                targeting Self factory
                Sim.use "Kusari Chains"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 20
            it "lasts an additional turn if target has Kusari Chains" do
                Sim.use "Kusari Chains"
                Sim.act
                Sim.turns 1
                Sim.as XEnemies $ apply Permanent [ Focus ]
                not <$> target (`is` Focus)

        useOn Self "Kamui Phase" do
            it "works on its own" do
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "does not work after Kusari Chains" do
                Sim.use "Kusari Chains"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "does not work after Kamui Banishment" do
                Sim.use "Kamui Banishment"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "does not work after Major Summoning: Kurama" do
                Sim.use "Major Summoning: Kurama"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
            it "does not work after itself" do
                Sim.use "Kamui Phase"
                Sim.use "Kamui Phase"
                Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
