{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.OrganizationsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Izumo and Kotetsu" do
        useOn Enemy "Mace Crush" do
            it "deals bonus damage if target has Syrup Trap" do
                tag' "Syrup Trap" Permanent
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30 + 10

        useOn Enemies "Syrup Trap" do
            it "stuns Physical" do
                Sim.act
                Sim.withClass Physical $ Sim.as XEnemies $ return ()
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` [Physical]
            it "stuns Chakra" do
                Sim.act
                Sim.withClass Chakra $ Sim.as XEnemies $ return ()
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` [Chakra]
            it "tags on stun" do
                Sim.act
                Sim.withClass Chakra $ Sim.as XEnemies $ return ()
                Sim.at XEnemies $ targetHas "Syrup Trap"

        useOn Enemy "Devastate" do
            it "tags target" do
                Sim.act
                targetHas "Devastate"
            it "deals damage if target has Annihilate" do
                tag' "Annihilate" Permanent
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 65
            it "does not tag if target has Annihilate" do
                tag' "Annihilate" Permanent
                Sim.act
                not <$> targetHas "Devastate"

        useOn Enemy "Annihilate" do
            it "tags target" do
                Sim.act
                targetHas "Annihilate"
            it "deals damage if target has Devastate" do
                tag' "Devastate" Permanent
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 65
            it "does not tag if target has Devastate" do
                tag' "Devastate" Permanent
                Sim.act
                not <$> targetHas "Annihilate"

        useOn Self "Tag Team" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Annihilate"
            it "transfers health to stacks" do
                Sim.as Enemy $ damage dmg
                Sim.act
                numStacks <- userStacks "Izumo's Health"
                100 - numStacks `shouldBe` dmg
            it "transfers health from stacks" do
                Sim.as Enemy $ damage dmg
                Sim.act
                userHealth <- user health
                100 - userHealth `shouldBe` 0
            it "transfers upon death" do
                Sim.as Enemy $ damage dmg
                Sim.act
                Sim.as Enemy $ damage stacks
                Sim.as Enemy kill
                userHealth <- target health
                dmg - (100 - userHealth) `shouldBe` 0

    describeCharacter "Aoba Yamashiro" do
        useOn Enemies "Scattering Crow Swarm" do
            it "deals stacking damage" do
                replicateM_ stacks Sim.act
                Sim.as Enemy $ damage dmg
                Sim.turns 5
                targetHealth <- target health
                100 - targetHealth `shouldBe` 5 * 4 * stacks
            it "reduces damage" do
                replicateM_ stacks Sim.act
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 5 * stacks

        useOn Ally "Revenge of the Murder" do
            it "resurrects target" do
                Sim.act
                Sim.as Enemy kill
                targetHealth <- target health
                targetHealth `shouldBe` 5
            it "ignores harm" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                Sim.as Enemy kill
                not <$> user (`is` Reveal)
            it "ignores help and dies" do
                Sim.act
                Sim.as Enemy kill
                Sim.as Self $ heal dmg
                Sim.turns 1
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "teaches target" do
                Sim.act
                Sim.as Enemy kill
                target $ hasSkill "Converging Murder"

        useOn Enemy "Converging Murder" do
            it "damages target per Scattering Crow Swarm" do
                apply Permanent [ AntiChannel ]
                replicateM_ stacks  $ Sim.use "Scattering Crow Swarm"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 45 + 5 * stacks

    describeCharacter "Ibiki Morino" do
        useOn Self "Biding Time" do
            it "adds a stack whenever Ibiki is damaged" do
                Sim.act
                replicateM_ stacks $ Sim.as Enemy $ damage 15
                replicateM_ (stacks * 2) $ Sim.as Enemy $ damage 10
                numStacks <- userStacks "Payback"
                numStacks `shouldBe` stacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Payback"

        useOn Enemy "Payback" do
            it "damages target per Payback" do
                self $ addStacks "Payback" stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15 + 5 * stacks
            it "spends all Payback" do
                self $ addStacks "Payback" stacks
                Sim.act
                not <$> userHas "Payback"

        useOn Enemy "Summoning: Iron Maiden" do
            it "damages target on harm" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.as Enemy $ return ()
                Sim.turns 3
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * 25

        useOn Enemy "Summoning: Torture Chamber" do
            it "damages target on non-action" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.turns 5
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * 25

    describeCharacter "Yūgao Uzuki"do
        useOn Enemy "Moonlight Night" do
            it "damages target per Moon Haze" do
                replicateM_ 2  $ Sim.use "Moon Haze"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 50 + 2 * 25

    describeCharacter "Demon Brothers" do
        useOn Enemy "Chain Wrap" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Chain Shred"

        useOn Enemy "Chain Shred" do
            it "prolongs Chain Wrap" do
                Sim.use "Chain Wrap"
                Sim.act
                targetHas "Chain Wrap"

        useOn Enemy "Poison Gauntlet" do
            it "deals bonus damage if target has Chain Wrap" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Chain Wrap"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Haku" do
        useOn Enemy "Thousand Needles of Death" do
            it "is normally single-target" do
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 0
            it "damages all during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 30

        useOn Enemy "Acupuncture" do
            it "is normally single-target" do
                Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` []
            it "targets all during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                targetStunned <- Effects.stun <$> Sim.targets XEnemies
                targetStunned `shouldBe` [All]

        useOn Ally "Acupuncture" do
            it "removes stun effects" do
                apply' "stun" 5 [Stun All]
                Sim.act
                not <$> targetHas "stun"
            it "ignores stuns" do
                Sim.act
                apply 5 [Stun All]
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "is normally single-target" do
                Sim.act
                Sim.at XAlly $ not <$> targetHas "Acupuncture"
            it "targets all during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                Sim.at XAlly $ targetHas "Acupuncture"

    describeCharacter "Zabuza Momochi" do
        useOn Enemy "Silent Killing" do
            it "deals bonus damage during Hidden Mist" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Hidden Mist"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 15

    describeCharacter "Itachi Uchiha" do
        useOn Enemy "Amaterasu" do
            it "damages target" do
                Sim.use "Amaterasu"
                Sim.turns stacks
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15 + 5 * stacks
            it "is normally single-target" do
                Sim.use "Amaterasu"
                Sim.turns stacks
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 0
            it "damages all targets and deals double damage during Mangekyō Sharingan" do
                Sim.use "Mangekyō Sharingan"
                Sim.use "Amaterasu"
                Sim.turns stacks
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 30 + 10 * stacks

        useOn Enemy "Tsukuyomi" do
            it "lasts 1 turn normally" do
                Sim.use "Tsukuyomi"
                Sim.turns 1
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "lasts 3 turns during Mangekyō Sharingan" do
                Sim.use "Mangekyō Sharingan"
                Sim.use "Tsukuyomi"
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

    describeCharacter "Jirōbō" do
        useOn Enemy "Crushing Palm" do
            it "deals bonus damage during Sphere of Graves" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Sphere of Graves"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

        useOn Enemies "Sphere of Graves" do
            it "deals bonus damage during Crushing Palm" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Crushing Palm"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5

        useOn Enemies "Earth Dome Prison" do
            it "steals chakra until broken" do
                gain [Blood, Gen, Nin, Tai]
                Sim.act
                Sim.turns 1
                Sim.as Enemy demolishAll
                Sim.turns 5
                chakras <- gameChakras
                chakras `shouldBe` ([Blood, Gen], [Nin, Tai])

    describeCharacter "Sakon and Ukon" do
        useOn Enemy "Demon Twin Attack" do
            it "deals less damage during Demon Parasite" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                apply Permanent [ Invulnerable Affliction ]
                Sim.use "Demon Parasite"
                Sim.act
                targetHealth' <- target health
                targetHealth' - targetHealth `shouldBe` 20

        useOn Enemy "Demon Parasite" do
            it "ends when target dies" do
                Sim.act
                Sim.as Self kill
                not <$> userHas "Demon Parasite"

        useOn Enemy "Regeneration" do
            it "ends Demon Parasite" do
                Sim.use "Demon Parasite"
                Sim.act
                not <$> targetHas "Demon Parasite"

        useOn Enemy "Summoning: Rashōmon" do
            it "makes user invulnerable" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "ends Demon Parasite" do
                Sim.use "Demon Parasite"
                Sim.act
                not <$> targetHas "Demon Parasite"
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
