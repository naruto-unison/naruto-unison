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
                tag Permanent "Syrup Trap"
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30 + 10

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
                Sim.at XEnemies $ target has "Syrup Trap"

        useOn Enemy "Devastate" do
            it "tags target" do
                Sim.act
                target has "Devastate"
            it "deals damage if target has Annihilate" do
                tag Permanent "Annihilate"
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 65
            it "does not tag if target has Annihilate" do
                tag Permanent "Annihilate"
                Sim.act
                not <$> target has "Devastate"

        useOn Enemy "Annihilate" do
            it "tags target" do
                Sim.act
                target has "Annihilate"
            it "deals damage if target has Devastate" do
                tag Permanent "Devastate"
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 65
            it "does not tag if target has Devastate" do
                tag Permanent "Devastate"
                Sim.act
                not <$> target has "Annihilate"

        useOn Self "Tag Team" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Annihilate"
            it "transfers health to stacks" do
                Sim.as Enemy $ damage dmg
                Sim.act
                stacks <- user numStacks "Izumo's Health"
                100 - stacks `shouldBe` dmg
            it "transfers health from stacks" do
                Sim.as Enemy $ damage dmg
                Sim.act
                userHealth <- user health
                userHealth `shouldBe` 100
            it "transfers upon death" do
                Sim.as Enemy $ damage dmg
                Sim.act
                Sim.as Enemy $ damage testStacks
                Sim.as Enemy kill
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 0

    describeCharacter "Aoba Yamashiro" do
        useOn Enemies "Scattering Crow Swarm" do
            it "deals stacking damage" do
                damaged <- measureDamage do
                    replicateM_ testStacks Sim.act
                    Sim.as Enemy $ damage dmg
                    Sim.turns 5
                damaged `shouldBe` 5 * 4 * testStacks
            it "reduces damage" do
                replicateM_ testStacks Sim.act
                damaged <- measureDamageTo Self $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 5 * testStacks

        useOn Ally "Revenge of the Murder" do
            it "resurrects target" do
                Sim.act
                Sim.as Enemy kill
                targetHealth <- target health
                targetHealth `shouldBe` 5
            it "ignores harm" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                Sim.as Enemy kill
                not <$> user (`is` Reveal)
            it "ignores help" do
                Sim.act
                Sim.as Enemy kill
                target (`is` Seal)
            it "kills target" do
                Sim.act
                Sim.as Enemy kill
                Sim.turns 1
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "teaches target" do
                Sim.act
                Sim.as Enemy kill
                target $ hasSkill "Converging Murder"

        useOn Enemy "Converging Murder" do
            it "damages target per Scattering Crow Swarm" do
                replicateM_ testStacks $ Sim.use "Scattering Crow Swarm"
                cancelChannel "Scattering Crow Swarm"
                setHealth 100
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 45 + 5 * testStacks

    describeCharacter "Ibiki Morino" do
        useOn Self "Biding Time" do
            it "adds a stack whenever Ibiki is damaged" do
                Sim.act
                replicateM_ testStacks $ Sim.as Enemy $ damage 15
                replicateM_ (testStacks * 2) $ Sim.as Enemy $ damage 10
                stacks <- user numStacks "Payback"
                stacks `shouldBe` testStacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Payback"

        useOn Enemy "Payback" do
            it "damages target per Payback" do
                targeting Self $ addStacks "Payback" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 15 + 5 * testStacks
            it "spends all Payback" do
                targeting Self $ addStacks "Payback" testStacks
                Sim.act
                not <$> user has "Payback"

        useOn Enemy "Summoning: Iron Maiden" do
            it "damages target on harm" do
                Sim.act
                damaged <- measureDamage do
                    Sim.as Enemy $ return ()
                    Sim.as Enemy $ return ()
                    Sim.turns 3
                damaged `shouldBe` 2 * 25

        useOn Enemy "Summoning: Torture Chamber" do
            it "damages target on non-action" do
                Sim.act
                damaged <- measureDamage do
                    Sim.as Enemy $ return ()
                    Sim.turns 5
                damaged `shouldBe` 2 * 25

    describeCharacter "Yūgao Uzuki"do
        useOn Enemy "Moonlight Night" do
            it "damages target per Moon Haze" do
                replicateM_ 2  $ Sim.use "Moon Haze"
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 50 + 2 * 25

    describeCharacter "Demon Brothers" do
        useOn Enemy "Chain Wrap" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Chain Shred"

        useOn Enemy "Chain Shred" do
            it "prolongs Chain Wrap" do
                Sim.use "Chain Wrap"
                Sim.act
                target has "Chain Wrap"

        useOn Enemy "Poison Gauntlet" do
            it "deals bonus damage if target has Chain Wrap" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Chain Wrap"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10

    describeCharacter "Haku" do
        useOn Enemy "Thousand Needles of Death" do
            it "is normally single-target" do
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 0
            it "damages all during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 30

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
                apply 5 "stun" [Stun All]
                Sim.act
                not <$> target has "stun"
            it "ignores stuns" do
                Sim.act
                apply 5 skillName [Stun All]
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "is normally single-target" do
                Sim.act
                Sim.at XAlly $ not <$> target has "Acupuncture"
            it "targets all during Crystal Ice Mirrors" do
                Sim.use "Crystal Ice Mirrors"
                Sim.act
                Sim.at XAlly $ target has "Acupuncture"

    describeCharacter "Zabuza Momochi" do
        useOn Enemy "Silent Killing" do
            it "deals bonus damage during Hidden Mist" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Hidden Mist"
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 15

    describeCharacter "Itachi Uchiha" do
        useOn Enemy "Amaterasu" do
            it "damages target" do
                damaged <- measureDamage do
                    Sim.use "Amaterasu"
                    Sim.turns testStacks
                damaged `shouldBe` 15 + 5 * testStacks
            it "is normally single-target" do
                damaged <- measureDamageTo XEnemies do
                    Sim.use "Amaterasu"
                    Sim.turns testStacks
                damaged `shouldBe` 0
            it "damages all targets and deals double damage during Mangekyō Sharingan" do
                Sim.use "Mangekyō Sharingan"
                damaged <- measureDamageTo XEnemies do
                    Sim.use "Amaterasu"
                    Sim.turns testStacks
                damaged `shouldBe` 30 + 10 * testStacks

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
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Sphere of Graves"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 10

        useOn Enemies "Sphere of Graves" do
            it "deals bonus damage during Crushing Palm" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Crushing Palm"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5

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
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                apply Permanent skillName [Invulnerable Affliction]
                Sim.use "Demon Parasite"
                damagedWith <- measureDamage Sim.act
                damagedWithout - damagedWith `shouldBe` 20

        useOn Enemy "Demon Parasite" do
            it "ends when target dies" do
                Sim.act
                Sim.as Self kill
                not <$> user has "Demon Parasite"

        useOn Enemy "Regeneration" do
            it "ends Demon Parasite" do
                Sim.use "Demon Parasite"
                Sim.act
                not <$> target has "Demon Parasite"

        useOn Enemy "Summoning: Rashōmon" do
            it "makes user invulnerable" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                not <$> user (`is` Reveal)
            it "ends Demon Parasite" do
                Sim.use "Demon Parasite"
                Sim.act
                not <$> target has "Demon Parasite"
  where
    describeCharacter = describeCategory Original
    dmg = 55
    testStacks = 3
