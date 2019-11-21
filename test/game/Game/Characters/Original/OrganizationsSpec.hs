{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.OrganizationsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Izumo and Kotetsu" do
        useOn Enemy "Mace Crush" do
            it "deals bonus damage if target has Syrup Trap" do
                tag' "Syrup Trap" Permanent
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 + 10

        useOn Enemies "Syrup Trap" do
            it "stuns Physical" do
                act
                withClass Physical $ as XEnemies $ return ()
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` [Physical]
            it "stuns Chakra" do
                act
                withClass Chakra $ as XEnemies $ return ()
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` [Chakra]
            it "tags on stun" do
                act
                withClass Chakra $ as XEnemies $ return ()
                has "Syrup Trap" <$> user <*> get XEnemies

        useOn Enemy "Devastate" do
            it "tags target" do
                act
                has "Devastate" <$> user <*> nTarget
            it "deals damage if target has Annihilate" do
                tag' "Annihilate" Permanent
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 65
            it "does not tag if target has Annihilate" do
                tag' "Annihilate" Permanent
                act
                not <$> (has "Devastate" <$> user <*> nTarget)

        useOn Enemy "Annihilate" do
            it "tags target" do
                act
                has "Annihilate" <$> user <*> nTarget
            it "deals damage if target has Devastate" do
                tag' "Devastate" Permanent
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 65
            it "does not tag if target has Devastate" do
                tag' "Devastate" Permanent
                act
                not <$> (has "Annihilate" <$> user <*> nTarget)

        useOn Self "Tag Team" do
            it "alternates" do
                act
                hasSkill "Annihilate" <$> nUser
            it "transfers health to stacks" do
                as Enemy $ damage dmg
                act
                userStacks <- numAnyStacks "Izumo's Health" <$> nUser
                100 - userStacks `shouldBe` dmg
            it "transfers health from stacks" do
                as Enemy $ damage dmg
                act
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` 0
            it "transfers upon death" do
                as Enemy $ damage dmg
                act
                as Enemy $ damage stacks
                as Enemy kill
                userHealth <- health <$> nTarget
                dmg - (100 - userHealth) `shouldBe` 0

    describeCharacter "Aoba Yamashiro" do
        useOn Enemies "Scattering Crow Swarm" do
            it "deals stacking damage" do
                replicateM_ stacks act
                as Enemy $ damage dmg
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 * 4 * stacks
            it "reduces damage" do
                replicateM_ stacks act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 5 * stacks

        useOn Ally "Revenge of the Murder" do
            it "resurrects target" do
                act
                as Enemy kill
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 5
            it "ignores harm" do
                act
                as Enemy $ apply Permanent [Reveal]
                as Enemy kill
                not . (`is` Reveal) <$> nUser
            it "ignores help and dies" do
                act
                as Enemy kill
                as Self $ heal dmg
                turns 1
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "teaches target" do
                act
                as Enemy kill
                hasSkill "Converging Murder" <$> nTarget

        useOn Enemy "Converging Murder" do
            it "damages target per Scattering Crow Swarm" do
                apply Permanent [AntiChannel]
                replicateM_ stacks $ use "Scattering Crow Swarm"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 45 + 5 * stacks

    describeCharacter "Ibiki Morino" do
        useOn Self "Biding Time" do
            it "adds a stack whenever Ibiki is damaged" do
                act
                replicateM_ stacks $ as Enemy $ damage 15
                replicateM_ (stacks * 2) $ as Enemy $ damage 10
                userStacks <- numAnyStacks "Payback" <$> nUser
                userStacks `shouldBe` stacks
            it "alternates" do
                act
                hasSkill "Payback" <$> nUser

        useOn Enemy "Payback" do
            it "damages target per Payback" do
                self $ addStacks "Payback" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 5 * stacks
            it "spends all Payback" do
                self $ addStacks "Payback" stacks
                act
                not . hasOwn "Payback" <$> nUser

        useOn Enemy "Summoning: Iron Maiden" do
            it "damages target on harm" do
                act
                as Enemy $ return ()
                as Enemy $ return ()
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * 25

        useOn Enemy "Summoning: Torture Chamber" do
            it "damages target on non-action" do
                act
                as Enemy $ return ()
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * 25

    describeCharacter "Yūgao Uzuki"do
        useOn Enemy "Moonlight Night" do
            it "damages target per Moon Haze" do
                replicateM_ 2 $ use "Moon Haze"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 50 + 2 * 25

    describeCharacter "Demon Brothers" do
        useOn Enemy "Chain Wrap" do
            it "alternates" do
                act
                hasSkill "Chain Shred" <$> nUser

        useOn Enemy "Chain Shred" do
            it "prolongs Chain Wrap" do
                use "Chain Wrap"
                act
                has "Chain Wrap" <$> user <*> nTarget

        useOn Enemy "Poison Gauntlet" do
            it "deals bonus damage if target has Chain Wrap" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Chain Wrap"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Haku" do
        useOn Enemy "Thousand Needles of Death" do
            it "is normally single-target" do
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 0
            it "damages all during Crystal Ice Mirrors" do
                use "Crystal Ice Mirrors"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 30

        useOn Enemy "Acupuncture" do
            it "is normally single-target" do
                act
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` []
            it "targets all during Crystal Ice Mirrors" do
                use "Crystal Ice Mirrors"
                act
                targetStunned <- Effects.stun <$> get XEnemies
                targetStunned `shouldBe` [All]

        useOn Ally "Acupuncture" do
            it "removes stun effects" do
                apply' "stun" 5 [Stun All]
                act
                not <$> (has "stun" <$> user <*> nTarget)
            it "ignores stuns" do
                act
                apply 5 [Stun All]
                stunned <- Effects.stun <$> nTarget
                stunned `shouldBe` []
            it "is normally single-target" do
                act
                not <$> (has "Acupuncture" <$> user <*> get XAlly)
            it "targets all during Crystal Ice Mirrors" do
                use "Crystal Ice Mirrors"
                act
                has "Acupuncture" <$> user <*> get XAlly

    describeCharacter "Zabuza Momochi" do
        useOn Enemy "Silent Killing" do
            it "deals bonus damage during Hidden Mist" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Hidden Mist"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 15

    describeCharacter "Itachi Uchiha" do
        useOn Enemy "Amaterasu" do
            it "damages target" do
                act
                turns stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 5 * stacks
            it "is normally single-target" do
                act
                turns stacks
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 0
            it "damages all targets and deals double damage during Mangekyō Sharingan" do
                use "Mangekyō Sharingan"
                act
                turns stacks
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 30 + 10 * stacks

        useOn Enemy "Tsukuyomi" do
            it "lasts 1 turn normally" do
                act
                turns 1
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "lasts 3 turns during Mangekyō Sharingan" do
                use "Mangekyō Sharingan"
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

    describeCharacter "Jirōbō" do
        useOn Enemy "Crushing Palm" do
            it "deals bonus damage during Sphere of Graves" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Sphere of Graves"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

        useOn Enemies "Sphere of Graves" do
            it "deals bonus damage during Crushing Palm" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Crushing Palm"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

        useOn Enemies "Earth Dome Prison" do
            it "steals chakra until broken" do
                gain [Blood, Gen, Nin, Tai]
                act
                turns 1
                as Enemy demolishAll
                turns 5
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood, Gen], [Nin, Tai])

    describeCharacter "Sakon and Ukon" do
        useOn Enemy "Demon Twin Attack" do
            it "deals less damage during Demon Parasite" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply Permanent [Invulnerable Affliction]
                use "Demon Parasite"
                act
                targetHealth' <- health <$> nTarget
                targetHealth' - targetHealth `shouldBe` 20

        useOn Enemy "Demon Parasite" do
            it "ends when target dies" do
                act
                as Self kill
                not . hasOwn "Demon Parasite" <$> nUser

        useOn Enemy "Regeneration" do
            it "ends Demon Parasite" do
                use "Demon Parasite"
                act
                not <$> (has "Demon Parasite" <$> user <*> nTarget)

        useOn Enemy "Summoning: Rashōmon" do
            it "makes user invulnerable" do
                act
                as Enemy $ apply Permanent [Reveal]
                not . (`is` Reveal) <$> nUser
            it "ends Demon Parasite" do
                use "Demon Parasite"
                act
                not <$> (has "Demon Parasite" <$> user <*> nTarget)
  where
    describeCharacter = describeCategory Original
    dmg = 55
    stacks = 3
