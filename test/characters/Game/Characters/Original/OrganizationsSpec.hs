{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.OrganizationsSpec (spec) where

import Import

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Ninjas as Ninjas
import qualified Game.Model.Game as Game
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill

spec :: Spec
spec = parallel do
    describeCharacter "Izumo and Kotetsu" \useOn -> do
        useOn Enemy "Mace Crush" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Syrup Trap" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage if target has Syrup Trap" $
                    targetHealth - targetHealth' `shouldBe` 10

        useOn Enemies "Syrup Trap" do
            act
            withClass Mental $ as Enemy $ return ()
            targetStunned <- Effects.stun <$> P.nTarget
            factory
            act
            withClass Physical $ as Enemy $ return ()
            targetStunned' <- Effects.stun <$> P.nTarget
            return do
                it "stuns type class" $
                    targetStunned' `shouldBe` [Physical]
                it "does not stun mental" $
                    targetStunned `shouldBe` []

        useOn Enemy "Devastate" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            tagged <- Ninja.has "Devastate" <$> P.user <*> P.nTarget
            factory
            tag' "Annihilate" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            tagged' <- Ninja.has "Devastate" <$> P.user <*> P.nTarget
            return do
                it "untagged: tags target"
                    tagged
                it "untagged: does not deal damage" $
                    100 - targetHealth `shouldBe` 0
                it "tagged: does not tag target" $
                    not tagged'
                it "tagged: deals damage" $
                    100 - targetHealth' `shouldBe` 65

        useOn Enemy "Annihilate" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            tagged <- Ninja.has "Annihilate" <$> P.user <*> P.nTarget
            factory
            tag' "Devastate" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            tagged' <- Ninja.has "Annihilate" <$> P.user <*> P.nTarget
            return do
                it "untagged: tags target"
                    tagged
                it "untagged: does not deal damage" $
                    100 - targetHealth `shouldBe` 0
                it "tagged: does not tag target" $
                    not tagged'
                it "tagged: deals damage" $
                    100 - targetHealth' `shouldBe` 65

        useOn Self "Tag Team" do
            as Enemy $ damage targetDmg
            act
            userHealth <- Ninja.health <$> P.nUser
            userStacks <- Ninja.numStacks "Izumo's Health"
                          <$> P.user <*> P.nUser
            as Enemy kill
            userHealth' <- Ninja.health <$> P.nUser
            return do
                it "transfers health..." $
                    100 - userHealth `shouldBe` 0
                it "...into stacks" $
                    100 - userStacks `shouldBe` targetDmg
                it "swaps upon death" $
                    100 - userHealth' `shouldBe` targetDmg

    describeCharacter "Aoba Yamashiro" \useOn -> do
        useOn Enemy "Scattering Crow Swarm" do
            replicateM_ stacks act
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "deals stacking damage" $
                    100 - targetHealth `shouldBe` 5 * 4 * stacks
                it "reduces damage" $
                    targetDmg - (100 - userHealth) `shouldBe` 5 * stacks

        useOn Ally "Revenge of the Murder" do
            act
            as Enemy kill
            targetHealth <- Ninja.health <$> P.nTarget
            skill <- Skill.name . Ninjas.getSkill (Left 1) <$> P.nTarget
            turns 1
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "saves target from death" $
                    targetHealth `shouldBe` 5
                it "replaces skills" $
                    skill `shouldBe` "Converging Murder"
                it "ends after 1 turn" $
                    targetHealth' `shouldBe` 0

        useOn Enemy "Converging Murder" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            addStacks "Scattering Crow Swarm" stacks
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 45
                it "deals bonus damage per stack of Scattering Crow Swarm" $
                    targetHealth - targetHealth' `shouldBe` 5 * stacks

    describeCharacter "Ibiki Morino" \useOn -> do
        useOn Self "Biding Time" do
            act
            as Enemy $ damage targetDmg
            as Enemy $ damage targetDmg
            as Enemy $ apply 0 [Reveal]
            userHealth <- Ninja.health <$> P.nUser
            userStacks <- Ninja.numStacks "Payback" <$> P.user <*> P.nUser
            return do
                it "reduces damage" $
                    100 - userHealth `shouldBe` 2 * (targetDmg - 10)
                it "adds a stack of Payback whenever damaged" $
                    userStacks `shouldBe` 2

        useOn Enemy "Payback" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ addStacks "Payback" stacks
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
                it "deals bonus damage per stack of Payback" $
                    targetHealth - targetHealth' `shouldBe` 5 * stacks

        useOn Enemy "Summoning: Iron Maiden" do
            act
            defense <- Ninja.totalDefense <$> P.nUser
            turns 1
            targetHealth <- Ninja.health <$> P.nTarget
            as Enemy $ return ()
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "defends user" $
                    defense `shouldBe` 30
                it "damages target on harm" $
                    targetHealth - targetHealth' `shouldBe` 25
                it "does nothing otherwise" $
                    100 - targetHealth `shouldBe` 0

        useOn Enemy "Summoning: Torture Chamber" do
            act
            defense <- Ninja.totalDefense <$> P.nUser
            as Enemy $ return ()
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            act
            turns 5
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "defends user" $
                    defense `shouldBe` 30
                it "damages target without action" $
                    100 - targetHealth' `shouldBe` 3 * 25
                it "does nothing otherwise" $
                    100 - targetHealth `shouldBe` 2 * 25

    describeCharacter "Yūgao Uzuki" \useOn -> do
        useOn Enemy "Moonlight Night" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            apply 0 [Reduce [All] Flat targetDmg]
            self $ addStacks "Moon Haze" stacks
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 50
                it "deals bonus damage per stack of Moon Haze" $
                    targetHealth - targetHealth' `shouldBe`
                        25 * stacks - targetDmg

        useOn Enemy "Moon Haze" do
            act
            defense <- Ninja.totalDefense <$> P.nUser
            factory
            replicateM_ stacks act
            userStacks <- Ninja.numStacks "Moon Haze" <$> P.user <*> P.nUser
            return do
                it "defends user" $
                    defense `shouldBe` 20
                it "adds stacks" $
                    userStacks `shouldBe` 1 + stacks

        useOn Enemy "Sealing Technique" do
            act
            exposed <- targetIsExposed
            effects <- Ninja.effects <$> P.nTarget
            apply 0 [Reduce [All] Flat 10, Enrage, Focus]
            effects' <- Ninja.effects <$> P.nTarget
            defend 0 10
            trap 0 (CounterAll All) $ return ()
            damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "exposes target" $
                    exposed
                it "blocks damage reduction and ignores" $
                    effects' `shouldBe` effects
                it "negates counters and destructible defense" $
                    100 - targetHealth `shouldBe` targetDmg

    describeCharacter "Demon Brothers" \useOn -> do
        useOn Enemy "Chain Wrap" do
            act
            stunned <- Effects.stun <$> P.nTarget
            return do
                it "stuns target" $
                    stunned `shouldBe` [NonMental]

        useOn Enemy "Chain Shred" do
            tag' "Chain Wrap" 1
            turns 1
            act
            targetHealth <- Ninja.health <$> P.nTarget
            has <- Ninja.has "Chain Wrap" <$> P.user <*> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 45
                it "prolongs Chain Wrap"
                    has

        useOn Enemy "Bladed Gauntlet" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Chain Wrap" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage if target has Chain Wrap" $
                    targetHealth - targetHealth' `shouldBe` 10

        useOn Self "Water Melding" do
            act
            defense     <- Ninja.totalDefense <$> P.nUser
            (chakra, _) <- Game.chakra <$> P.game
            return do
                it "defends user" $
                    defense `shouldBe` 20
                it "gains chakra" $
                    chakra `shouldBe` [Tai]

    describeCharacter "Haku" \useOn -> do
        useOn Enemy "Thousand Needles of Death" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            self $ tag' "Crystal Ice Mirrors" 0
            act
            targetHealth' <- Ninja.health <$> get XEnemies
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "targets all during Crystal Ice Mirrors" $
                    100 - targetHealth' `shouldBe` 100 - targetHealth

        useOn Ally "Acupuncture" do
            apply' "foo" 0 [Stun All]
            act
            has <- Ninja.has "foo" <$> P.user <*> P.nTarget
            apply' "bar" 0 [Stun All]
            stunned <- Effects.stun <$> P.nTarget
            return do
                it "cures stuns" $
                    not has
                it "ignores stuns" $
                    stunned `shouldBe` []

        useOn Enemy "Acupuncture" do
            act
            stunned <- Effects.stun <$> P.nTarget
            allyHas <- Ninja.has "Acupuncture" <$> P.user <*> get XAlly
            self $ tag' "Crystal Ice Mirrors" 0
            act
            allyHas' <- Ninja.has "Acupuncture" <$> P.user <*> get XAlly
            return do
                it "stuns target" $
                    stunned `shouldBe` [All]
                it "targets all during Crystal Ice Mirrors" $
                    allyHas'
                it "does not target all otherwise" $
                    not allyHas

        useOn Self "Crystal Ice Mirrors" do
            act
            as Enemy $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            return do
                it "makes user invulnerable" $
                    not harmed

    describeCharacter "Zabuza Momochi" \useOn -> do
        useOn Enemy "Soundless Murder" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Hidden Mist" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage during Hidden Mist" $
                    targetHealth - targetHealth' `shouldBe` 15

        useOn Enemies "Water Dragon" do
            act
            targetHealth <- Ninja.health <$> get XEnemies
            stunned <- Effects.stun <$> get XEnemies
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 10
                it "stuns targets" $
                    stunned `shouldBe` [Physical, Bane]

        useOn Enemies "Hidden Mist" do
            act
            targetExhausted <- Effects.exhaust [Mental] <$> get XEnemies
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "adds a random chakra to enemy skills" $
                    toList targetExhausted `shouldBe` [Rand]
                it "reduces damage" $
                    targetDmg - (100 - userHealth) `shouldBe` 5

    describeCharacter "Itachi Uchiha" \useOn -> do
        useOn Self "Mangekyō Sharingan" do
            act
            turns stacks
            userHealth <- Ninja.health <$> P.nUser
            self $ heal targetDmg
            userHealth' <- Ninja.health <$> P.nUser
            return do
                it "damages user every turn" $
                    100 - userHealth `shouldBe` 15 * (1 + stacks)
                it "prevents healing" $
                    userHealth' `shouldBe` userHealth

        useOn Enemy "Amaterasu" do
            act
            turns stacks
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Mangekyō Sharingan" 0
            act
            turns stacks
            targetHealth' <- Ninja.health <$> get XEnemies
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 10 + 5 * (1 + stacks)
                it "deals double damage and targets all during Mangekyō Sharingan" $
                    100 - targetHealth' `shouldBe` 2 * (100 - targetHealth)

        useOn Enemy "Tsukuyomi" do
            act
            targetStunned <- Effects.stun <$> P.nTarget
            targetHealth <- Ninja.health <$> P.nTarget
            turns 2
            targetStunned' <- Effects.stun <$> P.nTarget
            factory
            self $ tag' "Mangekyō Sharingan" 0
            act
            turns 2
            targetStunned'' <- Effects.stun <$> P.nTarget
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "lasts 1 turn normally" $
                    targetStunned' `shouldBe` []
                it "lasts 3 turns during Mangekyō Sharingan" $
                    targetStunned'' `shouldBe` [All]

    describeCharacter "Kisame Hoshigaki" \useOn -> do
        useOn Enemy "Samehada Slash" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "stuns target" $
                    targetStunned `shouldBe` [Chakra, Mental]

        useOn Enemy "Samehada Shred" do
            gain [Blood, Gen, Nin]
            act
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            (chakra, _) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15 * 2
                it "steals chakra" $
                    toList chakra `shouldBe` [Blood, Gen]

        useOn Enemy "Super Shark Bomb" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "stuns target" $
                    targetStunned `shouldBe` [Physical, Bane]

    describeCharacter "Jirōbō" \useOn -> do
        useOn Enemy "Crushing Palm" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            tagged <- Ninja.has "Crushing Palm" <$> P.user <*> P.nUser
            factory
            self $ tag' "Sphere of Graves" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "tags user"
                    tagged
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage after Sphere of Graves" $
                    targetHealth - targetHealth' `shouldBe` 10

        useOn Enemy "Sphere of Graves" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            tagged <- Ninja.has "Sphere of Graves" <$> P.user <*> P.nUser
            factory
            self $ tag' "Crushing Palm" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "tags user"
                    tagged
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
                it "deals bonus damage after Crushing Palm" $
                    targetHealth - targetHealth' `shouldBe` 5

        useOn REnemy "Earth Dome Prison" do
              gain [Blood, Gen, Nin, Tai]
              savedChakra <- P.game
              act
              defense <- Ninja.totalDefense <$> get XAlly
              turns 5
              (chakra, _) <- Game.chakra <$> P.game
              factory
              self factory
              P.alter $ const savedChakra
              act
              turns 1
              as Enemy $ damage targetDmg
              turns 4
              (chakra', _) <- Game.chakra <$> P.game
              return do
                  it "defends team" $
                      defense `shouldBe` 35
                  it "steals chakra" $
                      toList chakra `shouldBe` [Blood, Gen, Nin]
                  it "cancels if destructible defense breaks" $
                      toList chakra' `shouldBe` [Blood, Gen]


    describeCharacter "Kidōmaru" \useOn -> do
        useOn Enemy "Spider War Bow" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 50

        useOn Enemies "Summoning: Kyodaigumo" do
            act
            targetSnared <- Effects.snare <$> P.nTarget
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            turns 8
            targetHealth <- Ninja.health <$> get XEnemies
            return do
                it "reduces damage" $
                    targetDmg - (100 - userHealth) `shouldBe` 10
                it "snares target" $
                    targetSnared `shouldBe` 1
                it "damages targets" $
                    100 - targetHealth `shouldBe` 10 * 5

        useOn Ally "Spiral Web" do
            act
            withClass Physical $ as Enemy $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nTarget
            return do
                it "counters harm to target" $
                    not harmed

    describeCharacter "Tayuya" \useOn -> do
        useOn Enemies "Summoning: Doki" do
            act
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            turns 3
            targetHealth <- Ninja.health <$> get XEnemies
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 15 * 2
                it "reduces damage to user" $
                    targetDmg - (100 - userHealth) `shouldBe` 10

        useOn Enemy "Demon Revolution" do
            gain [Blood, Gen]
            act
            targetHealth <- Ninja.health <$> P.nTarget
            (_, chakra) <- Game.chakra <$> P.game
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 10
                it "depletes 1 chakra" $
                    toList chakra `shouldBe` [Gen]

        useOn Enemies "Chains of Fantasia" do
            act
            targetStunned <- Effects.stun <$> get XEnemies
            return do
                it "stuns targets" $
                    targetStunned `shouldBe` [All]

    describeCharacter "Sakon and Ukon" \useOn -> do
        useOn Enemy "Demon Twin Attack" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Demon Parasite" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 40
                it "deals less damage during Demon Parasite" $
                    targetHealth' - targetHealth `shouldBe` 20

        useOn Enemy "Demon Parasite" do
            act
            turns stacks
            targetHealth <- Ninja.health <$> P.nTarget
            as Enemy $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            self factory
            factory
            act
            has <- Ninja.hasOwn "Demon Parasite" <$> P.nUser
            as Enemy $ self $ kill
            has' <- Ninja.hasOwn "Demon Parasite" <$> P.nUser
            return do
                it "reduces damage" $
                    targetDmg - (100 - userHealth) `shouldBe` 15
                it "damages target every turn" $
                    100 - targetHealth `shouldBe` 20 * (1 + stacks)
                it "lasts..."
                    has
                it "...until target dies" $
                    not has'

        useOn Self "Regeneration" do
            as Enemy $ damage targetDmg
            act
            userHealth <- Ninja.health <$> P.nUser
            factory
            tag' "Demon Parasite" 0
            act
            has <- Ninja.hasOwn "Demon Parasite" <$> P.nUser
            return do
                it "heals user" $
                    targetDmg - (100 - userHealth) `shouldBe` 30
                it "ends Demon Parasite" $
                    not has

    describeCharacter "Kimimaro" \useOn -> do
        useOn Enemy "Camellia Dance" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "damage user" $
                    100 - userHealth `shouldBe` 5

        useOn Enemy "Clematis Dance" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 40
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "damages user" $
                    100 - userHealth `shouldBe` 10

        useOn Enemies "Bracken Dance" do
            act
            targetHealth <- Ninja.health <$> get XEnemies
            userHealth <- Ninja.health <$> P.nUser
            as Enemy $ damage targetDmg
            userHealth' <- Ninja.health <$> P.nUser
            turns 3
            userHealth'' <- Ninja.health <$> P.nUser
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` 30
                it "weakens targets" $
                    targetDmg - (userHealth - userHealth') `shouldBe` 20
                it "damages user" $
                    userHealth' - userHealth'' `shouldBe` 15
  where
    describeCharacter = describeCategory Original
    targetDmg = 55
    stacks = 3
