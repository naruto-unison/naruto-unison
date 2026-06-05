{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.AkatsukiSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Madara Uchiha" do
        useOn Self "Mangekyō Sharingan" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Eternal Mangekyō Sharingan"

        useOn Self "Susanoo" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Armored Susanoo Assault"

        useOn Enemy "Armored Susanoo Assault" do
            it "deals damage per stack of Susanoo" do
                Sim.use "Susanoo"
                Sim.turns testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 30 + 5 * (testStacks + 1)

        useOn Enemy "Majestic Destroyer Flame" do
            it "damages on defense" do
                Sim.act
                setHealth 100
                damaged <- measureDamage $ Sim.as Enemy $ defend Permanent 10
                damaged `shouldBe` 10
            it "damages on reduce" do
                Sim.act
                setHealth 100
                damaged <- measureDamage
                         $ Sim.as Enemy $ apply Permanent [ Reduce [All] Flat 10 ]
                damaged `shouldBe` 10
            it "does not damage otherwise" do
                Sim.act
                setHealth 100
                damaged <- measureDamage $ Sim.as Enemy $ damage dmg
                damaged `shouldBe` 0

    describeCharacter "Deidara" do
        useOn Enemy "C1: Bird Bomb" do
            it "alternates" do
                Sim.act
                user $ hasSkill "C3: Megaton Sculpture"

        useOn Enemy "C2: Clay Dragon" do
            it "alternates" do
                Sim.act
                user $ hasSkill "C2: Minefield"
            it "alternates other" do
                Sim.act
                user $ hasSkill "C2: Dragon Missile"

        useOn Enemy "C2: Minefield" do
            it "damages attacker" do
                Sim.act
                damaged <- measureDamage
                         $ Sim.withClass NonMental $ Sim.as Enemy $ return ()
                damaged `shouldBe` 10
            it "weakens attacker" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                damaged <- measureDamageTo Self $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 5

        useOn Enemy "C4: Karura" do
            it "alternates" do
                Sim.act
                user $ hasSkill "C0: Ultimate Art"

    describeCharacter "Sasori" do
        useOn Self "Kazekage Puppet Summoning" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Iron Sand: World Order"

        useOn Enemies "Iron Sand: World Order" do
            it "damages per Iron Sand" do
                Sim.use "Kazekage Puppet Summoning"
                Sim.turns testStacks
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 10 + 5 * (testStacks + 1)

        useOn Enemy "Poison Blade Assault" do
            it "damages repeatedly" do
                Sim.use "Kazekage Puppet Summoning"
                damaged <- measureDamage do
                    Sim.act
                    Sim.turns 4
                damaged `shouldBe` 20 * 2
            it "ends when destroyed" do
                Sim.use "Kazekage Puppet Summoning"
                damaged <- measureDamage do
                    Sim.act
                    Sim.as Enemy demolishAll
                    Sim.turns testStacks
                damaged `shouldBe` 20

        useOn Enemies "Thousand Arms" do
            it "exposes targets" do
                Sim.use "Kazekage Puppet Summoning"
                Sim.act
                Sim.as REnemy $ return ()
                target (`is` Expose)
            it "does not expose with harm" do
                Sim.use "Kazekage Puppet Summoning"
                Sim.act
                Sim.as Enemy $ return ()
                not <$> target (`is` Expose)
            it "alternates" do
                Sim.use "Kazekage Puppet Summoning"
                Sim.act
                user $ hasSkill "Poison Gas"

        useOn Enemies "Poison Gas" do
            it "lasts 1 turn normally" do
                Sim.use "Kazekage Puppet Summoning"
                Sim.turns 1
                Sim.use "Thousand Arms"
                Sim.as Enemy $ return ()
                Sim.act
                Sim.turns 1
                targetExhausted <- target $ Effects.exhaust [All]
                targetExhausted `shouldBe` []
            it "lasts 2 turns if target is Pinned" do
                Sim.use "Kazekage Puppet Summoning"
                Sim.turns 1
                Sim.use "Thousand Arms"
                Sim.turns -1
                Sim.act
                Sim.turns 1
                targetExhausted <- target $ Effects.exhaust [All]
                targetExhausted `shouldBe` [Rand]

    describeCharacter "Hidan" do
        let ritual = traverse_ Sim.use
                     (["Jashin Sigil", "First Blood", "Blood Curse"] :: [Text])
        useOn Enemy "Blood Curse" do
            it "performs the ritual" do
                ritual
                damaged <- measureDamage $ Sim.use "Death Blow"
                damaged `shouldBe` 50
            it "negates damage" do
                ritual
                damaged <- measureDamageTo Self $ Sim.use "Death Blow"
                damaged `shouldBe` 0

        useOn Enemy "Death Blow" do
            it "damages user without ritual" do
                damaged <- measureDamageTo Self Sim.act
                damaged `shouldBe` 50

        useOn Enemy "Self-Mutilation" do
            it "stuns user normally" do
                Sim.act
                userStunned <- user Effects.stun
                userStunned `shouldBe` [All]
            it "does not stun if ritual is ongoing" do
                ritual
                Sim.act
                userStunned <- user Effects.stun
                userStunned `shouldBe` []

    describeCharacter "Kakuzu" do
        useOn Enemy "Pressure Damage" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Searing Migraine"

        useOn Enemy "False Darkness" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Blast Flames"

        useOn Enemy "Earth Grudge" do
            it "does nothing if enemy is above 20 health" do
                setHealth 21
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 21
            it "executes target" do
                setHealth 20
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "heals user" do
                setHealth 20
                Sim.as Enemy $ damage dmg
                healed <- measureHealingTo Self Sim.act
                healed `shouldBe` 35

    describeCharacter "Kisame Hoshigaki" do
        useOn Enemies "Thousand Hungry Sharks" do
            it "damages enemies" do
                Sim.act
                Sim.turns 10
                totalDamage <- Sim.enemies $ (100 -) . health
                sum totalDamage `shouldBe` 10 * 5
            it "damages per stack" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.turns 10
                damaged `shouldBe` 5 * 4
            it "marks target" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.as Enemy $ return ()
                    Sim.as XEnemies $ return ()
                    Sim.turns 10
                damaged `shouldBe` 8 * 5
            it "ignores others once marked" do
                damaged <- measureDamageTo XEnemies do
                    Sim.act
                    Sim.turns testStacks
                    Sim.as Enemy $ return ()
                    Sim.as XEnemies $ return ()
                    Sim.turns 10
                damaged `shouldBe` 5 * testStacks
            it "un-ignores if target dies" do
                damaged <- measureDamageTo XEnemies do
                    Sim.act
                    Sim.as Enemy $ return ()
                    Sim.as XEnemies $ return ()
                    Sim.turns 2
                    kill
                    Sim.turns 10
                damaged `shouldBe` 5 * 3
            it "picks a new target if target dies" do
                damaged <- measureDamageTo XEnemies do
                    Sim.act
                    Sim.as Enemy $ return ()
                    kill
                    Sim.as XEnemies $ return ()
                    Sim.turns 10
                damaged `shouldBe` 5 * 5
            it "deals bonus damage during Exploding Water Shockwave" do
                Sim.use "Exploding Water Shockwave"
                Sim.act
                Sim.turns 10
                totalDamage <- Sim.enemies $ (100 -) . health
                sum totalDamage `shouldBe` 10 * 5 + 3 * 3 * 5

        useOn Enemies "Exploding Water Shockwave" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Shark Dance"

        useOn Enemy "Super Shark Bomb" do
            it "deals no damage initially" do
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 0
            it "damages after 1 turn" do
                Sim.act
                damaged <- measureDamage $ Sim.turns 1
                damaged `shouldBe` 30
            it "counters target" do
                Sim.act
                Sim.withClass Chakra $ Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "damages countered" do
                Sim.act
                damaged <- measureDamage do
                    Sim.withClass Chakra $ Sim.as Enemy $ return ()
                    Sim.turns 1
                damaged `shouldBe` 30 + 20

    describeCharacter "Itachi Uchiha" do
        useOn Self "Susanoo" do
            it "sacrifices health" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.turns 4
                damaged `shouldBe` 10
            it "defends user" do
                Sim.act
                Sim.turns testStacks
                defense <- user totalDefense
                defense `shouldBe` 5 * (testStacks + 1)
            it "alternates A" do
                Sim.act
                user $ hasSkill "Totsuka Blade"
            it "alternates B" do
                Sim.act
                user $ hasSkill "Yata Mirror"

        useOn Enemy "Totsuka Blade" do
            it "drains bloodline" do
                gain [Tai, Blood]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Tai])
            it "drains genjutsu" do
                gain [Tai, Gen]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Tai])
            it "does not drain other" do
                gain [Tai, Nin]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Tai, Nin])

        useOn Enemy "Mirage Crow" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "stuns countered" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical, Ranged]

        useOn Self "Yata Mirror" do
            it "ignores harm" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "exhausts attackers" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                targetExhausted <- Effects.exhaust [All] <$> Sim.targets Enemy
                targetExhausted `shouldBe` [Rand]

    describeCharacter "Konan" do
        useOn Enemy "Paper Cut" do
            it "deals bonus damage if target has Dance of the Shikigami" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Dance of the Shikigami"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout - 15 `shouldBe` 5

    describeCharacter "Zetsu" do
        useOn Self "White Zetsu" do
            it "alternates A" do
                Sim.act
                Sim.use "Black Zetsu"
                user $ hasSkill "White Zetsu"
            it "alternates B" do
                Sim.act
                user $ hasSkill "White Army"
            it "alternates C" do
                Sim.act
                user $ hasSkill "Doppelgänger"

        useOn Self "Black Zetsu" do
            it "alternates A" do
                Sim.act
                user $ hasSkill "White Zetsu"
            it "alternates B" do
                Sim.act
                user $ hasSkill "Underground Roots"
            it "alternates C" do
                Sim.act
                user $ hasSkill "Body Coating"

        useOn Enemy "Doppelgänger" do
            it "does nothing if the target has not used a skill yet" do
                Sim.act
                user $ not . hasSkill "Unnamed"
            it "copies after target uses skill" do
                Sim.as Enemy $ return ()
                Sim.act
                user $ hasSkill "Unnamed"

    describeCharacter "Tobi" do
        useOn Self "Sharingan" do
            it "does not alternate immediately" do
                Sim.act
                user $ not . hasSkill "Kamui"
            it "counters on user" do
                Sim.act
                Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "alternates when countered" do
                Sim.act
                Sim.as Enemy $ return ()
                user $ hasSkill "Kamui"

        let testKamui against = useOn against "Kamui" do
                it "applies itself" do
                    Sim.act
                    target has "Kamui"
                it "cancels if Kamui is used on another" do
                    Sim.act
                    Sim.at XAlly Sim.act
                    not <$> target has "Kamui"
                it "cancels if Kamui Strike is used on another" do
                    Sim.act
                    Sim.at XEnemies $ Sim.use "Kamui Strike"
                    not <$> target has "Kamui"
        testKamui Ally
        testKamui Enemy

        useOn Enemy "Kamui Strike" do
            it "deals bonus damge if target has Kamui" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Sharingan"
                Sim.as Enemy $ return ()
                Sim.use "Kamui"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 20

        useOn Self "Izanagi" do
            it "restores condition" do
                Sim.as Enemy $ apply 1 [Reveal]
                Sim.act
                Sim.turns 4
                user (`is` Reveal)

    describeCharacter "Deva Path Pain" do
        useOn Self "Almighty Push" do
            it "alternates back and forth" do
                Sim.use "Almighty Push"
                Sim.use "Universal Pull"
                Sim.use "Almighty Push"
                Sim.use "Universal Pull"
                return True

        useOn Ally "Universal Pull" do
            it "applies Almighty Push to user if used last turn" do
                Sim.use "Almighty Push"
                Sim.use "Universal Pull"
                targeting Self $ Sim.as Enemy $ apply Permanent [ Reveal ]
                not <$> user (`is` Reveal)
            it "does not apply Almighty Push otherwise" do
                Sim.use "Almighty Push"
                Sim.turns 2
                Sim.use "Universal Pull"
                targeting Self $ Sim.as Enemy $ apply Permanent [ Reveal ]
                user (`is` Reveal)

        useOn Enemy "Chakra Receiver" do
            it "stuns once every pair of turns" do
                Sim.act
                Sim.turns 3
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun once every pair of turns" do
                Sim.act
                Sim.turns 4
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

    describeCharacter "Asura Path Pain" do
        useOn Enemy "Missile Salvo" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Head Cannon"
        useOn Enemy "Guided Missile" do
            it "cycles to Bloodline" do
                Sim.act
                user $ hasSkill "Bloodline Missile"
            it "cycles to Genjutsu" do
                Sim.act
                Sim.turns 1
                user $ hasSkill "Genjutsu Missile"
            it "cycles to Ninjutsu" do
                Sim.act
                Sim.turns 2
                user $ hasSkill "Ninjutsu Missile"
            it "cycles to Taijutsu" do
                Sim.act
                Sim.turns 3
                user $ hasSkill "Taijutsu Missile"
            it "ends afterward" do
                Sim.act
                Sim.turns 4
                not <$> channeling "Guided Missile"

    describeCharacter "Human Path Pain" do
        useOn Enemy "Soul Rip" do
            it "executes at or below 30 health" do
                Sim.use "Mind Invasion"
                setHealth 60
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "does not execute otherwise" do
                Sim.use "Mind Invasion"
                Sim.act
                targetHealth <- target health
                targetHealth `shouldNotBe` 0
            it "absorbs chakra above 30 health" do
                Sim.use "Mind Invasion"
                gain [Blood, Gen]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [Gen])
            it "does not absorb otherwise" do
                Sim.use "Mind Invasion"
                gain [Blood, Gen]
                setHealth 60
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Blood, Gen])

    describeCharacter "Animal Path Pain" do
        useOn Enemy "Summoning: Giant Centipede" do
            it "stuns on inaction" do
                Sim.act
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun during" do
                Sim.act
                Sim.turns 1
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "does not stun on action" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.turns 1
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "alternates" do
                Sim.act
                user $ hasSkill "Summoning: Giant Crustacean"

        useOn Enemy "Summoning: Giant Multi-Headed Dog" do
            it "doubles in damage per harm" do
                damaged <- measureDamageTo XEnemies do
                    Sim.act
                    Sim.as Enemy $ return ()
                    Sim.as Enemy $ return ()
                    Sim.turns 3
                damaged `shouldBe` 10 + 10 * 2 + 10 * 2 * 2
            it "does not carry over stacks" do
                damaged <- measureDamage do
                    Sim.act
                    replicateM_ 4 do
                        unlessM (channeling "Summoning: Giant Multi-Headed Dog") do
                            factory
                            Sim.act
                        Sim.turns 1
                    Sim.turns 3
                damaged `shouldBe` 10 * 3

    describeCharacter "Naraka Path Pain" do
        useOn Enemy "Summoning: King of Hell" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Energy Transfer"

        useOn Enemy "Judgment" do
            it "adds to Summoning: King of Hell defense" do
                Sim.use "Summoning: King of Hell"
                defense <- user totalDefense
                Sim.act
                defense' <- user totalDefense
                defense' - defense `shouldBe` 20
            it "does not add otherwise" do
                Sim.use "Summoning: King of Hell"
                targeting Self demolishAll
                Sim.act
                defense <- user totalDefense
                defense `shouldBe` 0
            it "deals bonus damage if target has Choke Hold" do
                Sim.use "Summoning: King of Hell"
                Sim.use "Choke Hold"
                defense <- user totalDefense
                Sim.act
                defense' <- user totalDefense
                defense' - defense `shouldBe` 20 + 20

    describeCharacter "Nagato" do
        useOn Enemy "Summoning: Gedo Statue" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Control"

        useOn Self "Control" do
            it "reduces damage by up to 25" do
                Sim.use "Summoning: Gedo Statue"
                replicateM_ 6  $ Sim.use "Control"
                damaged <- measureDamage $ Sim.as Enemy $ damage dmg
                dmg - damaged `shouldBe` 25
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    testStacks = 3
