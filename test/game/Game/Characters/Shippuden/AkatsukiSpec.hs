{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.AkatsukiSpec (spec) where

import Import

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Madara Uchiha" do
        useOn Self "Mangekyō Sharingan" do
            it "alternates" do
                act
                hasSkill "Eternal Mangekyō Sharingan" <$> nUser

        useOn Self "Susanoo" do
            it "alternates" do
                act
                hasSkill "Armored Susanoo Assault" <$> nUser

        useOn Enemy "Armored Susanoo Assault" do
            it "deals damage per stack of Susanoo" do
                use "Susanoo"
                turns stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 + 5 * (stacks + 1)

        useOn Enemy "Majestic Destroyer Flame" do
            it "damages on defense" do
                act
                setHealth 100
                as Enemy $ defend 0 10
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10
            it "damages on reduce" do
                act
                setHealth 100
                as Enemy $ apply 0 [Reduce [All] Flat 10]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10
            it "does not damage otherwise" do
                act
                setHealth 100
                as Enemy $ damage dmg
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0

    describeCharacter "Deidara" do
        useOn Enemy "C1: Bird Bomb" do
            it "alternates" do
                act
                hasSkill "C3: Megaton Sculpture" <$> nUser

        useOn Enemy "C2: Clay Dragon" do
            it "alternates" do
                act
                hasSkill "C2: Minefield" <$> nUser
            it "alternates other" do
                act
                hasSkill "C2: Dragon Missile" <$> nUser

        useOn Enemy "C2: Minefield" do
            it "damages attacker" do
                act
                withClass NonMental $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10
            it "weakens attacker" do
                act
                withClass NonMental $ as Enemy $ return ()
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 5

        useOn Enemy "C4: Karura" do
            it "alternates" do
                act
                hasSkill "C0: Ultimate Art" <$> nUser

    describeCharacter "Sasori" do
        useOn Self "Kazekage Puppet Summoning" do
            it "alternates" do
                act
                hasSkill "Iron Sand: World Order" <$> nUser

        useOn Enemies "Iron Sand: World Order" do
            it "damages per Iron Sand" do
                use "Kazekage Puppet Summoning"
                turns stacks
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10 + 5 * (stacks + 1)

        useOn Enemy "Poison Blade Assault" do
            it "damages repeatedly" do
                act
                turns 4
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 * 2
            it "ends when destroyed" do
                act
                as Enemy demolishAll
                turns stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20

        useOn Enemies "Thousand Arms" do
            it "exposes targets" do
                act
                turns -1
                targetIsExposed
            it "does not expose with harm" do
                act
                as Enemy $ return ()
                not <$> targetIsExposed
            it "alternates" do
                act
                hasSkill "Poison Gas" <$> nUser

        useOn Enemies "Poison Gas" do
            it "lasts 1 turn normally" do
                use "Kazekage Puppet Summoning"
                turns 1
                use "Thousand Arms"
                as Enemy $ return ()
                act
                turns 1
                targetExhausted <- Effects.exhaust [All] <$> nTarget
                targetExhausted `shouldBe` []
            it "lasts 2 turns if target is Pinned" do
                use "Kazekage Puppet Summoning"
                turns 1
                use "Thousand Arms"
                turns -1
                act
                turns 1
                targetExhausted <- Effects.exhaust [All] <$> nTarget
                targetExhausted `shouldBe` [Rand]

    describeCharacter "Hidan" do
        let ritual = traverse_ use
                     (["Jashin Sigil", "First Blood", "Blood Curse"] :: [Text])
        useOn Enemy "Blood Curse" do
            it "performs the ritual" do
                ritual
                use "Death Blow"
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 50 + 5
            it "negates damage" do
                ritual
                use "Death Blow"
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` 0

        useOn Enemy "Death Blow" do
            it "damages user without ritual" do
                act
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` 50

        useOn Enemy "Self-Mutilation" do
            it "stuns self normally" do
                act
                userStunned <- Effects.stun <$> nUser
                userStunned `shouldBe` [All]
            it "does not stun if ritual is ongoing" do
                ritual
                act
                userStunned <- Effects.stun <$> nUser
                userStunned `shouldBe` []

    describeCharacter "Kakuzu" do
        useOn Enemy "Pressure Damage" do
            it "alternates" do
                act
                hasSkill "Searing Migraine" <$> nUser

        useOn Enemy "False Darkness" do
            it "alternates" do
                act
                hasSkill "Blast Flames" <$> nUser

        useOn Enemy "Earth Grudge" do
            it "does nothing if enemy is above 20 health" do
                setHealth 21
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 21
            it "executes target" do
                setHealth 20
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "heals user" do
                setHealth 20
                as Enemy $ damage dmg
                act
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 35

    describeCharacter "Kisame Hoshigaki" do
        useOn Enemies "Thousand Hungry Sharks" do
            it "damages enemies" do
                act
                turns 10
                targets <- P.enemies =<< P.user
                let totalDamage = sum $ (100 -) . health <$> targets
                totalDamage `shouldBe` 10 * 5
            it "damages per stack" do
                act
                turns 10
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 * 4
            it "marks target" do
                act
                as Enemy $ return ()
                as XEnemies $ return ()
                turns 10
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 8 * 5
            it "ignores others once marked" do
                act
                turns stacks
                as Enemy $ return ()
                as XEnemies $ return ()
                turns 10
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 5 * stacks
            it "un-ignores if target dies" do
                act
                as Enemy $ return ()
                as XEnemies $ return ()
                turns 2
                kill
                turns 10
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 5 * 3
            it "picks a new target if target dies" do
                act
                as Enemy $ return ()
                kill
                as XEnemies $ return ()
                turns 10
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 5 * 5
            it "deals bonus damage during Exploding Water Shockwave" do
                use "Exploding Water Shockwave"
                act
                turns 10
                targets <- P.enemies =<< P.user
                let totalDamage = sum $ (100 -) . health <$> targets
                totalDamage `shouldBe` 10 * 5 + 3 * 3 * 5

        useOn Enemies "Exploding Water Shockwave" do
            it "alternates" do
                act
                hasSkill "Shark Dance" <$> nUser

        useOn Enemy "Super Shark Bomb" do
            it "deals no damage initially" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "damages after 1 turn" do
                act
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30
            it "counters target" do
                act
                withClass Chakra $ as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered" do
                act
                withClass Chakra $ as Enemy $ return ()
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 + 20

    describeCharacter "Itachi Uchiha" do
        useOn Self "Susanoo" do
            it "sacrifices health" do
                act
                turns 4
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` 10
            it "defends user" do
                act
                turns stacks
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 5 * (stacks + 1)
            it "alternates A" do
                act
                hasSkill "Totsuka Blade" <$> nUser
            it "alternates B" do
                act
                hasSkill "Yata Mirror" <$> nUser

        useOn Enemy "Totsuka Blade" do
            it "drains bloodline" do
                gain [Tai, Blood]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Tai])
            it "drains genjutsu" do
                gain [Tai, Gen]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Tai])
            it "does not drain other" do
                gain [Tai, Nin]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Tai, Nin])

        useOn Enemy "Mirage Crow" do
            it "counters target" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "stuns countered" do
                act
                as Enemy $ apply 0 [Reveal]
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [Physical, Ranged]

        useOn Self "Yata Mirror" do
            it "ignores harm" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "exhausts attackers" do
                act
                as Enemy $ apply 0 [Reveal]
                targetExhausted <- Effects.exhaust [All] <$> get Enemy
                targetExhausted `shouldBe` [Rand]

    describeCharacter "Konan" do
        useOn Enemy "Paper Cut" do
            it "deals bonus damage if target has Dance of the Shikigami" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply 0 [AntiChannel]
                use "Dance of the Shikigami"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Zetsu" do
        useOn Self "White Zetsu" do
            it "alternates A" do
                act
                use "Black Zetsu"
                hasSkill "White Zetsu" <$> nUser
            it "alternates B" do
                act
                hasSkill "White Army" <$> nUser
            it "alternates C" do
                act
                hasSkill "Doppelgänger" <$> nUser

        useOn Self "Black Zetsu" do
            it "alternates A" do
                act
                hasSkill "White Zetsu" <$> nUser
            it "alternates B" do
                act
                hasSkill "Underground Roots" <$> nUser
            it "alternates C" do
                act
                hasSkill "Body Coating" <$> nUser

        useOn Enemy "Doppelgänger" do
            it "does nothing if the target has not used a skill yet" do
                act
                not . hasSkill "Unnamed" <$> nUser
            it "copies after target uses skill" do
                as Enemy $ return ()
                act
                hasSkill "Unnamed" <$> nUser

    describeCharacter "Tobi" do
        useOn Self "Sharingan" do
            it "does not alternate immediately" do
                act
                not . hasSkill "Kamui" <$> nUser
            it "counters on user" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "alternates when countered" do
                act
                as Enemy $ return ()
                hasSkill "Kamui" <$> nUser

        let testKamui against = useOn against "Kamui" do
                it "applies itself" do
                    act
                    has "Kamui" <$> user <*> nTarget
                it "cancels if Kamui is used on another" do
                    act
                    at XAlly act
                    not <$> (has "Kamui" <$> user <*> nTarget)
                it "cancels if Kamui Strike is used on another" do
                    act
                    at XEnemies $ use "Kamui Strike"
                    not <$> (has "Kamui" <$> user <*> nTarget)
        testKamui Ally
        testKamui Enemy

        useOn Enemy "Kamui Strike" do
            it "deals bonus damge if target has Kamui" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Sharingan"
                as Enemy $ return ()
                use "Kamui"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20

        useOn Self "Izanagi" do
            it "restores condition" do
                as Enemy $ apply 1 [Reveal]
                act
                turns 4
                (`is` Reveal) <$> nUser

    describeCharacter "Deva Path Pain" do
        useOn Self "Almighty Push" do
            it "alternates back and forth" do
                use "Almighty Push"
                use "Universal Pull"
                use "Almighty Push"
                use "Universal Pull"
                return True

        useOn Ally "Universal Pull" do
            it "applies Almighty Push to user if used last turn" do
                use "Almighty Push"
                use "Universal Pull"
                self $ as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "does not apply Almighty Push otherwise" do
                use "Almighty Push"
                turns 2
                use "Universal Pull"
                self $ as Enemy $ apply 0 [Reveal]
                (`is` Reveal) <$> nUser

        useOn Enemy "Chakra Receiver" do
            it "stuns once every pair of turns" do
                act
                turns 3
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun once every pair of turns" do
                act
                turns 4
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []

    describeCharacter "Asura Path Pain" do
        useOn Enemy "Missile Salvo" do
            it "alternates" do
                act
                hasSkill "Head Cannon" <$> nUser
        useOn Enemy "Guided Missile" do
            it "cycles to Bloodline" do
                act
                hasSkill "Bloodline Missile" <$> nUser
            it "cycles to Genjutsu" do
                act
                turns 1
                hasSkill "Genjutsu Missile" <$> nUser
            it "cycles to Ninjutsu" do
                act
                turns 2
                hasSkill "Ninjutsu Missile" <$> nUser
            it "cycles to Taijutsu" do
                act
                turns 3
                hasSkill "Taijutsu Missile" <$> nUser
            it "ends afterward" do
                act
                turns 4
                not . isChanneling "Guided Missile" <$> nUser

    describeCharacter "Human Path Pain" do
        useOn Enemy "Soul Rip" do
            it "executes at or below 30 health" do
                use "Mind Invasion"
                setHealth 60
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "does not execute otherwise" do
                use "Mind Invasion"
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldNotBe` 0
            it "absorbs chakra above 30 health" do
                use "Mind Invasion"
                gain [Blood, Gen]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [Gen])
            it "does not absorb otherwise" do
                use "Mind Invasion"
                gain [Blood, Gen]
                setHealth 60
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Blood, Gen])

    describeCharacter "Animal Path Pain" do
        useOn Enemy "Summoning: Giant Centipede" do
            it "stuns on inaction" do
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun during" do
                act
                turns 1
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "does not stun on action" do
                act
                as Enemy $ return ()
                turns 1
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "alternates" do
                act
                hasSkill "Summoning: Giant Crustacean" <$> nUser

        useOn Enemy "Summoning: Giant Multi-Headed Dog" do
            it "doubles in damage per harm" do
                act
                as Enemy $ return ()
                as Enemy $ return ()
                turns 3
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10 + 10 * 2 + 10 * 2 * 2
            it "does not carry over stacks" do
                act
                replicateM_ 4 do
                    unlessM (isChanneling "Summoning: Giant Multi-Headed Dog" <$> nUser) do
                        factory
                        act
                    turns 1
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 * 3

    describeCharacter "Naraka Path Pain" do
        useOn Enemy "Summoning: King of Hell" do
            it "alternates" do
                act
                hasSkill "Energy Transfer" <$> nUser

        useOn Enemy "Judgment" do
            it "adds to Summoning: King of Hell defense" do
                use "Summoning: King of Hell"
                userDefense <- totalDefense <$> nUser
                act
                userDefense' <- totalDefense <$> nUser
                userDefense' - userDefense `shouldBe` 20
            it "does not add otherwise" do
                use "Summoning: King of Hell"
                self demolishAll
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 0
            it "deals bonus damage if target has Choke Hold" do
                use "Summoning: King of Hell"
                use "Choke Hold"
                userDefense <- totalDefense <$> nUser
                act
                userDefense' <- totalDefense <$> nUser
                userDefense' - userDefense `shouldBe` 20 + 20

    describeCharacter "Nagato" do
        useOn Enemy "Summoning: Gedo Statue" do
            it "alternates" do
                act
                hasSkill "Control" <$> nUser

        useOn Self "Control" do
            it "reduces damage by up to 25" do
                use "Summoning: Gedo Statue"
                replicateM_ 6 $ use "Control"
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 25
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    stacks = 3
