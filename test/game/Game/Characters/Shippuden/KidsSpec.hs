{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.KidsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" do
        useOn Enemy "Multi Shadow Clone" do
            it "counters enemy" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "alternates" do
                act
                hasSkill "Rasen Shuriken" <$> nUser
            it "tags enemy if countered" do
                act
                as Enemy $ apply 0 [Reveal]
                has "Multi Shadow Clone" <$> user <*> nTarget
            it "does not tag enemy otherwise" do
                act
                not <$> (has "Multi Shadow Clone" <$> user <*> nTarget)

        useOn Enemy "Rasen Shuriken" do
            it "deals bonus damage if target has Multi Shadow Clone" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                tag' "Multi Shadow Clone" 0
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 25

    describeCharacter "Sakura Haruno" do
        useOn Enemy "Cherry Blossom Clash" do
            it "deals bonus damage during Seal" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Strength of One Hundred Seal"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "damages others during Seal" do
                use "Strength of One Hundred Seal"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10
            it "spends a Seal" do
                self $ addStacks "Seal" stacks
                act
                userStacks <- numAnyStacks "Seal" <$> nUser
                userStacks `shouldBe` stacks - 1

        useOn Self "Seal Release" do
            it "spends a Seal" do
                self $ addStacks "Seal" stacks
                act
                userStacks <- numAnyStacks "Seal" <$> nUser
                userStacks `shouldBe` stacks - 1

    describeCharacter "Sai" do
        useOn Allies "Ink Mist" do
            it "makes stunned allies invulnerable" do
                act
                as Enemy $ apply 0 [Stun All]
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "gains chakra when depleted" do
                act
                gain [Gen, Tai]
                as Enemy $ absorb 1
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood, Tai], [Gen])
            it "strengthens user when target damaged" do
                act
                as Enemy $ damage 5
                setHealth 100
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 10
            it "does not strengthen with affliction" do
                act
                as Enemy $ afflict 5
                setHealth 100
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg `shouldBe` 0
            it "alternates" do
                act
                hasSkill "Super Beast Scroll: Bird" <$> nUser

    describeCharacter "Kiba Inuzuka" do
        useOn Self "Man-Beast Clone" do
            it "alternates" do
                act
                hasSkill "Three-Headed Wolf" <$> nUser

        useOn Self "Three-Headed Wolf" do
            it "alternates" do
                act
                hasSkill "Tail Chasing Rotating Fang" <$> nUser

        useOn XEnemies "Rotating Fang" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30
            it "damages random during Man-Beast Clone" do
                use "Man-Beast Clone"
                act
                targetHealth <- health <$> get REnemy
                100 - targetHealth `shouldBe` 20
            it "damages all during Three-Headed Wolf" do
                use "Man-Beast Clone"
                use "Three-Headed Wolf"
                act
                targetHealth <- health <$> get Enemies
                100 - targetHealth `shouldBe` 20

        useOn Enemy "Fang Over Fang" do
            it "deals bonus damage during Man-Beast Clone" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Man-Beast Clone"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage during Three-Headed Wolf" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Man-Beast Clone"
                use "Three-Headed Wolf"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20

    describeCharacter "Shino Aburame" do
        useOn Enemy "Insect Swarm" do
            it "alternates" do
                act
                hasSkill "Chakra Leech" <$> nUser
            it "deals bonus damage if target has Chakra Leech" do
                act
                turns 5
                targetHealth <- health <$> nTarget
                factory
                self factory
                tag' "chakra leech" 0
                act
                turns 5
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 3 * 5

        useOn Enemy "Chakra Leech" do
            it "tags target" do
                act
                has "chakra leech" <$> user <*> nTarget

        useOn Ally "Insect Barricade" do
            it "counters on target" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "counters with Gigantic Beetle Infestation" do
                everyone $ addStacks "Gigantic Beetle Infestation" 2
                act
                as Enemy $ apply 0 [Reveal]
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 3 * 25
            it "does not gain chakra normally" do
                act
                as Enemy $ apply 0 [Reveal]
                turns 2
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [])
            it "gains chakra if not countered" do
                act
                turns 2
                chakras <- chakra <$> game
                chakras `shouldBe` ([Blood], [])

        useOn Enemy "Gigantic Beetle Infestation" do
            it "deals no damage initially" do
                act
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 0
            it "deals damage afterward" do
                act
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 25
            it "stacks" do
                act
                act
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * 25
            it "removes all stacks" do
                act
                act
                turns 3
                not <$> (has "Gigantic Beetle Infestation" <$> user <*> nUser)

    describeCharacter "Hinata Hyūga" do
        useOn Enemy "Pressure Point Strike" do
            it "deals bonus damage during Eight Trigrams Sixty-Four Palms" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Eight Trigrams Sixty-Four Palms"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "removes Eight Trigrams Sixty-Four Palms" do
                tag' "Eight Trigrams Sixty-Four Palms" 0
                act
                not <$>
                    (has "Eight Trigrams Sixty-Four Palms" <$> user <*> nTarget)

        useOn Enemy "Gentle Step Twin Lion Fists" do
            it "attacks enemies" do
                act
                replicateM_ 5 $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * 30
            it "increases during Eight Trigrams Sixty-Four Palms" do
                use "Eight Trigrams Sixty-Four Palms"
                act
                replicateM_ 5 $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * 30

        useOn Enemy "Eight Trigrams Sixty-Four Palms" do
            it "tags harm" do
                act
                replicateM_ stacks $ as Enemy $ return ()
                targetStacks <- numAnyStacks "Eight Trigrams Sixty-Four Palms"
                                <$> nTarget
                targetStacks `shouldBe` stacks

    describeCharacter "Shikamaru Nara" do
        useOn Enemy "Shadow Sewing" do
            it "alternates" do
                act
                hasSkill "Shadow Sewing: Hold" <$> nUser

        useOn Enemy "Shadow Sewing: Hold" do
            it "damages target" do
                use "Shadow Sewing"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20

        useOn Enemy "Long-Range Tactics" do
            it "makes invulnerable on harm" do
                act
                as Enemy $ afflict dmg
                as Self $ return ()
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "pauses from non-affliction damage" do
                act
                as Enemy $ pierce dmg
                as Self $ return ()
                as Enemy $ apply 0 [Reveal]
                (`is` Reveal) <$> nUser
            it "alternates" do
                act
                turns 1
                as Self $ return ()
                hasSkill "Final Explosion" <$> nUser

        useOn Enemy "Expert Analysis" do
            it "undoes counters" do
                trap 0 (Counter All) $ return ()
                act
                as Enemy $ return ()
                as Self $ apply 0 [Reveal]
                (`is` Reveal) <$> nTarget
            it "does nothing if target does nothing" do
                trap 0 (Counter All) $ return ()
                act
                as Self $ apply 0 [Reveal]
                (`is` Reveal) <$> nTarget

    describeCharacter "Chōji Akimichi" do
        useOn Self "Butterfly Mode" do
            it "alternates" do
                act
                hasSkill "Super-Slam" <$> nUser

    describeCharacter "Ino Yamanaka" do
        useOn Enemy "Mind Destruction" do
            it "counters target" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "copies countered target" do
                act
                withClass All $ as Enemy $ apply 0 [Reveal]
                hasSkill "Unnamed" <$> nUser

        useOn Enemies "Proxy Surveillance" do
            it "reduces damage reduction" do
                act
                as Enemy $ self $ apply 0 [Reduce [All] Flat stacks]
                damage dmg
                targetHealth <- health <$> nTarget
                (100 - targetHealth) - dmg + stacks `shouldBe` 15

    describeCharacter "Rock Lee" do
        useOn Enemy "Leaf Rising Wind" do
            it "deals more damage with dead allies" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                allies kill
                self $ setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 2 * 10

        useOn Enemy "Leaf Hurricane" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "deals more damage with dead allies" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                allies kill
                self $ setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 2 * 10
            it "damages more consecutively" do
                act
                targetHealth <- health <$> nTarget
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "reduces more consecutively" do
                act
                act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 20

        useOn Enemy "Full Power of Youth" do
            it "damages target per health lost" do
                as Enemy $ damage $ 30 * stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 20 * stacks
            it "deals more damage with dead allies" do
                allies kill
                self $ setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 2 * 20

    describeCharacter "Tenten" do
        useOn Self "Switch Loadout" do
            it "defends user" do
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 5
            it "alternates 1" do
                act
                hasSkill "Tensasai" <$> nUser
            it "alternates 2" do
                act
                hasSkill "Segmented Iron Dome" <$> nUser
            it "alternates 3" do
                act
                hasSkill "Switch Loadout " <$> nUser
        useOn Self "Switch Loadout " do
            it "defends user" do
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 5
            it "alternates 1" do
                act
                hasSkill "Scroll of Fire" <$> nUser
            it "alternates 2" do
                act
                hasSkill "Scroll of Wind" <$> nUser
            it "alternates 3" do
                act
                hasSkill "Switch Loadout  " <$> nUser
        useOn Self "Switch Loadout  " do
            it "defends user" do
                act
                userDefense <- totalDefense <$> nUser
                userDefense `shouldBe` 5
            it "alternates 1" do
                act
                hasSkill "Kunai Grenade" <$> nUser
            it "alternates 2" do
                act
                hasSkill "Chain Spin" <$> nUser
            it "alternates 3" do
                act
                hasSkill "Switch Loadout" <$> nUser

    describeCharacter "Neji Hyūga" do
        useOn Enemies "Eight Trigrams Sixty-Four Palms" do
            it "counters" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "alternates when countered" do
                act
                as Enemy $ apply 0 [Reveal]
                hasSkill "Pressure Point Strike" <$> nUser

        useOn Enemy "Pressure Point Strike" do
            it "damages target per stack" do
                replicateM_ stacks act
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 + 5 * stacks

    describeCharacter "Kazekage Gaara" do
        useOn Enemy "Sand Summoning" do
            it "defends allies" do
                act
                targetDefense <- totalDefense <$> get XAlly
                targetDefense `shouldBe` 15
            it "triples damage" do
                act
                damage stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * stacks
            it "quintuples damage" do
                act
                act
                damage stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 * stacks
            it "reduces damage" do
                act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 10
            it "reduces more damage" do
                act
                act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` 10 + 10

    describeCharacter "Kankurō" do
        useOn Enemy "Kuroari Trap" do
            it "counters enemy" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "tags countered enemy" do
                act
                as Enemy $ apply 0 [Reveal]
                has "Kuroari Trap" <$> user <*> nTarget

        useOn Enemy "Karasu Knives" do
            it "damages target" do
                act
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 10
            it "deals bonus damage with Kuroari Trap" do
                tag' "Kuroari Trap" 0
                act
                turns 3
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 2 * (20 + 10)

        useOn Allies "Sanshōuo Shield" do
            it "alternates" do
                act
                hasSkill "Salamander Puppet" <$> nUser

    describeCharacter "Temari" do
        useOn Self "First Moon" do
            it "alternates" do
                act
                hasSkill "Second Moon" <$> nUser
        useOn Self "Second Moon" do
            it "alternates" do
                act
                hasSkill "Third Moon" <$> nUser

        useOn Enemy "Cyclone Scythe" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "deals bonus damage during First Moon" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "First Moon"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5
            it "deals bonus damage during Second Moon" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "First Moon"
                use "Second Moon"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Kabuto Yakushi" do
        useOn Enemy "Chakra Absorbing Snakes" do
            it "does not stun if target does not heal" do
                act
                setHealth 100
                as Enemy $ self $ heal 100
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "stuns if target heals" do
                act
                as Enemy $ self $ heal 100
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

    describeCharacter "Konohamaru Sarutobi" do
        useOn Enemy "Rasengan" do
            it "damages target" do
                act
                turns 2
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 25
            it "deals bonus damage if target acts" do
                act
                turns 2
                targetHealth <- health <$> nTarget
                factory
                self factory
                act
                as Enemy $ return ()
                turns 2
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 15

        useOn Ally "Quick Recovery" do
            it "resurrects" do
                act
                as Enemy kill
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 15
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    stacks = 3
