{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.KidsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Naruto Uzumaki" do
        useOn Enemy "Multi Shadow Clone" do
            it "counters enemy" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "alternates" do
                Sim.act
                user $ hasSkill "Rasen Shuriken"
            it "tags enemy if countered" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                targetHas "Multi Shadow Clone"
            it "does not tag enemy otherwise" do
                Sim.act
                not <$> targetHas "Multi Shadow Clone"

        useOn Enemy "Rasen Shuriken" do
            it "deals bonus damage if target has Multi Shadow Clone" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                tag' "Multi Shadow Clone" Permanent
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 25

    describeCharacter "Sakura Haruno" do
        useOn Enemy "Cherry Blossom Clash" do
            it "deals bonus damage during Seal" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "damages others during Seal" do
                Sim.use "Strength of One Hundred Seal"
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 10
            it "spends a Seal" do
                self $ addStacks "Seal" stacks
                Sim.act
                userStacks <- user $ numAnyStacks "Seal"
                userStacks `shouldBe` stacks - 1

        useOn Self "Seal Release" do
            it "spends a Seal" do
                self $ addStacks "Seal" stacks
                Sim.act
                userStacks <- user $ numAnyStacks "Seal"
                userStacks `shouldBe` stacks - 1

    describeCharacter "Sai" do
        useOn Allies "Ink Mist" do
            it "makes stunned allies invulnerable" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Stun All]
                Sim.as Enemy $ apply Permanent [Reveal]
                target $ not . (`is` Reveal)
            it "gains chakra when depleted" do
                Sim.act
                gain [Gen, Tai]
                Sim.as Enemy $ absorb 1
                chakras <- gameChakras
                chakras `shouldBe` ([Blood, Tai], [Gen])
            it "strengthens user when target damaged" do
                Sim.act
                Sim.as Enemy $ damage 5
                setHealth 100
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 10
            it "does not strengthen with affliction" do
                Sim.act
                Sim.as Enemy $ afflict 5
                setHealth 100
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg `shouldBe` 0
            it "alternates" do
                Sim.act
                user $ hasSkill "Super Beast Scroll: Bird"

    describeCharacter "Kiba Inuzuka" do
        useOn Self "Man-Beast Clone" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Three-Headed Wolf"

        useOn Self "Three-Headed Wolf" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Tail Chasing Rotating Fang"

        useOn XEnemies "Rotating Fang" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30
            it "damages random during Man-Beast Clone" do
                Sim.use "Man-Beast Clone"
                Sim.act
                targetHealth <- health <$> Sim.targets REnemy
                100 - targetHealth `shouldBe` 20
            it "damages all during Three-Headed Wolf" do
                Sim.use "Man-Beast Clone"
                Sim.use "Three-Headed Wolf"
                Sim.act
                targetHealth <- health <$> Sim.targets Enemies
                100 - targetHealth `shouldBe` 20

        useOn Enemy "Fang Over Fang" do
            it "deals bonus damage during Man-Beast Clone" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Man-Beast Clone"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage during Three-Headed Wolf" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Man-Beast Clone"
                Sim.use "Three-Headed Wolf"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 20

    describeCharacter "Shino Aburame" do
        useOn Enemy "Insect Swarm" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Chakra Leech"
            it "deals bonus damage if target has Chakra Leech" do
                Sim.act
                Sim.turns 5
                targetHealth <- target health
                factory
                self factory
                tag' "chakra leech" Permanent
                Sim.act
                Sim.turns 5
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 3 * 5

        useOn Enemy "Chakra Leech" do
            it "tags target" do
                Sim.act
                targetHas "chakra leech"

        useOn Ally "Insect Barricade" do
            it "counters on target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                target $ not . (`is` Reveal)
            it "counters with Gigantic Beetle Infestation" do
                everyone $ addStacks "Gigantic Beetle Infestation" 2
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 3 * 25
            it "does not gain chakra normally" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                Sim.turns 2
                chakras <- gameChakras
                chakras `shouldBe` ([], [])
            it "gains chakra if not countered" do
                Sim.act
                Sim.turns 2
                chakras <- gameChakras
                chakras `shouldBe` ([Blood], [])

        useOn Enemy "Gigantic Beetle Infestation" do
            it "deals no damage initially" do
                Sim.act
                Sim.turns 1
                targetHealth <- target health
                100 - targetHealth `shouldBe` 0
            it "deals damage afterward" do
                Sim.act
                Sim.turns 3
                targetHealth <- target health
                100 - targetHealth `shouldBe` 25
            it "stacks" do
                Sim.act
                Sim.act
                Sim.turns 3
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * 25
            it "removes all stacks" do
                Sim.act
                Sim.act
                Sim.turns 3
                not <$> userHas "Gigantic Beetle Infestation"

    describeCharacter "Hinata Hyūga" do
        useOn Enemy "Pressure Point Strike" do
            it "deals bonus damage during Eight Trigrams Sixty-Four Palms" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Eight Trigrams Sixty-Four Palms"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "removes Eight Trigrams Sixty-Four Palms" do
                tag' "Eight Trigrams Sixty-Four Palms" Permanent
                Sim.act
                not <$>
                    targetHas "Eight Trigrams Sixty-Four Palms"

        useOn Enemy "Gentle Step Twin Lion Fists" do
            it "attacks enemies" do
                Sim.act
                replicateM_ 5 $ Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * 30
            it "increases during Eight Trigrams Sixty-Four Palms" do
                Sim.use "Eight Trigrams Sixty-Four Palms"
                Sim.act
                replicateM_ 5 $ Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 3 * 30

        useOn Enemy "Eight Trigrams Sixty-Four Palms" do
            it "tags harm" do
                Sim.act
                replicateM_ stacks $ Sim.as Enemy $ return ()
                targetStacks <- target $ numAnyStacks "Eight Trigrams Sixty-Four Palms"
                targetStacks `shouldBe` stacks

    describeCharacter "Shikamaru Nara" do
        useOn Enemy "Shadow Sewing" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Shadow Sewing: Hold"

        useOn Enemy "Shadow Sewing: Hold" do
            it "damages target" do
                Sim.use "Shadow Sewing"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20

        useOn Enemy "Long-Range Tactics" do
            it "makes invulnerable on harm" do
                Sim.act
                Sim.as Enemy $ afflict dmg
                Sim.as Self $ return ()
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "pauses from non-affliction damage" do
                Sim.act
                Sim.as Enemy $ pierce dmg
                Sim.as Self $ return ()
                Sim.as Enemy $ apply Permanent [Reveal]
                user (`is` Reveal)
            it "alternates" do
                Sim.act
                Sim.turns 1
                Sim.as Self $ return ()
                user $ hasSkill "Final Explosion"

        useOn Enemy "Expert Analysis" do
            it "undoes counters" do
                trap Permanent (Counter All) $ return ()
                Sim.act
                Sim.as Enemy $ return ()
                Sim.as Self $ apply Permanent [Reveal]
                target (`is` Reveal)
            it "does nothing if target does nothing" do
                trap Permanent (Counter All) $ return ()
                Sim.act
                Sim.as Self $ apply Permanent [Reveal]
                target (`is` Reveal)

    describeCharacter "Chōji Akimichi" do
        useOn Self "Butterfly Mode" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Super-Slam"

    describeCharacter "Ino Yamanaka" do
        useOn Enemy "Mind Destruction" do
            it "counters target" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "copies countered target" do
                Sim.act
                Sim.withClass All $ Sim.as Enemy $ apply Permanent [Reveal]
                user $ hasSkill "Unnamed"

        useOn Enemies "Proxy Surveillance" do
            it "reduces damage reduction" do
                Sim.act
                Sim.as Enemy $ self $ apply Permanent [Reduce [All] Flat stacks]
                damage dmg
                targetHealth <- target health
                (100 - targetHealth) - dmg + stacks `shouldBe` 15

    describeCharacter "Rock Lee" do
        useOn Enemy "Leaf Rising Wind" do
            it "deals more damage with dead allies" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                allies kill
                self $ setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 2 * 10

        useOn Enemy "Leaf Hurricane" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "deals more damage with dead allies" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                allies kill
                self $ setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 2 * 10
            it "damages more consecutively" do
                Sim.act
                targetHealth <- target health
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "reduces more consecutively" do
                Sim.act
                Sim.act
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 20

        useOn Enemy "Full Power of Youth" do
            it "damages target per health lost" do
                Sim.as Enemy $ damage $ 30 * stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 20 * stacks
            it "deals more damage with dead allies" do
                allies kill
                self $ setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 2 * 20

    describeCharacter "Tenten" do
        useOn Self "Switch Loadout" do
            it "defends user" do
                Sim.act
                userDefense <- user totalDefense
                userDefense `shouldBe` 5
            it "alternates 1" do
                Sim.act
                user $ hasSkill "Tensasai"
            it "alternates 2" do
                Sim.act
                user $ hasSkill "Segmented Iron Dome"
            it "alternates 3" do
                Sim.act
                user $ hasSkill "Switch Loadout "
        useOn Self "Switch Loadout " do
            it "defends user" do
                Sim.act
                userDefense <- user totalDefense
                userDefense `shouldBe` 5
            it "alternates 1" do
                Sim.act
                user $ hasSkill "Scroll of Fire"
            it "alternates 2" do
                Sim.act
                user $ hasSkill "Scroll of Wind"
            it "alternates 3" do
                Sim.act
                user $ hasSkill "Switch Loadout  "
        useOn Self "Switch Loadout  " do
            it "defends user" do
                Sim.act
                userDefense <- user totalDefense
                userDefense `shouldBe` 5
            it "alternates 1" do
                Sim.act
                user $ hasSkill "Kunai Grenade"
            it "alternates 2" do
                Sim.act
                user $ hasSkill "Chain Spin"
            it "alternates 3" do
                Sim.act
                user $ hasSkill "Switch Loadout"

    describeCharacter "Neji Hyūga" do
        useOn Enemies "Eight Trigrams Sixty-Four Palms" do
            it "counters" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                target $ not . (`is` Reveal)
            it "alternates when countered" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ hasSkill "Pressure Point Strike"

        useOn Enemy "Pressure Point Strike" do
            it "damages target per stack" do
                replicateM_ stacks Sim.act
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 5 + 5 * stacks

    describeCharacter "Kazekage Gaara" do
        useOn Enemy "Sand Summoning" do
            it "defends allies" do
                Sim.act
                targetDefense <- totalDefense <$> Sim.targets XAlly
                targetDefense `shouldBe` 15
            it "triples damage" do
                Sim.act
                damage stacks
                targetHealth <- target health
                100 - targetHealth `shouldBe` 3 * stacks
            it "quintuples damage" do
                Sim.act
                Sim.act
                damage stacks
                targetHealth <- target health
                100 - targetHealth `shouldBe` 5 * stacks
            it "reduces damage" do
                Sim.act
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 10
            it "reduces more damage" do
                Sim.act
                Sim.act
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` 10 + 10

    describeCharacter "Kankurō" do
        useOn Enemy "Kuroari Trap" do
            it "counters enemy" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                user $ not . (`is` Reveal)
            it "tags countered enemy" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                targetHas "Kuroari Trap"

        useOn Enemy "Karasu Knives" do
            it "damages target" do
                Sim.act
                Sim.turns 3
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 10
            it "deals bonus damage with Kuroari Trap" do
                tag' "Kuroari Trap" Permanent
                Sim.act
                Sim.turns 3
                targetHealth <- target health
                100 - targetHealth `shouldBe` 2 * (20 + 10)

        useOn Allies "Sanshōuo Shield" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Salamander Puppet"

    describeCharacter "Temari" do
        useOn Self "First Moon" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Second Moon"
        useOn Self "Second Moon" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Third Moon"

        useOn Enemy "Cyclone Scythe" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "deals bonus damage during First Moon" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "First Moon"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5
            it "deals bonus damage during Second Moon" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "First Moon"
                Sim.use "Second Moon"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Kabuto Yakushi" do
        useOn Enemy "Chakra Absorbing Snakes" do
            it "does not stun if target does not heal" do
                Sim.act
                setHealth 100
                Sim.as Enemy $ self $ heal 100
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "stuns if target heals" do
                Sim.act
                Sim.as Enemy $ self $ heal 100
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

    describeCharacter "Konohamaru Sarutobi" do
        useOn Enemy "Rasengan" do
            it "damages target" do
                Sim.act
                Sim.turns 2
                targetHealth <- target health
                100 - targetHealth `shouldBe` 25
            it "deals bonus damage if target acts" do
                Sim.act
                Sim.turns 2
                targetHealth <- target health
                factory
                self factory
                Sim.act
                Sim.as Enemy $ return ()
                Sim.turns 2
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 15

        useOn Ally "Quick Recovery" do
            it "resurrects" do
                Sim.act
                Sim.as Enemy kill
                targetHealth <- target health
                targetHealth `shouldBe` 15
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    stacks = 3
