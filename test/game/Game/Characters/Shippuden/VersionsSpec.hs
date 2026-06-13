{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.VersionsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Sage Mode Naruto" do
        useOn Enemy "Frog Kumite" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Rasen Shuriken"

        useOn Enemies "Natural Energy Assault" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Rasengan Barrage"

        useOn Enemies "Rasengan Barrage" do
            it "counters on user" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                not <$> user (`is` Reveal)
            it "damages countered" do
                Sim.act
                damaged <- measureDamage
                         $ Sim.as Enemy $ apply Permanent skillName [Reveal]
                damaged `shouldBe` 30

    describeCharacter "Mangekyō Sasuke" do
        useOn Enemy "Susanoo" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Blazing Arrow"

        useOn Enemy "Blazing Arrow" do
            it "damages immediately if interrupted" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.as Enemy $ apply 1 skillName [Stun All]
                    Sim.turns 1
                damaged `shouldBe` 3 * 15
            it "does not continue to damage if interrupted" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.as Enemy $ apply 1 skillName [Stun All]
                    Sim.turns 5
                damaged `shouldBe` 3 * 15

        useOn Enemy "Amaterasu" do
            it "is cured when target becomes invulnerable" do
                damaged <- measureDamage do
                    Sim.act
                    Sim.turns testStacks
                    Sim.as Enemy $ targeting Self $
                        apply Permanent skillName [Invulnerable Physical]
                    Sim.turns 5
                damaged `shouldBe` 5 * (testStacks + 1)
            it "spreads if helped" do
                Sim.act
                damaged <- measureDamageTo XEnemies do
                    Sim.as XEnemies $ return ()
                    Sim.turns testStacks
                damaged `shouldBe` 5 * (testStacks + 1)
            it "spreads back" do
                Sim.act
                Sim.as XEnemies $ return ()
                factory
                damaged <- measureDamage do
                    Sim.at XEnemies $ Sim.as Enemy $ return ()
                    Sim.turns testStacks
                damaged `shouldBe` 5 * (testStacks + 1)

        useOn Enemy "Yasaka Beads" do
            it "increases damage when Amaterasu is cured" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                replicateM_ testStacks do
                    Sim.use "Amaterasu"
                    Sim.as Enemy $ targeting Self $
                        apply 1 skillName [Invulnerable Physical]
                    Sim.turns 1
                Sim.use "Amaterasu"
                Sim.as Enemy $ targeting Self cureAll
                Sim.turns 1
                factory
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 5 * (testStacks + 1)


    describeCharacter "Regimental Commander Gaara" do
        useOn Enemy "Sand Grasp" do
            it "adds Sand Bombs" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Sand Bomb"
                stacks `shouldBe` testStacks
            it "deals damage per Sand Bomb" do
                addStacks "Sand Bomb" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 10 + 5 * (testStacks + 1)
            it "deals single-target damage normally" do
                Sim.use "Mother's Embrace"
                targeting Self demolishAll
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 0
            it "damages all enemies during Mother's Embrace" do
                Sim.use "Mother's Embrace"
                damaged <- measureDamageTo XEnemies Sim.act
                damaged `shouldBe` 10 + 5

        useOn Self "Mother's Embrace" do
            it "ignores harmful effects" do
                Sim.as Enemy $ apply Permanent skillName [Plague]
                Sim.act
                not <$> user (`is` Plague)
            it "ends when destroyed" do
                Sim.as Enemy $ apply Permanent skillName [Plague]
                Sim.act
                Sim.as Enemy demolishAll
                user (`is` Plague)

        useOn Enemies "Sand Mausoleum Seal" do
            it "damages per Sand Bomb" do
                addStacks "Sand Bomb" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 15 + 5 * testStacks
            it "removes Sand Bombs" do
                addStacks "Sand Bomb" testStacks
                Sim.act
                not <$> target has "Sand Bomb"

    describeCharacter "Puppet Master Kankurō" do
        useOn Enemy "Sasori Surrogate" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Hidden Coil Strike"

        useOn Enemy "Kuroari Trap" do
            it "triggers immediately with Hidden Coil Strike" do
                Sim.act
                Sim.use "Sasori Surrogate"
                Sim.use "Hidden Coil Strike"
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not trigger immediately otherwise" do
                Sim.act
                Sim.turns 1
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "triggers after 5 turns" do
                Sim.act
                Sim.turns 5
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "alternates" do
                Sim.act
                user $ hasSkill "Iron Maiden"

        useOn Enemy "Iron Maiden" do
            it "deals bonus damage if target has Kuroari Ambush" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                Sim.use "Sasori Surrogate"
                Sim.use "Kuroari Trap"
                Sim.use "Hidden Coil Strike"
                cancelChannel "Sasori Surrogate"
                setHealth 100
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 40

        useOn Allies "Salamander Shield" do
            it "redirects from targets" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                not <$> target (`is` Reveal)
            it "redirects to user" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                user (`is` Reveal)
            it "ends when destroyed" do
                Sim.act
                targeting Self $ Sim.as Enemy demolishAll
                Sim.as Enemy $ apply Permanent skillName [Reveal]
                target (`is` Reveal)

    describeCharacter "Sage Mode Kabuto" do
        useOn Self "Sage Transformation" do
            it "cycles to Bloodline" do
                Sim.act
                Sim.turns 1
                channeling "Bloodline Sage"
            it "cycles to Genjutsu" do
                Sim.act
                Sim.turns 2
                channeling "Genjutsu Sage"
            it "cycles to Ninjutsu" do
                Sim.act
                Sim.turns 3
                channeling "Ninjutsu Sage"
            it "cycles to Taijutsu" do
                Sim.act
                Sim.turns 4
                channeling "Taijutsu Sage"
            it "goes one at a time" do
                Sim.act
                Sim.turns 4
                not <$> channeling "Bloodline Sage"
            it "cycles back to Bloodline" do
                Sim.act
                Sim.turns 5
                channeling "Bloodline Sage"

        useOn Ally "DNA Transmission Shadow" do
            it "does not resurrect immediately" do
                kill
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "resurrects after 1 turn" do
                kill
                Sim.act
                Sim.turns 1
                targetHealth <- target health
                targetHealth `shouldBe` 100
            it "kills previous" do
                kill
                Sim.act
                Sim.turns 1
                Sim.at XAlly do
                    kill
                    Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "kills if user dies" do
                kill
                Sim.act
                Sim.turns 1
                targeting Self kill
                Sim.turns 1
                targetHealth <- target health
                targetHealth `shouldBe` 0

    describeCharacter "Eight-Gates Guy" do
        useOn Enemy "Evening Elephant" do
            it "deals increasing damage" do
                damagedWithout <- measureDamage Sim.act
                factory
                targeting Self factory
                replicateM_ testStacks Sim.act
                factory
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 20 * testStacks

        useOn Enemy "Night Guy" do
            it "deals increasing damage" do
                damagedWithout <- measureDamage Sim.act
                factory
                damagedWith <- measureDamage Sim.act
                damagedWith - damagedWithout `shouldBe` 25

    describeCharacter "True Form Sasori" do
        useOn Enemy "Poisonous Chain Skewer" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Impale"

        useOn Enemy "Impale" do
            it "stuns after 2 turns with Poisonous Chain Skewer" do
                Sim.use "Poisonous Chain Skewer"
                Sim.act
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                Sim.act
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

        useOn Enemy "Flamethrower Jets" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Cutting Water Jets"

            let cancelAfter skill = do
                    Sim.act
                    Sim.turns 1
                    Sim.use skill
                    not <$> channeling "Flamethrower Jets"

            it "cancels after Poisonous Chain Skewer" $
                cancelAfter "Poisonous Chain Skewer"

            it "cancels after Impale" do
                Sim.use "Poisonous Chain Skewer"
                cancelAfter "Impale"

            it "cancels after Cutting Water Jets" $
                cancelAfter "Cutting Water Jets"

            it "cancels after Performance of a Hundred Puppets" $
                cancelAfter "Performance of a Hundred Puppets"

            it "cancels after Barrage of a Hundred Puppets" do
                Sim.use "Performance of a Hundred Puppets"
                cancelAfter "Barrage of a Hundred Puppets"

        useOn Allies "Performance of a Hundred Puppets" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Barrage of a Hundred Puppets"
            it "ends when destroyed" do
                Sim.act
                targeting Self $ Sim.as Enemy demolishAll
                user $ not . hasSkill "Barrage of a Hundred Puppets"
            it "does not end when ally defense destroyed" do
                Sim.act
                Sim.as Enemy demolishAll
                user $ hasSkill "Barrage of a Hundred Puppets"

        useOn Enemy "Barrage of a Hundred Puppets" do
            it "stuns after 2 turns" do
                Sim.use "Poisonous Chain Skewer"
                Sim.act
                Sim.turns 2
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun immediately" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []

    describeCharacter "Konan of the Rain" do
        useOn Allies "Sacred Paper Emissary" do
            it "snares enemies on action" do
                Sim.act
                Sim.as Enemy $ return ()
                targetSnared <- Effects.snare <$> Sim.targets Enemy
                targetSnared `shouldBe` 1

        useOn Enemy "Paper Bomb" do
            it "damages target per Paper Shuriken" do
                addStacks "Paper Shuriken" testStacks
                damaged <- measureDamage Sim.act
                damaged `shouldBe` 15 + 10 * testStacks
            it "alternates" do
                Sim.act
                user $ hasSkill "Paper Shuriken"

        useOn Enemy "Paper Shuriken" do
            it "adds a stack" do
                replicateM_ testStacks Sim.act
                stacks <- target numStacks "Paper Shuriken"
                stacks `shouldBe` testStacks

    describeCharacter "White Snake Orochimaru" do
        useOn Enemy "Immortality Transference" do
            it "heals user on execute" do
                Sim.as Enemy $ damage dmg
                Sim.act
                Sim.as Self kill
                userHealth <- user health
                userHealth `shouldBe` 100
            it "does not heal otherwise" do
                Sim.as Enemy $ damage dmg
                Sim.act
                userHealth <- user health
                userHealth `shouldBe` 100 - dmg

        useOn Ally "Curse Mark Release" do
            let resurrect = do
                    Sim.act
                    targeting Self kill
                    Sim.as XAlly $ setHealth 25

            it "does not revive when target is above 25" do
                Sim.act
                targeting Self kill
                Sim.as XAlly $ setHealth 26
                targetHealth <- target health
                targetHealth `shouldBe` 26
            it "does not revive if user is alive" do
                Sim.act
                Sim.as Self $ setHealth 25
                targetHealth <- target health
                targetHealth `shouldBe` 25
            it "kills target to revive" do
                resurrect
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "revives user" do
                resurrect
                userHealth <- user health
                userHealth `shouldBe` 100
            it "alternates A" do
                resurrect
                user $ hasSkill "Kusanagi"
            it "alternates B" do
                resurrect
                user $ hasSkill "Eight-Headed Serpent"
            it "alternates C" do
                resurrect
                user $ hasSkill "Regeneration"

        useOn Enemies "Eight-Headed Serpent" do
            it "stuns stunners" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Stun Mental]
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "removes stuns" do
                Sim.act
                Sim.as Enemy $ apply Permanent skillName [Stun Mental]
                userStunned <- user Effects.stun
                userStunned `shouldBe` []
            it "damages stunners" do
                Sim.act
                setHealth 100
                damaged <- measureDamage
                         $ Sim.as Enemy $ apply Permanent skillName [Stun Mental]
                damaged `shouldBe` 20
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    testStacks = 3
