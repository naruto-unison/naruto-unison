{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.VersionsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Sage Mode Naruto" do
        useOn Enemy "Frog Kumite" do
            it "alternates" do
                act
                hasSkill "Rasen Shuriken" <$> nUser

        useOn Enemies "Natural Energy Assault" do
            it "alternates" do
                act
                hasSkill "Rasengan Barrage" <$> nUser

        useOn Enemies "Rasengan Barrage" do
            it "counters on user" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered" do
                act
                as Enemy $ apply 0 [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30

    describeCharacter "Mangekyō Sasuke" do
        useOn Enemy "Susanoo" do
            it "alternates" do
                act
                hasSkill "Blazing Arrow" <$> nUser

        useOn Enemy "Blazing Arrow" do
            it "damages immediately if interrupted" do
                act
                as Enemy $ apply 1 [Stun All]
                turns 1
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * 15
            it "does not continue to damage if interrupted" do
                act
                as Enemy $ apply 1 [Stun All]
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * 15

        useOn Enemy "Amaterasu" do
            it "is cured when target becomes invulnerable" do
                act
                turns stacks
                as Enemy $ self $ apply 0 [Invulnerable Physical]
                turns 5
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 * (stacks + 1)
            it "spreads if helped" do
                act
                as XEnemies $ return ()
                turns stacks
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 5 * (stacks + 1)
            it "spreads back" do
                act
                as XEnemies $ return ()
                factory
                at XEnemies $ as Enemy $ return ()
                turns stacks
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 5 * (stacks + 1)

        useOn Enemy "Yasaka Beads" do
            it "increases damage when Amaterasu is cured" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                replicateM_ stacks do
                    use "Amaterasu"
                    as Enemy $ self $ apply 1 [Invulnerable Physical]
                    turns 1
                use "Amaterasu"
                as Enemy $ self cureAll
                turns 1
                factory
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5 * (stacks + 1)


    describeCharacter "Regimental Commander Gaara" do
        useOn Enemy "Sand Grasp" do
            it "adds Sand Bombs" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Sand Bomb" <$> nTarget
                targetStacks `shouldBe` stacks
            it "deals damage per Sand Bomb" do
                addStacks "Sand Bomb" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 + 5 * (stacks + 1)
            it "deals single-target damage normally" do
                use "Mother's Embrace"
                self demolishAll
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 0
            it "damages all enemies during Mother's Embrace" do
                use "Mother's Embrace"
                act
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10 + 5

        useOn Self "Mother's Embrace" do
            it "ignores harmful effects" do
                as Enemy $ apply 0 [Plague]
                act
                not . (`is` Plague) <$> nUser
            it "ends when destroyed" do
                as Enemy $ apply 0 [Plague]
                act
                as Enemy demolishAll
                (`is` Plague) <$> nUser

        useOn Enemies "Sand Mausoleum Seal" do
            it "damages per Sand Bomb" do
                addStacks "Sand Bomb" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 5 * stacks
            it "removes Sand Bombs" do
                addStacks "Sand Bomb" stacks
                act
                not <$> (has "Sand Bomb" <$> user <*> nTarget)

    describeCharacter "Puppet Master Kankurō" do
        useOn Enemy "Sasori Surrogate" do
            it "alternates" do
                act
                hasSkill "Hidden Coil Strike" <$> nUser

        useOn Enemy "Kuroari Trap" do
            it "triggers immediately with Hidden Coil Strike" do
                act
                use "Sasori Surrogate"
                use "Hidden Coil Strike"
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not trigger immediately otherwise" do
                act
                turns 1
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "triggers after 5 turns" do
                act
                turns 5
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "alternates" do
                act
                hasSkill "Iron Maiden" <$> nUser

        useOn Enemy "Iron Maiden" do
            it "deals bonus damage if target has Kuroari Ambush" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply 0 [AntiChannel]
                use "Sasori Surrogate"
                use "Kuroari Trap"
                use "Hidden Coil Strike"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 40

        useOn Allies "Salamander Shield" do
            it "redirects from targets" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget
            it "redirects to user" do
                act
                as Enemy $ apply 0 [Reveal]
                (`is` Reveal) <$> nUser
            it "ends when destroyed" do
                act
                self $ as Enemy demolishAll
                as Enemy $ apply 0 [Reveal]
                (`is` Reveal) <$> nTarget

    describeCharacter "Sage Mode Kabuto" do
        useOn Self "Sage Transformation" do
            it "cycles to Bloodline" do
                act
                turns 1
                isChanneling "Bloodline Sage" <$> nUser
            it "cycles to Genjutsu" do
                act
                turns 2
                isChanneling "Genjutsu Sage" <$> nUser
            it "cycles to Ninjutsu" do
                act
                turns 3
                isChanneling "Ninjutsu Sage" <$> nUser
            it "cycles to Taijutsu" do
                act
                turns 4
                isChanneling "Taijutsu Sage" <$> nUser
            it "goes one at a time" do
                act
                turns 4
                not . isChanneling "Bloodline Sage" <$> nUser
            it "cycles back to Bloodline" do
                act
                turns 5
                isChanneling "Bloodline Sage" <$> nUser

        useOn Ally "DNA Transmission Shadow" do
            it "does not resurrect immediately" do
                kill
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "resurrects after 1 turn" do
                kill
                act
                turns 1
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 100
            it "kills previous" do
                kill
                act
                turns 1
                at XAlly do
                    kill
                    act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "kills if user dies" do
                kill
                act
                turns 1
                self kill
                turns 1
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0

    describeCharacter "Eight-Gates Guy" do
        useOn Enemy "Evening Elephant" do
            it "deals increasing damage" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                replicateM_ stacks act
                factory
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 20 * stacks

        useOn Enemy "Night Guy" do
            it "deals increasing damage" do
                act
                targetHealth <- health <$> nTarget
                factory
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 25

    describeCharacter "True Form Sasori" do
        useOn Enemy "Poisonous Chain Skewer" do
            it "alternates" do
                act
                hasSkill "Impale" <$> nUser

        useOn Enemy "Impale" do
            it "stuns after 2 turns with Poisonous Chain Skewer" do
                use "Poisonous Chain Skewer"
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []

        useOn Enemy "Flamethrower Jets" do
            it "alternates" do
                act
                hasSkill "Cutting Water Jets" <$> nUser

            let cancelAfter skill = do
                    act
                    turns 1
                    use skill
                    not . isChanneling "Flamethrower Jets" <$> nUser

            it "cancels after Poisonous Chain Skewer" $
                cancelAfter "Poisonous Chain Skewer"

            it "cancels after Impale" do
                use "Poisonous Chain Skewer"
                cancelAfter "Impale"

            it "cancels after Cutting Water Jets" $
                cancelAfter "Cutting Water Jets"

            it "cancels after Performance of a Hundred Puppets" $
                cancelAfter "Performance of a Hundred Puppets"

            it "cancels after Barrage of a Hundred Puppets" do
                use "Performance of a Hundred Puppets"
                cancelAfter "Barrage of a Hundred Puppets"

        useOn Allies "Performance of a Hundred Puppets" do
            it "alternates" do
                act
                hasSkill "Barrage of a Hundred Puppets" <$> nUser
            it "ends when destroyed" do
                act
                self $ as Enemy demolishAll
                not . hasSkill "Barrage of a Hundred Puppets" <$> nUser
            it "does not end when ally defense destroyed" do
                act
                as Enemy demolishAll
                hasSkill "Barrage of a Hundred Puppets" <$> nUser

        useOn Enemy "Barrage of a Hundred Puppets" do
            it "stuns after 2 turns" do
                use "Poisonous Chain Skewer"
                act
                turns 2
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun immediately" do
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []

    describeCharacter "Konan of the Rain" do
        useOn Allies "Sacred Paper Emissary" do
            it "snares enemies on action" do
                act
                as Enemy $ return ()
                targetSnared <- Effects.snare <$> get Enemy
                targetSnared `shouldBe` 1

        useOn Enemy "Paper Bomb" do
            it "damages target per Paper Shuriken" do
                addStacks "Paper Shuriken" stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15 + 10 * stacks
            it "alternates" do
                act
                hasSkill "Paper Shuriken" <$> nUser

        useOn Enemy "Paper Shuriken" do
            it "adds a stack" do
                replicateM_ stacks act
                targetStacks <- numAnyStacks "Paper Shuriken" <$> nTarget
                targetStacks `shouldBe` stacks

    describeCharacter "White Snake Orochimaru" do
        useOn Enemy "Immortality Transference" do
            it "heals user on execute" do
                as Enemy $ damage dmg
                act
                as Self kill
                userHealth <- health <$> nUser
                userHealth `shouldBe` 100
            it "does not heal otherwise" do
                as Enemy $ damage dmg
                act
                userHealth <- health <$> nUser
                userHealth `shouldBe` 100 - dmg

        useOn Ally "Curse Mark Release" do
            let resurrect = do
                    act
                    self kill
                    as XAlly $ setHealth 25

            it "does not revive when target is above 25" do
                act
                self kill
                as XAlly $ setHealth 26
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 26
            it "does not revive if user is alive" do
                act
                as Self $ setHealth 25
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 25
            it "kills target to revive" do
                resurrect
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "revives user" do
                resurrect
                userHealth <- health <$> nUser
                userHealth `shouldBe` 100
            it "alternates A" do
                resurrect
                hasSkill "Kusanagi" <$> nUser
            it "alternates B" do
                resurrect
                hasSkill "Eight-Headed Serpent" <$> nUser
            it "alternates C" do
                resurrect
                hasSkill "Regeneration" <$> nUser

        useOn Enemies "Eight-Headed Serpent" do
            it "stuns stunners" do
                act
                as Enemy $ apply 0 [Stun Mental]
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "removes stuns" do
                act
                as Enemy $ apply 0 [Stun Mental]
                userStunned <- Effects.stun <$> nUser
                userStunned `shouldBe` []
            it "damages stunners" do
                act
                setHealth 100
                as Enemy $ apply 0 [Stun Mental]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
  where
    describeCharacter = describeCategory Shippuden
    dmg = 56
    stacks = 3
