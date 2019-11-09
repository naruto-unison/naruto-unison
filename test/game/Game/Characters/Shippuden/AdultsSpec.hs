{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.AdultsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Kakashi Hatake" do
        useOn Enemy "Lightning Beast Fang" do
            it "stuns if damages" do
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                apply 0 [Reduce [Affliction] Flat 25]
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "alternates" do
                act
                hasSkill "Lightning Blade Finisher" <$> nUser

        useOn Enemy "Lightning Blade Finisher" do
            it "deals bonus damage if target has Lightning Beast Fang" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Lightning Beast Fang"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage if target is stunned" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply 0 [Stun All]
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage if both" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply 0 [Stun All]
                use "Lightning Beast Fang"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

        useOn Allies "Team Tactics" do
            it "copies skill" do
                act
                as Enemy $ return ()
                hasSkill "Unnamed" <$> nUser

    describeCharacter "Asuma Sarutobi" do
        useOn Enemy "Thousand Hand Strike" do
            it "alternates" do
                act
                hasSkill "Kannon Strike" <$> nUser

        useOn Enemies "Burning Ash" do
            it "alternates" do
                act
                hasSkill "Burning Ash: Ignite" <$> nUser

        useOn Enemies "Burning Ash: Ignite" do
            it "damages target per Burning Ash" do
                use "Burning Ash"
                turns stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10 * (stacks + 1)
            it "removes Burning Ash" do
                use "Burning Ash"
                turns stacks
                act
                not <$> (has "Burning Ash" <$> user <*> nTarget)

        useOn Enemy "Decapitate" do
            it "executes under 25" do
                setHealth 25
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
            it "does nothing otherwise" do
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 100

    describeCharacter "Might Guy" do
        useOn Enemy "Nunchaku" do
            it "damages target per Single Gate Release" do
                replicateM_ stacks $ use "Single Gate Release"
                act
                turns 4
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 3 * 10 + 5 * stacks
            it "damages attackers" do
                act
                setHealth 100
                withClass Physical $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Fiery Kick" do
            it "damages target per Single Gate Release" do
                replicateM_ stacks $ use "Single Gate Release"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 35 + 5 * stacks

        useOn Self "Single Gate Release" do
            it "does not alternate pre 6" do
                replicateM_ 5 act
                hasSkill "Fiery Kick" <$> nUser
            it "alternates at 6" do
                replicateM_ 6 act
                hasSkill "Asakujaku" <$> nUser
            it "alternates at 7" do
                replicateM_ 7 act
                hasSkill "Hirudora" <$> nUser

    describeCharacter "Maki" do
        useOn Enemy "Binding Cloth" do
            it "stuns from harm" do
                act
                as Enemy $ return ()
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [Physical, Melee]

    describeCharacter "Chiyo" do
        useOn Self "Ten Puppets Collection" do
            it "alternates" do
                act
                hasSkill "Lion Roar Sealing" <$> nUser
            it "alternates other" do
                act
                hasSkill "Three Treasure Suction Crush" <$> nUser
            it "ends when destroyed" do
                act
                as Enemy demolishAll
                turns 1
                not . hasSkill "Three Treasure Suction Crush" <$> nUser

        useOn XEnemies "Three Treasure Suction Crush" do
            it "deals normal damage normally" do
                defend 0 stacks
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30 - stacks
            it "deals affliction damage if target has Lion Roar Sealing" do
                use "Ten Puppets Collection"
                setHealth 100
                defend 0 stacks
                use "Lion Roar Sealing"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 30

        useOn Ally "Self-Sacrifice Reanimation" do
            it "cures harm on death" do
                act
                as Enemy do
                    apply 0 [Reveal]
                    kill
                not . (`is` Reveal) <$> nTarget
            it "heals target on death" do
                act
                as Enemy kill
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 100
            it "sacrifices user health on death" do
                act
                as Enemy kill
                userHealth <- health <$> nUser
                userHealth `shouldBe` 1

    describeCharacter "Akatsuchi" do
        useOn Enemy "Chakra Devour" do
            it "damages target" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 15
            it "depletes taijutsu" do
                gain [Blood, Nin, Tai]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Blood, Nin])
            it "depletes genjutsu" do
                gain [Blood, Nin, Gen]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Blood, Nin])
            it "does not deplete otherwise" do
                gain [Blood, Nin, Nin]
                act
                chakras <- chakra <$> game
                chakras `shouldBe` ([], [Blood, Nin, Nin])

    describeCharacter "Kurotsuchi" do
        useOn Enemy "Water Trumpet" do
            it "stuns if target has Lava Quicklime" do
                use "Lava Quicklime"
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [Physical, Chakra]

    describeCharacter "C" do
        useOn XEnemies "Sensory Technique" do
            it "damages random target" do
                act
                targetHealth <- health <$> get REnemy
                100 - targetHealth `shouldBe` 20
            it "makes user invulnerable if harmed" do
                act
                as Enemy $ return ()
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser

    describeCharacter "Atsui" do
        useOn Enemy "Burning Blade" do
            it "damages attackers" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10

        useOn Enemies "Fire Wall" do
            it "harms enemies on action" do
                act
                as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20

        useOn Enemy "Flame Slice" do
            it "deals bonus damage during Burning Blade" do
                use "Burning Blade"
                setHealth 100
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 25 + 10

    describeCharacter "Omoi" do
        useOn Enemies "Back Slice" do
            it "counters" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered" do
                act
                as Enemy $ apply 0 [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "alternates" do
                act
                hasSkill "Crescent Moon Slice" <$> nUser

        useOn Ally "Paper Bomb" do
            it "deals stacking damage" do
                replicateM_ stacks act
                as Enemy $ return ()
                targetHealth <- health <$> get Enemy
                100 - targetHealth `shouldBe` 20 * stacks

    describeCharacter "Dodai" do
        useOn Enemy "Sensory Technique" do
            it "stuns if target has Rubber Sphere and Rope" do
                use "Rubber Sphere and Rope"
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [Physical, Chakra]

        useOn Ally "Rubber Sphere and Rope" do
            it "makes random ally invulnerable" do
                act
                as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nTarget

    describeCharacter "Darui" do
        useOn Enemy "Laser Circus" do
            it "deals bonus damage if target has Water Wall" do
                act
                targetHealth <- health <$> get XEnemies
                use "Water Wall"
                everyone $ setHealth 100
                act
                targetHealth' <- health <$> get XEnemies
                targetHealth - targetHealth' `shouldBe` 5

        useOn Enemy "Black Panther" do
            it "deals bonus damage if target has Water Wall" do
                act
                targetHealth <- health <$> nTarget
                use "Water Wall"
                everyone $ setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Ao" do
        useOn Enemy "Byakugan" do
            it "damages harmers" do
                act
                withClass NonMental $ as Enemy $ return ()
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Barrier Talisman" do
            it "counters on user" do
                act
                withClass NonMental $ as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "exhausts countered" do
                act
                withClass NonMental $ as Enemy $ return ()
                targetExhausted <- Effects.exhaust [All] <$> nTarget
                targetExhausted `shouldBe` [Rand]

    describeCharacter "Chōjūrō" do
        useOn Enemy "Hiramekarei Twinswords" do
            it "counters on user" do
                act
                withClass Physical $ as Enemy $ apply 0 [Reveal]
                not . (`is` Reveal) <$> nUser
            it "damages countered" do
                act
                withClass Physical $ as Enemy $ apply 0 [Reveal]
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
  where
    describeCharacter = describeCategory Shippuden
    stacks = 3
