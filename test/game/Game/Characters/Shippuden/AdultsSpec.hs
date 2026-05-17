{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.AdultsSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Kakashi Hatake" do
        useOn Enemy "Lightning Beast Fang" do
            it "stuns if damages" do
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun otherwise" do
                apply Permanent [Reduce [Affliction] Flat 25]
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "alternates" do
                Sim.act
                user $ hasSkill "Lightning Blade Finisher"

        useOn Enemy "Lightning Blade Finisher" do
            it "deals bonus damage if target has Lightning Beast Fang" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Lightning Beast Fang"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage if target is stunned" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                apply Permanent [Stun All]
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "deals bonus damage if both" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                apply Permanent [Stun All]
                Sim.use "Lightning Beast Fang"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

        useOn Allies "Team Tactics" do
            it "copies skill" do
                Sim.act
                Sim.as Enemy $ return ()
                user $ hasSkill "Unnamed"

    describeCharacter "Asuma Sarutobi" do
        useOn Enemy "Thousand Hand Strike" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Kannon Strike"

        useOn Enemies "Burning Ash" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Burning Ash: Ignite"

        useOn Enemies "Burning Ash: Ignite" do
            it "damages target per Burning Ash" do
                Sim.use "Burning Ash"
                Sim.turns stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10 * (stacks + 1)
            it "removes Burning Ash" do
                Sim.use "Burning Ash"
                Sim.turns stacks
                Sim.act
                not <$> targetHas "Burning Ash"

        useOn Enemy "Decapitate" do
            it "executes under 25" do
                setHealth 25
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
            it "does nothing otherwise" do
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 100

    describeCharacter "Might Guy" do
        useOn Enemy "Nunchaku" do
            it "damages target per Single Gate Release" do
                replicateM_ stacks  $ Sim.use "Single Gate Release"
                Sim.act
                Sim.turns 4
                targetHealth <- target health
                100 - targetHealth `shouldBe` 3 * 10 + 5 * stacks
            it "damages attackers" do
                Sim.act
                setHealth 100
                Sim.withClass Physical $ Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Fiery Kick" do
            it "damages target per Single Gate Release" do
                replicateM_ stacks  $ Sim.use "Single Gate Release"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 35 + 5 * stacks

        useOn Self "Single Gate Release" do
            it "does not alternate pre 6" do
                replicateM_ 5 Sim.act
                user $ hasSkill "Fiery Kick"
            it "alternates at 6" do
                replicateM_ 6 Sim.act
                user $ hasSkill "Asakujaku"
            it "alternates at 7" do
                replicateM_ 7 Sim.act
                user $ hasSkill "Hirudora"

    describeCharacter "Maki" do
        useOn Enemy "Binding Cloth" do
            it "stuns from harm" do
                Sim.act
                Sim.as Enemy $ return ()
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical, Melee]

    describeCharacter "Chiyo" do
        useOn Self "Ten Puppets Collection" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Lion Roar Sealing"
            it "alternates other" do
                Sim.act
                user $ hasSkill "Three Treasure Suction Crush"
            it "ends when destroyed" do
                Sim.act
                Sim.as Enemy demolishAll
                Sim.turns 1
                user $ not . hasSkill "Three Treasure Suction Crush"

        useOn XEnemies "Three Treasure Suction Crush" do
            it "deals normal damage normally" do
                defend Permanent stacks
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30 - stacks
            it "deals affliction damage if target has Lion Roar Sealing" do
                Sim.use "Ten Puppets Collection"
                setHealth 100
                defend Permanent stacks
                Sim.use "Lion Roar Sealing"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 30

        useOn Ally "Self-Sacrifice Reanimation" do
            it "cures harm on death" do
                Sim.act
                Sim.as Enemy do
                    apply Permanent [Reveal]
                    kill
                not <$> target (`is` Reveal)
            it "heals target on death" do
                Sim.act
                Sim.as Enemy kill
                targetHealth <- target health
                targetHealth `shouldBe` 100
            it "sacrifices user health on death" do
                Sim.act
                Sim.as Enemy kill
                userHealth <- user health
                userHealth `shouldBe` 1

    describeCharacter "Akatsuchi" do
        useOn Enemy "Chakra Devour" do
            it "damages target" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 15
            it "depletes taijutsu" do
                gain [Blood, Nin, Tai]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Blood, Nin])
            it "depletes genjutsu" do
                gain [Blood, Nin, Gen]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Blood, Nin])
            it "does not deplete otherwise" do
                gain [Blood, Nin, Nin]
                Sim.act
                chakras <- gameChakras
                chakras `shouldBe` ([], [Blood, Nin, Nin])

    describeCharacter "Kurotsuchi" do
        useOn Enemy "Water Trumpet" do
            it "stuns if target has Lava Quicklime" do
                Sim.use "Lava Quicklime"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical, Chakra]

    describeCharacter "C" do
        useOn XEnemies "Sensory Technique" do
            it "damages random target" do
                Sim.act
                targetHealth <- health <$> Sim.targets REnemy
                100 - targetHealth `shouldBe` 20
            it "makes user invulnerable if harmed" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.as Enemy $ apply Permanent [Reveal]
                not <$> user (`is` Reveal)

    describeCharacter "Atsui" do
        useOn Enemy "Burning Blade" do
            it "damages attackers" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10

        useOn Enemies "Fire Wall" do
            it "harms enemies on action" do
                Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20

        useOn Enemy "Flame Slice" do
            it "deals bonus damage during Burning Blade" do
                Sim.use "Burning Blade"
                setHealth 100
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 25 + 10

    describeCharacter "Omoi" do
        useOn Enemies "Back Slice" do
            it "counters" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                not <$> user (`is` Reveal)
            it "damages countered" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "alternates" do
                Sim.act
                user $ hasSkill "Crescent Moon Slice"

        useOn Ally "Paper Bomb" do
            it "deals stacking damage" do
                replicateM_ stacks Sim.act
                Sim.as Enemy $ return ()
                targetHealth <- health <$> Sim.targets Enemy
                100 - targetHealth `shouldBe` 20 * stacks

    describeCharacter "Dodai" do
        useOn Enemy "Sensory Technique" do
            it "stuns if target has Rubber Sphere and Rope" do
                Sim.use "Rubber Sphere and Rope"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [Physical, Chakra]

        useOn Ally "Rubber Sphere and Rope" do
            it "makes random ally invulnerable" do
                Sim.act
                Sim.as Enemy $ apply Permanent [Reveal]
                not <$> target (`is` Reveal)

    describeCharacter "Darui" do
        useOn Enemy "Laser Circus" do
            it "deals bonus damage if target has Water Wall" do
                Sim.act
                targetHealth <- health <$> Sim.targets XEnemies
                Sim.use "Water Wall"
                everyone $ setHealth 100
                Sim.act
                targetHealth' <- health <$> Sim.targets XEnemies
                targetHealth - targetHealth' `shouldBe` 5

        useOn Enemy "Black Panther" do
            it "deals bonus damage if target has Water Wall" do
                Sim.act
                targetHealth <- target health
                Sim.use "Water Wall"
                everyone $ setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Ao" do
        useOn Enemy "Byakugan" do
            it "damages harmers" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                targetHealth <- target health
                100 - targetHealth `shouldBe` 10

        useOn Enemy "Barrier Talisman" do
            it "counters on user" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ apply Permanent [Reveal]
                not <$> user (`is` Reveal)
            it "exhausts countered" do
                Sim.act
                Sim.withClass NonMental $ Sim.as Enemy $ return ()
                targetExhausted <- target $ Effects.exhaust [All]
                targetExhausted `shouldBe` [Rand]

    describeCharacter "Chōjūrō" do
        useOn Enemy "Hiramekarei Twinswords" do
            it "counters on user" do
                Sim.act
                Sim.withClass Physical $ Sim.as Enemy $ apply Permanent [Reveal]
                not <$> user (`is` Reveal)
            it "damages countered" do
                Sim.act
                Sim.withClass Physical $ Sim.as Enemy $ apply Permanent [Reveal]
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
  where
    describeCharacter = describeCategory Shippuden
    stacks = 3
