{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.KageSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects
import qualified Sim as Sim

spec :: Spec
spec = parallel do
    describeCharacter "Hashirama Senju" do
        useOn Enemy "Tree Strangulation" do
            it "stuns all during Deep Forest Creation" do
                Sim.use "Deep Forest Creation"
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

        useOn Enemies "Deep Forest Creation" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Deep Forest Flourishing"

    describeCharacter "Tobirama Senju" do
        useOn Enemy "Water Prison" do
            it "deals bonus damage during Water Shockwave" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                apply Permanent [ Enrage
                                , AntiChannel
                                ]
                Sim.use "Water Shockwave"
                setHealth 100
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 15

    describeCharacter "Minato Namikaze" do
        useOn Enemies "Space-Time Marking" do
            it "marks on inaction" do
                Sim.act
                Sim.turns 1
                targetHas "Space-Time Marking"
            it "does not mark otherwise" do
                Sim.act
                Sim.as Enemy $ return ()
                not <$> targetHas "Space-Time Marking"

        useOn XAlly "Reciprocal Round-Robin" do
            it "tags user if target harmed" do
                Sim.act
                Sim.as Enemy $ return ()
                userHas "Round-Robin Surprise Attack"
            it "tags target if user harmed" do
                Sim.act
                self $ Sim.as Enemy $ return ()
                targetHas "Round-Robin Surprise Attack"

    describeCharacter "Hanzō" do
        useOn Self "Major Summoning: Ibuse" do
            it "reduces damage" do
                Sim.act
                Sim.as Enemy $ damage dmg
                userHealth <- user health
                dmg - (100 - userHealth) `shouldBe` dmg `quot` 2
            it "spends Ibuse's health" do
                Sim.act
                Sim.as Enemy $ damage dmg
                ibuseHealth <- userStacks "Major Summoning: Ibuse"
                30 - ibuseHealth `shouldBe` dmg `quot` 2
            it "spends all health" do
                Sim.act
                Sim.as Enemy $ damage (30 + dmg)
                userHealth <- user health
                100 - userHealth `shouldBe` dmg

        useOn Enemies "Poison Fog" do
            it "ends when Ibuse dies" do
                Sim.use "Major Summoning: Ibuse"
                Sim.act
                Sim.turns stacks
                self $ Sim.as Enemy $ damage 80
                Sim.turns 3
                targetHealth <- health <$> Sim.targets XEnemies
                100 - targetHealth `shouldBe` 10 * (stacks + 1)

        useOn Enemy "Sickle Dance" do
            it "deals bonus damage during Major Summoning: Ibuse" do
                Sim.act
                Sim.turns 3
                targetHealth <- target health
                factory
                self factory
                Sim.use "Major Summoning: Ibuse"
                Sim.act
                Sim.turns 3
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Rasa" do
        useOn Enemies "Magnet Technique" do
            it "adds bonus barrier if target has Gold Dust Waterfall" do
                Sim.act
                targetBarrier <- target totalBarrier
                factory
                self factory
                Sim.use "Gold Dust Waterfall"
                targetBarrier' <- target totalBarrier
                Sim.act
                targetBarrier'' <- target totalBarrier
                targetBarrier'' - targetBarrier' - targetBarrier `shouldBe` 5

        useOn Enemies "24-Karat Barricade" do
            it "counters with barrier" do
                Sim.use "Gold Dust Waterfall"
                Sim.as Enemy demolishAll
                Sim.act
                Sim.as Enemy $ return ()
                targetBarrier <- target totalBarrier
                targetBarrier `shouldBe` 20
            it "adds bonus barrier if target has Gold Dust Waterfall" do
                Sim.use "Gold Dust Waterfall"
                targetBarrier <- target totalBarrier
                Sim.act
                Sim.as Enemy $ return ()
                targetBarrier' <- target totalBarrier
                targetBarrier' - targetBarrier `shouldBe` 20 + 10

    describeCharacter "A" do
        useOn Ally "Piercing Four-Fingered" do
            it "alternates" do
                Sim.act
                user $ hasSkill "Three-Fingered Assault"
            it "marks harmers" do
                Sim.act
                Sim.as Enemy $ return ()
                Sim.at Enemy $ targetHas "Piercing Four-Fingered"

        useOn Enemy "Lightning Straight" do
            it "deals damage" do
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20
            it "deals bonus damage during Piercing Four-Fingered" do
                Sim.use "Piercing Four-Fingered"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 5
            it "stuns if target has Piercing Four-Fingered" do
                Sim.use "Piercing Four-Fingered"
                Sim.as Enemy $ return ()
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]
            it "does not stun in a row" do
                Sim.use "Piercing Four-Fingered"
                Sim.as Enemy $ return ()
                Sim.act
                Sim.turns 3
                Sim.as Enemy $ return ()
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` []
            it "deals bonus damage during Three-Fingered Assault" do
                Sim.use "Piercing Four-Fingered"
                Sim.use "Three-Fingered Assault"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 10
            it "deals bonus damage during One-Fingered Assault" do
                Sim.use "Piercing Four-Fingered"
                Sim.use "Three-Fingered Assault"
                Sim.use "One-Fingered Assault"
                Sim.act
                targetHealth <- target health
                100 - targetHealth `shouldBe` 20 + 15
            it "shortens immunity during One-Fingered Assault" do
                Sim.use "Piercing Four-Fingered"
                Sim.use "Three-Fingered Assault"
                Sim.use "One-Fingered Assault"
                Sim.as Enemy $ return ()
                Sim.act
                Sim.turns 3
                Sim.as Enemy $ return ()
                Sim.act
                targetStunned <- target Effects.stun
                targetStunned `shouldBe` [All]

    describeCharacter "Mū" do
        useOn Enemy "Particle Beam" do
            it "deals bonus damage if target is invulnerable" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                apply Permanent [ Invulnerable All ]
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Gengetsu Hōzuki" do
        useOn Enemy "Water Pistol" do
            it "deals bonus damage during Major Summoning: Giant Clam" do
                Sim.act
                targetHealth <- target health
                factory
                self factory
                Sim.use "Major Summoning: Giant Clam"
                Sim.act
                targetHealth' <- target health
                targetHealth - targetHealth' `shouldBe` 10
            it "executes at or below 10 health" do
                setHealth 20
                Sim.act
                targetHealth <- target health
                targetHealth `shouldBe` 0
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    stacks = 3
