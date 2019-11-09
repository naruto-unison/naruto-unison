{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.KageSpec (spec) where

import Import

import qualified Game.Engine.Effects as Effects

spec :: Spec
spec = parallel do
    describeCharacter "Hashirama Senju" do
        useOn Enemy "Tree Strangulation" do
            it "stuns all during Deep Forest Creation" do
                use "Deep Forest Creation"
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

        useOn Enemies "Deep Forest Creation" do
            it "alternates" do
                act
                hasSkill "Deep Forest Flourishing" <$> nUser

    describeCharacter "Tobirama Senju" do
        useOn Enemy "Water Prison" do
            it "deals bonus damage during Water Shockwave" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply 0 [Enrage, AntiChannel]
                use "Water Shockwave"
                setHealth 100
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 15

    describeCharacter "Minato Namikaze" do
        useOn Enemies "Space-Time Marking" do
            it "marks on inaction" do
                act
                turns 1
                has "Space-Time Marking" <$> user <*> nTarget
            it "does not mark otherwise" do
                act
                as Enemy $ return ()
                not <$> (has "Space-Time Marking" <$> user <*> nTarget)

        useOn XAlly "Reciprocal Round-Robin" do
            it "tags user if target harmed" do
                act
                as Enemy $ return ()
                hasOwn "Round-Robin Surprise Attack" <$> nUser
            it "tags target if user harmed" do
                act
                self $ as Enemy $ return ()
                has "Round-Robin Surprise Attack" <$> user <*> nTarget

    describeCharacter "Hanzō" do
        useOn Self "Major Summoning: Ibuse" do
            it "reduces damage" do
                act
                as Enemy $ damage dmg
                userHealth <- health <$> nUser
                dmg - (100 - userHealth) `shouldBe` dmg `quot` 2
            it "spends Ibuse's health" do
                act
                as Enemy $ damage dmg
                ibuseHealth <- numAnyStacks "Major Summoning: Ibuse" <$> nUser
                30 - ibuseHealth `shouldBe` dmg `quot` 2
            it "spends all health" do
                act
                as Enemy $ damage (30 + dmg)
                userHealth <- health <$> nUser
                100 - userHealth `shouldBe` dmg

        useOn Enemies "Poison Fog" do
            it "ends when Ibuse dies" do
                use "Major Summoning: Ibuse"
                act
                turns stacks
                self $ as Enemy $ damage 80
                turns 3
                targetHealth <- health <$> get XEnemies
                100 - targetHealth `shouldBe` 10 * (stacks + 1)

        useOn Enemy "Sickle Dance" do
            it "deals bonus damage during Major Summoning: Ibuse" do
                act
                turns 3
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Major Summoning: Ibuse"
                act
                turns 3
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Rasa" do
        useOn Enemies "Magnet Technique" do
            it "adds bonus barrier if target has Gold Dust Waterfall" do
                act
                targetBarrier <- totalBarrier <$> nTarget
                factory
                self factory
                use "Gold Dust Waterfall"
                targetBarrier' <- totalBarrier <$> nTarget
                act
                targetBarrier'' <- totalBarrier <$> nTarget
                targetBarrier'' - targetBarrier' - targetBarrier `shouldBe` 5

        useOn Enemies "24-Karat Barricade" do
            it "counters with barrier" do
                use "Gold Dust Waterfall"
                as Enemy demolishAll
                act
                as Enemy $ return ()
                targetBarrier <- totalBarrier <$> nTarget
                targetBarrier `shouldBe` 20
            it "adds bonus barrier if target has Gold Dust Waterfall" do
                use "Gold Dust Waterfall"
                targetBarrier <- totalBarrier <$> nTarget
                act
                as Enemy $ return ()
                targetBarrier' <- totalBarrier <$> nTarget
                targetBarrier' - targetBarrier `shouldBe` 20 + 10

    describeCharacter "A" do
        useOn Ally "Piercing Four-Fingered" do
            it "alternates" do
                act
                hasSkill "Three-Fingered Assault" <$> nUser
            it "marks harmers" do
                act
                as Enemy $ return ()
                has "Piercing Four-Fingered" <$> user <*> get Enemy

        useOn Enemy "Lightning Straight" do
            it "deals damage" do
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20
            it "deals bonus damage during Piercing Four-Fingered" do
                use "Piercing Four-Fingered"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 5
            it "stuns if target has Piercing Four-Fingered" do
                use "Piercing Four-Fingered"
                as Enemy $ return ()
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]
            it "does not stun in a row" do
                use "Piercing Four-Fingered"
                as Enemy $ return ()
                act
                turns 3
                as Enemy $ return ()
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` []
            it "deals bonus damage during Three-Fingered Assault" do
                use "Piercing Four-Fingered"
                use "Three-Fingered Assault"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 10
            it "deals bonus damage during One-Fingered Assault" do
                use "Piercing Four-Fingered"
                use "Three-Fingered Assault"
                use "One-Fingered Assault"
                act
                targetHealth <- health <$> nTarget
                100 - targetHealth `shouldBe` 20 + 15
            it "shortens immunity during One-Fingered Assault" do
                use "Piercing Four-Fingered"
                use "Three-Fingered Assault"
                use "One-Fingered Assault"
                as Enemy $ return ()
                act
                turns 3
                as Enemy $ return ()
                act
                targetStunned <- Effects.stun <$> nTarget
                targetStunned `shouldBe` [All]

    describeCharacter "Mū" do
        useOn Enemy "Particle Beam" do
            it "deals bonus damage if target is invulnerable" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                apply 0 [Invulnerable All]
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10

    describeCharacter "Gengetsu Hōzuki" do
        useOn Enemy "Water Pistol" do
            it "deals bonus damage during Major Summoning: Giant Clam" do
                act
                targetHealth <- health <$> nTarget
                factory
                self factory
                use "Major Summoning: Giant Clam"
                act
                targetHealth' <- health <$> nTarget
                targetHealth - targetHealth' `shouldBe` 10
            it "executes at or below 10 health" do
                setHealth 20
                act
                targetHealth <- health <$> nTarget
                targetHealth `shouldBe` 0
  where
    describeCharacter = describeCategory Reanimated
    dmg = 56
    stacks = 3
