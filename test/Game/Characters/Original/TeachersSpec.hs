{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.TeachersSpec (spec) where

import TestImport

import qualified Class.Play as P
import qualified Game.Engine.Effects as Effects
import qualified Game.Model.Ninja as Ninja

spec :: Spec
spec = parallel do
    describeCharacter "Iruka Umino" \useOn -> do
        useOn Enemy "Shuriken Throw" do
            self $ damage (25 * healthInterval)
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target per health lost" $
                    100 - targetHealth `shouldBe` 20 + 10 * healthInterval
        useOn Ally "Ally Shield" do
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nTarget
            return do
                it "makes target invulnerable" $
                    not harmed
        useOn Enemy "Capture and Arrest" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            enemyTurn $ apply 0 [Reveal]
            targetHealth' <- Ninja.health <$> P.nTarget
            setHealth 100
            damage targetDmg
            targetHealth'' <- Ninja.health <$> P.nTarget
            return do
                it "traps target" $
                    targetHealth == 100
                it "damages target" $
                    100 - targetHealth' `shouldBe` 40
                it "adds to damage against target" $
                    (100 - targetHealth'') - targetDmg `shouldBe` 25

    describeCharacter "Mizuki" \useOn -> do
        useOn Enemy "Kunai Assault" do
            act
            targetHealthAfter1Turn <- Ninja.health <$> P.nTarget
            turns 2
            targetHealthAfterChannel <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Successful Ambush" 0
            act
            targetHealthAfterInstant <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealthAfterChannel `shouldBe` 30
                it "deals all damage at once during Successful Ambush" $
                    100 - targetHealthAfterInstant `shouldBe`
                        2 * (100 - targetHealthAfter1Turn)
        useOn Enemy "Execution Shuriken" do
            damage (20 * healthInterval)
            targetHealth <- Ninja.health <$> P.nTarget
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            factory
            damage (20 * healthInterval)
            self $ tag' "Successful Ambush" 0
            act
            targetHealth'' <- Ninja.health <$> P.nTarget
            return do
                it "damages target per health lost" $
                    targetHealth - targetHealth' `shouldBe`
                        10 + 10 * healthInterval
                it "deals bonus damage during Successful Ambush" $
                    targetHealth' - targetHealth'' `shouldBe` 30
        useOn Self "Genjutsu Ambush Tactics" do
            act
            enemyTurn $ apply 0 [Exhaust All]
            harmedBefore <- (`is` Exhaust All) <$> P.nUser
            enemyTurn $ apply 0 [Reveal]
            harmedAfter <- (`is` Reveal) <$> P.nUser
            self factory
            act
            enemyTurn $ damage targetDmg
            enemyTurn $ apply 0 [Reveal]
            harmedInterrupted <- (`is` Reveal) <$> P.nUser
            return do
                it "waits 1 turn"
                    harmedBefore
                it "makes the user invulnerable after 1 turn" $
                    not harmedAfter
                it "...unless the user is damaged during that turn"
                    harmedInterrupted

    describeCharacter "Kakashi Hatake" \useOn -> do
        useOn Enemy "Sharingan" do
            self act
            enemyTurn $ apply 0 [Reveal]
            userHarmed <- (`is` Reveal) <$> P.nUser
            targetHarmed <- (`is` Reveal) <$> P.nTarget
            return do
                it "reflects harmful skills from self" $
                    not userHarmed
                it "reflects harmful skills upon their user"
                    targetHarmed
        useOn Enemy "Summoning: Ninja Hounds" do
            act
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [NonMental]
        useOn Enemy "Lightning Blade" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Summoning: Ninja Hounds" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 50
                it "kills target if affected by Summoning: Ninja Hounds" $
                    targetHealth' `shouldBe` 0

    describeCharacter "Anko Mitarashi" \useOn -> do
        useOn Enemy "Dual Pin" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            exposed <- targetIsExposed
            factory
            tag' "Dragon Flame" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 5
                it "exposes target"
                    exposed
                it "deals bonus damage if target has Dragon Flame" $
                    targetHealth - targetHealth' `shouldBe` 5
        useOn Enemies "Dragon Flame" do
            act
            tagged <- Ninja.has "Dragon Flame" <$> P.user <*> P.nTarget
            turns 4
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "tags targets"
                    tagged
                it "damages targets" $
                    100 - targetHealth `shouldBe` 10 * 2
        useOn Enemy "Twin Snake Sacrifice" do
            tag' "Dual Pin" 0
            act
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "kills target" $
                    targetHealth `shouldBe` 0
                it "kills user" $
                    userHealth `shouldBe` 0
        useOn Enemy "Striking Shadow Snakes" do
            act
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            tag' "Dragon Flame" 0
            act
            turns 5
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20 + 5 * 3
                it "deals bonus damage if target has Dragon Flame" $
                    targetHealth - targetHealth' `shouldBe` 5

    describeCharacter "Hayate Gekkō" \useOn -> do
        useOn Enemy "Secret Sword" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            damage targetDmg
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
                it "strengthens user" $
                    (100 - targetHealth') - targetDmg `shouldBe` 5
        useOn Enemy "Crescent Moon Dance" do
            act
            enemyTurn $ apply 0 [Reveal]
            targetHealth <- Ninja.health <$> P.nTarget
            harmed <- (`is` Reveal) <$> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "counters against user" $
                    not harmed
        useOn Enemy "Transparency Technique" do
            let lessTargetDmg = 30
            let damageTurns   = 4
            act
            damage targetDmg
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self factory
            act
            replicateM_ damageTurns do
                enemyTurn $ damage lessTargetDmg
                enemyTurn $ return ()
            userHealth <- Ninja.health <$> P.nUser
            return do
                it "reduces damage" $
                    100 - userHealth `shouldBe`
                        lessTargetDmg * damageTurns - 25 - 15 - 5
                it "strengthens user" $
                    (100 - targetHealth) - targetDmg `shouldBe` 10

    describeCharacter "Kurenai Yuhi" \useOn -> do
        useOn Enemy "Demonic Illusion: Entrap" do
            self $ addStacks "Illusion" stacks
            act
            targetHealth <- Ninja.health <$> P.nTarget
            exposed <- targetIsExposed
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetExhausted <- Effects.exhaust [All] <$> P.nTarget
            userStacks <- Ninja.numStacks "Illusion" <$> P.user <*> P.nUser
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 10
                it "exposes target"
                    exposed
                it "weakens target" $
                    targetDmg - (100 - userHealth) `shouldBe` 10
                it "adds 1 random chakra to target's skills" $
                    targetExhausted `shouldBe` [Rand]
                it "empowers next Illusory Tree Meld" $
                    userStacks - stacks `shouldBe` 1
        useOn Self "Illusory Tree Meld" do
            addStacks "Illusion" stacks
            act
            defense <- totalDefense <$> P.nUser
            userStacks <- Ninja.numStacks "Illusion" <$> P.user <*> P.nUser
            return do
                it "defends user" $
                    defense `shouldBe` 10 + 5 * stacks
                it "resets stacks" $
                    userStacks `shouldBe` 0
        useOn Enemy "Demonic Illusion: Sylvan Fetters" do
            self $ addStacks "Illusion" stacks
            act
            targetStunned <- Effects.stun <$> P.nTarget
            userStacks <- Ninja.numStacks "Illusion" <$> P.user <*> P.nUser
            return do
                it "stuns target" $
                    targetStunned `shouldBe` [All]
                it "empowers next Illusory Tree Meld" $
                    userStacks - stacks `shouldBe` 1
        useOn Enemy "Sylvan Fetters Attack" do
            tag' "Demonic Illusion: Sylvan Fetters" 0
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30

    describeCharacter "Asuma Sarutobi" \useOn -> do
        useOn Enemies "Flying Swallow" do
            self $ addStacks "Sharpen Blades" stacks
            act
            userStacks <- Ninja.numStacks "Sharpen Blades" <$> P.user <*> P.nUser
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            turns $ 8 + stacks
            targetHealth <- Ninja.health <$> (allyOf =<< P.target)
            return do
                it "damages targets" $
                    100 - targetHealth `shouldBe` (2 + stacks) * 15
                it "reduces damage to team" $
                    targetDmg - (100 - userHealth) `shouldBe` 15
                it "spends all Sharpen Blades" $
                    userStacks `shouldBe` 0
        useOn Enemy "Finishing Blow" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            targetStunned <- Effects.stun <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 35
                it "stuns target" $
                    targetStunned `shouldBe` [All]
        useOn Self "Sharpen Blades" do
            replicateM_ stacks act
            userStacks <- Ninja.numStacks "Sharpen Blades" <$> P.user <*> P.nUser
            return do
                it "adds a stack to user" $
                    userStacks `shouldBe` stacks
        useOn Enemy "Flying Kick" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 35
        useOn Ally "Self-Sacrifice" do
            act
            enemyTurn $ damage targetDmg
            userHealth <- Ninja.health <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "reflects harm from ally" $
                    100 - targetHealth `shouldBe` 0
                it "reflects harm to self" $
                    100 - userHealth `shouldBe` targetDmg

    describeCharacter "Might Guy" \useOn -> do
        useOn Enemy "Leaf Hurricane" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            factory
            self $ tag' "Sixth Gate Opening" 0
            act
            targetHealth' <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 30
                it "deals bonus damage during Sixth Gate Opening" $
                    targetHealth - targetHealth' `shouldBe` 30
        useOn Self "Sixth Gate Opening" do
            act
            userHealth <- Ninja.health <$> P.nUser
            tagged <- Ninja.hasOwn "Sixth Gate Opening" <$> P.nUser
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            self do
                factory
                setHealth 5
            act
            userHealth' <- Ninja.health <$> P.nUser
            return do
                it "sacrifices health" $
                    100 - userHealth `shouldBe` 40
                it "tags user"
                    tagged
                it "cannot kill user" $
                    userHealth' `shouldBe` 1
                it "makes target invulnerable" $
                    not harmed
        useOn Enemy "Counter Punch" do
            act
            enemyTurn $ apply 0 [Reveal]
            harmed <- (`is` Reveal) <$> P.nUser
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "counters target" $
                    not harmed
                it "damages target if countered" $
                    100 - targetHealth `shouldBe` 30

    describeCharacter "Baki" \useOn -> do
        useOn Enemy "Sudden Strike" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 20
        useOn Enemy "Wind Sword" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 40
        useOn Ally "Flak Jacket" do
            act
            defense <- totalDefense <$> P.nTarget
            enemyTurn $ apply 0 [Plague]
            harmed <- (`is` Plague) <$> P.nTarget
            return do
                it "defends target" $
                    defense `shouldBe` 50
                it "enrages target" $
                    not harmed

    describeCharacter "Shizune" \useOn -> do
        useOn Enemy "Poison Needle Volley" do
            act
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 15
        useOn Enemy "Poison Fog" do
            act
            turns 5
            targetHealth <- Ninja.health <$> P.nTarget
            return do
                it "damages target" $
                    100 - targetHealth `shouldBe` 3 * 10
        useOn Ally "Regenerative Healing Technique" do
            enemyTurn do
                apply 0 [Expose]
                damage targetDmg
            act
            targetHealth <- Ninja.health <$> P.nTarget
            exposed <- targetIsExposed
            return do
                it "heals target" $
                    targetDmg - (100 - targetHealth) `shouldBe` 35
                it "cures target" $
                    not exposed
  where
    describeCharacter = describeCategory Original
    targetDmg = 55
    stacks = 3
    healthInterval = 2
