{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Original.Versions (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "One-Tailed Naruto"
    "Naruto's anger and desperation has forced the nine-tailed beast's power to surface, unleashing Kurama's animalistic fury and boundless chakra. Overflowing with undirected power that could heal or destroy, Naruto has a choice to make: will he protect himself and stay in control, or give in to the beast within?"
    [LeafVillage, Eleven, Genin, Jinchuriki, Sage, Sensor, Wind, Lightning, Earth, Water, Fire, Yin, Yang, Uzumaki]
    [ [ Skill.new
        { Skill.name      = "Tailed Beast Rasengan"
        , Skill.desc      = "Naruto slams an orb of supercharged chakra into an enemy, dealing 35 damage, stunning them for a turn, and losing 5 health from the backlash. Deals 10 additional damage during [Tailed Beast Chakra Arms], or 10 fewer damage during [Inner Chakra Mode]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Stun All]
                arms  <- 10 `bonusIf` channeling "Tailed Beast Chakra Arms"
                inner <- -10 `bonusIf` channeling "Inner Chakra Mode"
                damage (35 + arms + inner)
          , To Self $ sacrifice 0 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Chakra Arms"
        , Skill.desc      = "Naruto unleashes his tailed-beast chakra, dealing 15 damage to all enemies for 3 turns. Cannot be used during [Inner Chakra Mode]."
        , Skill.require   = HasI 0 "Inner Chakra Mode"
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemies $ damage 15 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inner Chakra Mode"
        , Skill.desc      = "Naruto draws in his tailed-beast chakra, gaining 10 points of damage reduction and restoring 15 health for 5 turns. Cannot be used during [Tailed Beast Chakra Arms]."
        , Skill.require   = HasI 0 "Tailed Beast Chakra Arms"
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 5
        , Skill.dur       = Action 5
        , Skill.effects   =
          [ To Self do
                heal 15
                apply 1 [Reduce [All] Flat 10]
          ]
        }
      ]
    , [ invuln "Dodge" "Naruto" [Physical] ]
    ]
  , Character
    "Curse Mark Sasuke"
    "Having acclimated to his Curse Mark, Sasuke draws Orochimaru's vile chakra through it to grow stronger and more monstrous. His powers have grown exponentially, allowing him to pierce or trap enemies with his corrupted chakra."
    [LeafVillage, Genin, Lightning, Fire, Wind, Earth, Water, Yin, Uchiha]
    [ [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Mixing a Chidori with his corrupt chakra, Sasuke deals 45 piercing damage to an enemy. Corruption enfeebles the target for 2 turns, weakening their damage by 20. Costs 1 arbitrary chakra during [Curse Mark]."
        , Skill.classes   = [Bane, Chakra, Melee]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 45
                apply 2 [Weaken [All] Flat 20]
          ]
        , Skill.changes   =
            changeWith "Curse Mark" \x -> x { Skill.cost = [Rand] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dark Void"
        , Skill.desc      = "Corrupt chakra engulfs an enemy, stunning them for 2 turns. While active, the target is invulnerable to allies as well as enemies. At the end of the 2 turns, the enemy receives 55 damage. Costs 2 arbitrary chakra during [Curse Mark]."
        , Skill.classes   = [Bane, Chakra, Ranged, Soulbound]
        , Skill.cost      = [Nin, Nin, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Enemy $
                bombWith [Bypassing] 2 [Stun All, Alone, Invulnerable All]
                    [ To Expire $ damage 55 ]
          ]
        , Skill.changes   =
            changeWith "Curse Mark" \x -> x { Skill.cost = [Rand, Rand] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Curse Mark"
        , Skill.desc      = "Sasuke taps into the power of Orochimaru's curse mark, losing 20 health and becoming invulnerable to all skills for 1 turn."
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                  sacrifice 0 20
                  apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ invuln "Sharingan Foresight" "Sasuke" [Mental] ]
    ]
  , Character
    "Drunken Lee"
    "Accidentally drinking sake has awakened Lee's hidden Drunken Fist powers, completely changing his fighting style. Lee's strength increases the longer he fights, though he is too drunk to focus his attacks on a single opponent."
    [LeafVillage, Eleven, Genin]
    [ [ Skill.new
        { Skill.name      = "Unpredictable Assault"
        , Skill.desc      = "Lee lashes out drunkenly, dealing 20 damage to a random enemy. Every time this skill is used, its damage increases by 5. During [Drunken Fist], targets a specific enemy and deals 5 additional damage."
        , Skill.classes   = [Physical, Melee, Uncounterable]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To REnemy do
                stacks <- userStacks "Unpredictable Assault"
                damage (20 + 5 * stacks)
          , To Self addStack
          ]
        , Skill.changes   =
            changeWithChannel "Drunken Fist" \x ->
              x { Skill.effects =
                  [ To Enemy do
                        stacks <- userStacks "Unpredictable Assault"
                        damage (25 + 5 * stacks)
                  , To Self addStack
                  ]
                }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Drunken Counter"
        , Skill.desc      = "Lee targets himself or an ally. The first physical skill an enemy uses on the target next turn will be countered, and Lee will instantly strike the person countered with [Unpredictable Assault]."
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Ally $ trapFrom 1 (Counter Physical) do
                stacks <- userStacks "Unpredictable Assault"
                damage (20 + 5 * stacks)
                self $ addStack' "Unpredictable Assault"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Drunken Fist"
        , Skill.desc      = "Lee assumes the Drunken Fist stance and deals 15 damage to an enemy for 3 turns, ignoring harmful status effects."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Self $ apply 1 [Enrage]
          , To Enemy $ damage 15
          ]
        }
      ]
    , [ invuln "Dodge" "Lee" [Physical] ]
    ]
  , Character
    "Shukaku Gaara"
    "Gaara has spent his whole life struggling to contain Shukaku, the one-tailed beast, within himself. As his will fades and sleep rushes to claim him, Shukaku gleefully takes over, eager to wreak havoc."
    [SandVillage, Genin, Jinchuriki, Sensor, Wind, Earth, Lightning, SandClan]
    [ [ Skill.new
        { Skill.name      = "Desert Hand"
        , Skill.desc      = "Shukaku attacks with a claw of dense sand, dealing 30 damage to an enemy and gaining 10 permanent destructible defense. Costs 1 bloodline chakra during [Tailed Beast Form]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 30
          , To Self $ defend Permanent 10
          ]
        , Skill.changes   =
            changeWith "Tailed Beast Form" \x -> x { Skill.cost = [Blood] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Monstrous Sand Arm"
        , Skill.desc      = "Shukaku squeezes an enemy in a fist of sand, dealing 10 damage to them every turn. If they use a skill, the skill will be countered and this effect will end. During [Tailed Beast Form], this skill becomes [Wind Bullet][b][b]."
        , Skill.require   = HasI 0 "Monstrous Sand Arm"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.dur       = Control Permanent
        , Skill.start     =
          [ To Enemy $
                trap Permanent Nullified $ cancelChannel "Monstrous Sand Arm"
          ]
        , Skill.effects   =
          [ To Enemy $ damage 10 ]
        }
      , Skill.new
        { Skill.name      = "Wind Bullet"
        , Skill.desc      = "Shukaku fires a sphere of wind like a canonball that deals 60 damage to an enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ damage 60 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Transformation"
        , Skill.desc      = "Shukaku gradually accumulates sand around Gaara's body, gaining 10 permanent destructible defense every turn for 5 turns. At the end of the 5 turns, Shukaku enters Tailed Beast Form for 3 turns. During [Tailed Beast Form], this skill becomes [Shukaku Full Release][b]."
        , Skill.require   = HasI 0 "Sand Transformation"
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 6
        , Skill.dur       = Ongoing -5
        , Skill.start     =
          [ To Self $ bombWith [Hidden] -5 [] [ To Expire $
                apply' "Tailed Beast Form" 3
                    [ Face
                    , Alternate "Monstrous Sand Arm" "Wind Bullet"
                    , Alternate "Sand Transformation" "Shukaku Full Release"
                    ] ]
          ]
        , Skill.effects   =
          [ To Self $ defend Permanent 10 ]
        }
      , Skill.new
        { Skill.name      = "Shukaku Full Release"
        , Skill.desc      = "Shukaku unleashes the full extent of its power, doubling the damage of its skills for 1 turn."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Self $ apply 1 [Strengthen [All] Percent 100] ]
        }
      ]
    , [ invuln "Thick Sand Coat" "Shukaku" [Physical] ]
    ]
  , Character
    "Rehabilitated Gaara"
    "Following his fateful encounter with Naruto, Gaara has become a kind and loyal friend. The sand that was once an extension of his fear and rage has become a versatile tool for shaping the battlefield."
    [SandVillage, Genin, Jinchuriki, Wind, Earth, Lightning, SandClan]
    [ [ Skill.new
        { Skill.name      = "Sand Shower"
        , Skill.desc      = "A stream of sand flows around Gaara toward an enemy, providing him with 35 destructible defense for 3 turns and dealing 15 damage to the target each turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Self $ defend 1 35
          , To Enemy $ damage 15
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Burial Prison"
        , Skill.desc      = "Gaara traps all enemies in a sinking pit of sand, increasing the costs of their non-mental skills by 1 arbitrary chakra for 1 turn. If an enemy uses a non-mental skill, they are freed from [Sand Burial Prison]. Next turn, this skill becomes [Giant Sand Burial][n][n]."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemies do
                apply 1 [Exhaust [NonMental]]
                trap 1 (OnAction NonMental) $ remove "Sand Burial Prison"
          , To Self $ apply' "Giant Sand Burial" 1
                [Alternate "Sand Burial Prison" "Giant Sand Burial"]
          ]
        }
      , Skill.new
        { Skill.name      = "Giant Sand Burial"
        , Skill.desc      = "Demolishes Gaara's destructible barrier and the destructible defense of enemies affected by [Sand Burial Prison], then deals 40 piercing damage to them."
        , Skill.require   = HasU 1 "Sand Burial Prison"
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Blood, Nin]
        , Skill.effects   =
          [ To Enemies do
                demolishAll
                pierce 40
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Tsunami"
        , Skill.desc      = "Massive waves of sand flood the battlefield. For 4 turns, all enemies take 15 damage, their destructible skills are weakened by 10, and the destructible skills of Gaara's team are increased by 10. If they use a skill with negative destructible defense, their target gains destructible barrier equal to its amount. If they use a skill with negative destructible barrier, their target gains destructible defense equal to its amount."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 4
        , Skill.effects   =
          [ To Enemies do
                damage 15
                apply 1 [Build -10]
          , To Allies $ apply 1 [Build 10]
          ]
        }
      ]
    , [ invuln "Shukaku Shield" "Gaara" [Physical] ]
    ]
  ]
