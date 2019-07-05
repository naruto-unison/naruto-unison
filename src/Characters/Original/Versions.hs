{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Original.Versions (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "One-Tailed Naruto"
    "Naruto's anger and desperation has forced the nine-tailed beast's power to the surface, transforming him into a single-minded beast. His overflowing tailed-beast chakra heals him and ravages his enemies."
    [ [ Skill.new
        { Skill.name      = "Tailed Beast Rasengan"
        , Skill.desc      = "Naruto slams an orb of ultrapowered chakra into an enemy, dealing 35 damage, stunning them for a turn, and losing 5 health from the backlash. Deals 10 additional damage during [Tailed Beast Chakra Arms], or 10 fewer damage during [Inner Chakra Mode]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                apply 1 [Stun All]
                arms  <- 10 `bonusIf` channeling "Tailed Beast Chakra Arms"
                inner <- (-10) `bonusIf` channeling "Inner Chakra Mode"
                damage (35 + arms + inner)
          , p Self $ sacrifice 0 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Chakra Arms"
        , Skill.desc      = "Naruto unleashes his tailed-beast chakra, dealing 15 damage to all enemies for 3 turns. Cannot be used during [Inner Chakra Mode]."
        , Skill.require   = HasI (-1) "Inner Chakra Mode"
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Blood, Tai]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Enemies $ damage 15 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inner Chakra Mode"
        , Skill.desc      = "Naruto draws in his tailed-beast chakra, gaining 10 points of damage reduction and restoring 15 health for 5 turns. Cannot be used during [Tailed Beast Chakra Arms]."
        , Skill.require   = HasI (-1) "Tailed Beast Chakra Arms"
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Blood, Rand]
        , Skill.cooldown  = 5
        , Skill.channel   = Action 5
        , Skill.effects   =
          [ p Self do
                heal 15
                apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ invuln "Dodge" "Naruto" [Physical] ]
    ] []
  , Character
    "Curse Mark Sasuke"
    "Having acclimated to his Curse Mark, Sasuke draws Orochimaru's vile chakra through it to grow stronger and more monstrous. His powers have grown exponentially, allowing him to pierce or trap enemies with his corrupted chakra."
    [ [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Mixing a Chidori with his corrupt chakra, Sasuke deals 45 piercing damage to an enemy. Corruption weakens the target for 2 turns, decreasing their non-affliction damage by 20. Costs 1 random chakra during [Curse Mark]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                pierce 45
                apply 2 [Weaken All Flat 20]
          ]
        , Skill.changes   = changeWith "Curse Mark" $ setCost [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dark Void"
        , Skill.desc      = "Corrupt chakra engulfs an enemy, stunning them for 2 turns. While active, the target is immune to effects from allies and invulnerable. At the end of the 2 turns, the enemy receives 55 damage. Costs 2 random chakra during [Curse Mark]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Nin, Nin, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ p Enemy $ bomb 2 [Stun All, Invulnerable All, Seal]
                             [ p Expire $ damage 55 ]
          ]
        , Skill.changes   = changeWith "Curse Mark" $ setCost [Rand, Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Curse Mark"
        , Skill.desc      = "Sasuke taps into the power of Orochimaru's curse mark, becoming invulnerable to all skills for 1 turn and losing 20 health."
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self $ apply 1 [Invulnerable All] ]
        }
      ]
    , [ invuln "Sharingan Foresight" "Sasuke" [Mental] ]
    ] []
  , Character
    "Drunken Lee"
    "Accidentally drinking sake has awakened Lee's hidden Drunken Fist powers, completely changing his fighting style. Lee's strength increases the longer he fights, though he is too drunk to focus his attacks on a single opponent."
    [ [ Skill.new
        { Skill.name      = "Unpredictable Assault"
        , Skill.desc      = "Lee lashes out drunkenly, dealing 20 damage to a random enemy and permanently increasing the damage of this skill by 5. During [Drunken Fist], targets a specific enemy and deals 5 additional damage."
        , Skill.classes   = [Physical, Melee, Uncounterable]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Self addStack
          , p REnemy do
                stacks <- userStacks "Unpredictable Assault"
                damage (20 + 5 * stacks)
          ]
        }
      , Skill.new
        { Skill.name      = "Unpredictable Assault"
        , Skill.desc      = "Lee lashes out drunkenly, dealing 20 damage to a random enemy and permanently increasing the damage of this skill by 5. During [Drunken Fist], targets a specific enemy and deals 5 additional damage."
        , Skill.classes   = [Physical, Melee, Uncounterable]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Self addStack
          , p Enemy do
                stacks <- userStacks "Unpredictable Assault"
                damage (25 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Drunken Counter"
        , Skill.desc      = "Lee targets himself or an ally. The first harmful physical skill used on the target next turn will be countered, and Lee will instantly strike the person countered with [Unpredictable Assault]."
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Ally $ apply 1 [Parry Physical $ Play do
                self $ addStacks "Unpredictable Assault" 1
                stacks <- userStacks "Unpredictable Assault"
                damage (20 + 5 * stacks)]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Drunken Fist"
        , Skill.desc      = "Lee assumes the Drunken Fist stance and deals 15 damage to an enemy for 3 turns, ignoring status effects from enemies except chakra cost changes."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Rand, Rand]
        , Skill.channel   = Action 3
        , Skill.cooldown  = 3
        , Skill.start     =
          [ p Self $ vary "Unpredictable Assault" "Unpredictable Assault" ]
        , Skill.effects   =
          [ p Enemy $ damage 15
          , p Self  $ apply 1 [Enrage]
          ]
        }
      ]
    , [ invuln "Dodge" "Lee" [Physical] ]
    ] []
  , Character
    "Shukaku Gaara"
    "Gaara has spent his whole life struggling to contain Shukaku, the one-tailed beast, within himself. As his will fades and sleep rushes to claim him, Shukaku gleefully takes over, eager to wreak havoc."
    [ [ Skill.new
        { Skill.name      = "Desert Hand"
        , Skill.desc      = "Shukaku attacks with a claw of dense sand, dealing 30 damage to an enemy and gaining 10 permanent destructible defense. Costs 1 bloodline chakra during [Tailed Beast Form]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Blood, Rand]
        , Skill.effects   =
          [ p Enemy $ damage 30
          , p Self  $ defend 0 10
          ]
        , Skill.changes   = changeWith "Tailed Beast Form" $ setCost [Blood]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Monstrous Sand Arm"
        , Skill.desc      = "Shukaku squeezes an enemy in a fist of sand, dealing 10 damage to them each turn. If they use a skill, the skill is countered and this effect ends. During [Tailed Beast Form], this skill becomes [Wind Bullet][b][b]."
        , Skill.classes   = [Physical, Melee, Single]
        , Skill.cost      = k [Blood]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Enemy $ trap 0 (OnCounter All) $
                self $ cancelChannel "Monstrous Sand Arm"
          ]
        , Skill.effects   =
          [ p Enemy $ afflict 10 ]
        }
      , Skill.new
        { Skill.name      = "Wind Bullet"
        , Skill.desc      = "Deals 60 damage to an enemy."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Blood, Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ damage 60 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Transformation"
        , Skill.desc      = "Shukaku gradually accumulates sand around Gaara's body, gaining 10 permanent destructible defense each turn for 5 turns. At the end of the 5 turns, Shukaku enters Tailed Beast Form. During [Tailed Beast Form], this skill becomes [Shukaku Full Release][b]."
        , Skill.require   = HasI (-1) "Sand Transformation"
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 6
        , Skill.channel   = Action 5
        , Skill.start     =
          [ p Self $ bombWith [Hidden] (-5) [] [ p Expire do
                tag' "Tailed Beast Form" 0
                setFace 0
                vary' 0 "Montrous Sand Arm" "Wind Bullet"
                vary' 0 "Sand Transformation" "Shukaku Full Release" ]
          ]
        , Skill.effects   =
          [ p Self do
                defend 0 10
                heal 2
          ]
        , Skill.interrupt =
          [ p Self $ remove "Sand Transformation" ]
        }
      , Skill.new
        { Skill.name      = "Shukaku Full Release"
        , Skill.desc      = "Shukaku unleashes the full extent of its power, doubling the damage of its skills for the following turn."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self $ apply 1 [Strengthen All Percent 200] ]
        }
      ]
    , [ invuln "Thick Sand Coat" "Shukaku" [Physical] ]
    ] []
  , Character
    "Rehabilitated Gaara"
    "Following his fateful encounter with Naruto, Gaara has become a kind and loyal friend. The sand that was once an extension of his fear and rage has become a versatile tool for shaping the battlefield."
    [ [ Skill.new
        { Skill.name      = "Sand Shower"
        , Skill.desc      = "A stream of sand flows around Gaara toward an enemy, providing 35 destructible defense to him and dealing 15 damage to the target for 3 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Self  $ defend 1 35
          , p Enemy $ damage 15
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Burial Prison"
        , Skill.desc      = "Gaara traps all enemies in a sinking pit of sand, increasing the costs of their non-mental skills by 1 random chakra for 1 turn. If an enemy uses a non-mental skill, they are freed from [Sand Burial Prison]. While active, this skill becomes [Giant Sand Burial][n][n]."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Nin]
        , Skill.channel   = Control 1
        , Skill.start     = [ p Self $ vary "Sand Burial Prison" "Giant Sand Burial"]
        , Skill.effects   =
          [ p Enemies do
                apply 1 [Exhaust NonMental]
                trap 1 (OnAction NonMental) $ remove "Sand Burial Prison"
          , p Self $ tag' "Giant Sand Burial" 1
          ]
        }
      , Skill.new
        { Skill.name      = "Giant Sand Burial"
        , Skill.desc      = "Demolishes Gaara's destructible barrier and the destructible defense of enemies affected by [Sand Burial Prison], then deals 40 piercing damage to them."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Nin, Nin]
        , Skill.effects   =
          [ p Enemies $ whenM (targetHas "Sand Burial Prison") do
                demolishAll
                pierce 40
          , p Self    $ cancelChannel "Sand Burial Prison"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Tsunami"
        , Skill.desc      = "Massive waves of sand flood the battlefield. For 4 turns, all enemies take 15 damage, their destructible skills are weakened by 10, and the destructible skills of Gaara's team are increased by 10. If an enemy uses a skill with negative destructible defense, their target is damaged for its amount. If they use a skill with negative destructible barrier, they are damaged for its amount."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 4
        , Skill.channel   = Action 4
        , Skill.effects   =
          [ p Enemies do
                damage 15
                apply 1 [Build (-10)]
          , p Allies $  apply 1 [Build 10]
          ]
        }
      ]
    , [ invuln "Shukaku Shield" "Gaara" [Physical] ]
    ] []
  ]
