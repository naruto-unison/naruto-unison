{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Versions (versionCs) where

import Preludesque
import Game.Functions
import Game.Game
import Game.Structure

versionCs ∷ [Character]
versionCs =
  [ Character
    "One-Tailed Naruto"
    "Naruto's anger and desperation has forced the nine-tailed beast's power to the surface, transforming him into a single-minded beast. His overflowing tailed-beast chakra heals him and ravages his enemies."
    [ [ newSkill
        { label   = "Tailed Beast Rasengan"
        , desc    = "Naruto slams an orb of ultrapowered chakra into an enemy, dealing 35 damage, stunning them for a turn, and losing 5 health from the backlash. Deals 10 additional damage during [Tailed Beast Chakra Arms], or 10 fewer damage during [Inner Chakra Mode]."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [ (Enemy, apply 1 [Stun All]
                            • withChan "Tailed Beast Chakra Arms" 10
                              (withChan "Inner Chakra Mode" (-10) damage) 35)
                    , (Self,  sacrifice 0 5)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Tailed Beast Chakra Arms"
        , desc    = "Naruto unleashes his tailed-beast chakra, dealing 15 damage to all enemies for 3 turns. Cannot be used during [Inner Chakra Mode]."
        , require = HasI (-1) "Inner Chakra Mode"
        , classes = [Chakra, Melee]
        , cost    = χ [Blood, Tai]
        , cd      = 3
        , channel = Action 3
        , effects = [(Enemies, damage 15)]
        }
      ]
    , [ newSkill
        { label   = "Inner Chakra Mode"
        , desc    = "Naruto draws in his tailed-beast chakra, gaining 10 points of damage reduction and restoring 15 health for 5 turns. Cannot be used during [Tailed Beast Chakra Arms]."
        , require = HasI (-1) "Tailed Beast Chakra Arms"
        , classes = [Chakra]
        , cost    = χ [Blood, Rand]
        , cd      = 5
        , channel = Action 5
        , effects = [(Self, heal 15 • apply 1 [Reduce All 10])]
        }
      ]
    , invuln "Dodge" "Naruto" [Physical]
    ] []
  , Character
    "Curse Mark Sasuke"
    "Having acclimated to his Curse Mark, Sasuke draws Orochimaru's vile chakra through it to grow stronger and more monstrous. His powers have grown exponentially, allowing him to pierce or trap enemies with his corrupted chakra."
    [ [ newSkill
        { label   = "Chidori"
        , desc    = "Mixing a Chidori with his corrupt chakra, Sasuke deals 45 piercing damage to an enemy. Corruption weakens the target for 2 turns, decreasing their non-affliction damage by 20. Costs 1 random chakra during [Curse Mark]."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Nin]
        , cd      = 1
        , effects = [(Enemy, pierce 45 • apply 2 [Weaken All 20])]
        , changes = changeWith "Curse Mark" $ setCost [Rand]
        }
      ]
    , [ newSkill
        { label   = "Dark Void"
        , desc    = "Sasuke engulfs an enemy in corrupt chakra, stunning them for 2 turns. While active, the target is immune to effects from allies and invulnerable. At the end of the 2 turns, the enemy receives 55 damage. Costs 2 random chakra during [Curse Mark]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Nin, Rand]
        , cd      = 5
        , effects = [(Enemy, bomb 2 [Stun All, Immune All, Seal]
                             [(Expire, damage 55)])]
        , changes = changeWith "Curse Mark" $ setCost [Rand, Rand]
        }
      ]
    , [ newSkill
        { label   = "Curse Mark"
        , desc    = "Sasuke taps into the power of Orochimaru's curse mark, becoming invulnerable to all skills for 1 turn and losing 20 health."
        , cost    = χ [Blood]
        , cd      = 1
        , effects = [(Self, apply 1 [Immune All])]
        }
      ]
    , invuln "Sharingan Foresight" "Sasuke" [Mental]
    ] []
  , Character
    "Drunken Lee"
    "Accidentally drinking sake has awakened Lee's hidden Drunken Fist powers, completely changing his fighting style. Lee's strength increases the longer he fights, though he is too drunk to focus his attacks on a single opponent."
    [ [ newSkill
        { label   = "Unpredictable Assault"
        , desc    = "Lee lashes out drunkenly, dealing 20 damage to a random enemy and permanently increasing the damage of this skill by 5. During [Drunken Fist], targets a specific enemy and deals 5 additional damage."
        , classes = [Physical, Melee, Uncounterable, Multi]
        , cost    = χ [Tai]
        , effects = [ (Self,   addStack)
                    , (REnemy, perI "Unpredictable Assault" 5 damage 20)
                    ]
        }
      , newSkill
        { label   = "Unpredictable Assault"
        , desc    = "Lee lashes out drunkenly, dealing 20 damage to a random enemy and permanently increasing the damage of this skill by 5. During [Drunken Fist], targets a specific enemy and deals 5 additional damage."
        , classes = [Physical, Melee, Uncounterable, Multi]
        , cost    = χ [Tai]
        , effects = [ (Self, addStack)
                    , (Enemy, perI "Unpredictable Assault" 5 damage 25)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Drunken Counter"
        , desc    = "Lee targets himself or an ally. The first harmful physical skill used on the target next turn will be countered, and Lee will instantly strike the person countered with [Unpredictable Assault]."
        , classes = [Physical, Melee, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(Ally, apply 1 [Parry Physical 0])]
        }
      ]
    , [ newSkill
        { label   = "Drunken Fist"
        , desc    = "Lee assumes the Drunken Fist stance and deals 15 damage to an enemy for 3 turns, ignoring harmful non-damage effects other than chakra cost changes."
        , classes = [Physical, Melee]
        , cost    = χ [Rand, Rand]
        , channel = Action 3
        , cd      = 3
        , start   = [(Self, vary' 0 1)]
        , effects = [ (Enemy, damage 15)
                    , (Self,  apply 1 [Enrage])
                    ]
        }
      ]
    , invuln "Dodge" "Lee" [Physical]
    ] []
  , Character
    "Shukaku Gaara"
    "Gaara has spent his whole life struggling to contain Shukaku, the one-tailed beast, within himself. As his will fades and sleep rushes to claim him, Shukaku gleefully takes over, eager to escape and wreak havoc."
    [ [ newSkill
        { label   = "Desert Hand"
        , desc    = "Shukaku attacks with a claw of dense sand, dealing 30 damage to an enemy and gaining 10 permanent destructible defense. Costs 1 bloodline chakra during [Tailed Beast Form]."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Rand]
        , effects = [ (Enemy, damage 30)
                    , (Self,  defend 0 10)
                    ]
        , changes = changeWith "Tailed Beast Form" $ setCost [Blood]
        }
      ]
    , [ newSkill
        { label   = "Monstrous Sand Arm"
        , desc    = "Shukaku squeezes an enemy in a fist of sand, dealing 10 damage to them each turn. If they use a skill, the skill is countered and this effect ends. During [Tailed Beast Form], this skill becomes [Wind Bullet][b][b]."
        , classes = [Physical, Melee, Single]
        , cost    = χ [Blood]
        , channel = Ongoing 0
        , start   = [(Enemy, trap 0 (OnCounter All) ∘ self 
                             § cancelChannel "Monstrous Sand Arm")]
        , effects = [(Enemy, afflict 10)]
        }
      , newSkill
        { label   = "Wind Bullet"
        , desc    = "Deals 60 damage to an enemy."
        , classes = [Chakra, Ranged]
        , cost    = χ [Blood, Blood]
        , cd      = 1
        , effects = [(Enemy, damage 60)]
        }
      ]
    , [ newSkill
        { label   = "Sand Transformation"
        , desc    = "Shukaku gradually accumulates sand around Gaara's body, gaining 10 permanent destructible defense each turn for 5 turns. At the end of the 5 turns, Shukaku enters Tailed Beast Form. During [Tailed Beast Form], this skill becomes [Shukaku Full Release][b]."
        , require = HasI (-1) "Sand Transformation"
        , classes = [Mental]
        , cost    = χ [Rand, Rand]
        , cd      = 6
        , channel = Action 5
        , start   = [(Self, bombWith [Hidden] (-5) []
                        [(Expire, tag' "Tailed Beast Form" 0
                                • setFace 0 • vary 0 1 1 • vary 0 2 1)])]        
        , effects = [(Self, defend 0 10 • heal 2)] 
        , disrupt = [(Self, remove "Sand Transformation")]   
        }
      , newSkill
        { label   = "Shukaku Full Release"
        , desc    = "Shukaku unleashes the full extent of its power, doubling the damage of its skills for the following turn."
        , classes = [Mental, Unremovable]
        , cost    = χ [Blood]
        , cd      = 1
        , effects = [(Self, apply 1 [Scale All 2])]
        }
      ]
    , invuln "Thick Sand Coat" "Shukaku" [Physical]
    ] []
  , Character
    "Rehabilitated Gaara"
    "Following his fateful encounter with Naruto, Gaara has become a kind and loyal friend. The sand that was once an extension of his fear and rage has become a versatile tool for shaping the battlefield."
    [ [ newSkill
        { label   = "Sand Shower"
        , desc    = "A stream of sand flows around Gaara toward an enemy, providing him with 35 destructible defense and dealing 15 damage to the target for 3 turns."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand, Rand]
        , cd      = 3
        , channel = Action 3
        , effects = [ (Self,  defend 1 35)
                    , (Enemy, damage 15)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Sand Burial Prison"
        , desc    = "Gaara traps all enemies in a sinking pit of sand, increasing the costs of their non-mental skills by 1 random chakra for 1 turn. If an enemy uses a non-mental skill, they are freed from [Sand Burial Prison]. While active, this skill becomes [Giant Sand Burial][n][n]."
        , classes = [Physical, Ranged, Unreflectable]
        , cost    = χ [Nin]
        , start   = [(Self, vary' 1 1)]
        , effects = [ (Enemies, apply 1 [Exhaust NonMental] 
                              • trap 1 (OnAction NonMental) 
                                § remove "Sand Burial Prison")
                    , (Self,    tag' "Giant Sand Burial" 1)
                    ]
        }
      , newSkill
        { label   = "Giant Sand Burial"
        , desc    = "Demolishes Gaara's destructible barrier and the destructible defense of enemies affected by [Sand Burial Prison], then deals 40 piercing damage to them."
        , classes = [Physical, Ranged, Unreflectable]
        , cost    = χ [Nin, Nin]
        , effects = [ (Enemies, ifU "Sand Burial Prison" § demolish ° pierce 40)
                    , (Self,    vary 0 1 0)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Sand Tsunami"
        , desc    = "Gaara fills the battlefield with massive waves of sand. For 4 turns, all enemies take 15 damage, their destructible skills are weakened by 10, and the destructible skills of Gaara's team are increased by 10. If an enemy uses a skill with negative destructible defense, their target is damaged for its amount. If they use a skill with negative destructible barrier, they are damaged for its amount."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin, Nin]
        , cd      = 4
        , channel = Action 4
        , effects = [ (Enemies, damage 15 • apply 1 [Build (-10)])
                    , (Allies,  apply 1 [Build 10])
                    ]
        }
      ]
    , invuln "Shukaku Shield" "Gaara" [Physical]
    ] []
  ]
