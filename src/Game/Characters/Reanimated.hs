{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Reanimated (reanimatedCsS) where

import Preludesque

import qualified Game.Ninja as N

import Calculus
import Game.Functions
import Game.Game
import Game.Structure

reanimatedCsS ∷ [Character]
reanimatedCsS = 
  [ Character
    "Hashirama Senju"
    "Resurrected by Orochimaru, Hashirama was the founder of the Hidden Leaf Village and its first Hokage. His unique ability to manipulate wood allows him give life to trees, which protect his allies and impair his enemies."
    [ [ newSkill
        { label   = "Tree Wave Destruction"
        , desc    = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [ (Enemies, damage 10)
                    , (Allies,  defend 0 5)
                    ]
        }
      , newSkill
        { label   = "Tree Wave Destruction"
        , desc    = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , varicd  = True
        , effects = [ (Enemies, damage 10)
                    , (Allies,  defend 0 5)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Tree Strangulation"
        , desc    = "Hashirama encases an enemy in growing trees, dealing 25 damage and stunning their physical and chakra skills for 1 turn. Stuns all skills during [Deep Forest Creation]."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Rand]
        , effects = [ (Enemy, damage 25
                            • ifI "Deep Forest Creation" § apply 1 [Stun All]
                            • ifnotI "Deep Forest Creation" 
                              § apply 1 [Stun Physical, Stun Chakra])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Deep Forest Creation"
        , desc    = "Hashirama transforms the battlefield into a forest. For 2 turns, enemy cooldowns are increased by 1 and the cost of enemy non-mental skills is increased by 1 random chakra. While active, this skill becomes [Deep Forest Flourishing][b][b]."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Blood]
        , effects = [ (Enemies, apply 2 [Snare 1, Exhaust NonMental])
                    , (Self,    tag 2 • vary 2 0 1 • vary 2 2 1)
                    ]
        }
      , newSkill
        { label   = "Deep Forest Flourishing"
        , desc    = "Grants Hashirama's team 30 permanent destructible defense and resets their cooldowns."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Blood]
        , effects = [(Allies, defend 0 30 • resetAll)]
        }
      ]
    , invuln "Parry" "Hashirama" [Physical]
    ] []
  , Character
    "Tobirama Senju"
    "Resurrected by Orochimaru, Hashirama was the second Hokage. His water-manipulating skills flood the battlefield, impairing and harming the enemy team."
    [ [ newSkill
        { label   = "Water Prison"
        , desc    = "Tobirama encases an enemy in water, dealing 15 damage and making them immune to effects from allies for 1 turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin]
        , effects = [(Enemy, withChan "Water Shockwave" 15 damage 15 
                           • apply 1 [Seal])]
        }
      ]
    , [ newSkill
        { label   = "Water Shockwave"
        , desc    = "Tobirama summons a giant wave of water that assaults the enemy team for 3 turns, dealing 15 damage, negating their affliction damage, and increasing the damage of [Water Prison] by 15."
        , classes = [Physical, Ranged]
        , cost    = χ [Gen, Nin]
        , cd      = 3
        , channel = Ongoing 3
        , effects = [(Enemies, damage 15 • apply 1 [Stun Affliction])]
        }
      ]
    , [ newSkill
        { label   = "Infinite Darkness"
        , desc    = "Tobirama plunges the battlefield into darkness, making his team invulnerable to all harmful physical and mental skills for 1 turn."
        , classes = [Mental]
        , cost    = χ [Gen]
        , cd      = 3
        , effects = [(Allies, apply 1 [Immune All])]
        }
      ]
    , invuln "Water Wall" "Tobirama" [Physical]
    ] []
  , Character
    "Haku"
    "Resurrected by Kabuto, Haku remains as loyal to Zabuza as he was in life. With his inherited ice manipulation techniques, he disrupts his enemies while hiding safely behind crystalline mirrors."
    [ [ newSkill
        { label   = "Thousand Needles of Death"
        , desc    = "Haku flings numerous ice needles outward, dealing 10 piercing damage to the enemy team. During [Crystal Ice Mirrors], this skill deals all 30 damage to a single enemy. If an enemy damaged by this skill loses at least 50 health during the same turn, they are stunned for 1 turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood]
        , effects = [(Enemies, pierce 10 
                             • trapPer (-1) TrackDamaged § \i → 
                               if | i ≥ 50    → apply 1 [Stun All]
                                  | otherwise → wait)]
        , changes = changeWith "Crystal Ice Mirrors" $ \_ skill → skill
          { effects = [(Enemy, pierce 30
                             • trapPer (-1) TrackDamaged § \i → 
                               if | i ≥ 50    → apply 1 [Stun All]
                                  | otherwise → wait)] }
        }
      ]
    , [ newSkill
        { label   = "Acupuncture"
        , desc    = "Haku alters the flow of energy in an enemy by sticking a needle into one of their vital points, disabling the non-damage effects of their skills for 2 turns. Bypasses invulnerability and targets all enemies during [Crystal Ice Mirrors]."
        , classes = [Physical, Ranged, Single]
        , cost    = χ [Nin]
        , cd      = 2
        , effects = [(Enemy, apply 2 [Silence])]
        , changes = changeWith "Crystal Ice Mirrors" 
                  $ addClass Bypassing •• targetAll
        }
      ]
    , [ newSkill
        { label   = "Crystal Ice Mirrors"
        , desc    = "Haku fills the battlefield with disorienting crystalline mirrors, gaining 20 permanent destructible defense. For 3 turns, if Haku loses all destructible defense from this skill, he will gain destructible defense equal to how much health he lost during the same turn. Cannot be used while Haku still has destructible defense from this skill."
        , classes = [Chakra, Single]
        , cost    = χ [Blood, Nin]
        , cd      = 6
        , channel = Ongoing 3
        , start   = [(Self, defend 0 20)]
        }
      ]
    , invuln "Ice Dome" "Haku" [Chakra]
    ] 
    [(PerDamaged, \n@Ninja{..} i → 
        if | isChanneling "Crystal Ice Mirrors" n 
           ∧ not (hasDefense "Crystal Ice Mirrors" nId n) 
           → n { nDefense = Defense i nId "Crystal Ice Mirrors" 0 : nDefense }
           | otherwise → n
    )]
  , Character
    "Nagato"
    "Resurrected by Kabuto, Nagato is as much a pawn in the schemes of others as he was in life. With the power of the Rinnegan and all his Paths at his disposal, he uses the attacks of his opponents to strengthen his own abilities."
    [ [ newSkill
        { label   = "Human Path"
        , desc    = "Nagato restores 15 health and deals 20 piercing damage to an enemy. If the target deals any damage next turn, the damage and healing of this skill will be set to the damage they dealt for 1 turn and its cost will increase by 1 random chakra."
        , classes = [Mental, Melee, Multi]
        , cost    = χ [Gen]
        , effects = [ (Self,  heal 15) 
                    , (Enemy, pierce 20 
                            • trap 1 OnDamage ∘ self § vary 1 0 1
                            • trapPer (-1) TrackDamage § self
                              ∘ addStacks' (-1) "Human Path")
                    ]
        }
      , newSkill
        { label   = "Human Path"
        , desc    = "Nagato restores 15 health and deals 20 piercing damage to an enemy. If the target deals any damage next turn, the damage and healing of this skill will be set to the damage they dealt for 1 turn and its cost will remain increased."
        , classes = [Mental, Melee, Multi]
        , cost    = χ [Gen, Rand]
        , effects = [ (Self,  perI "Human Path" 1 heal 0) 
                    , (Enemy, perI "Human Path" 1 pierce 0
                            • trap 1 OnDamage ∘ self § vary 1 0 1
                            • trapPer (-1) TrackDamage § self
                              ∘ addStacks' (-1) "Human Path")
                    ]
        , changes = \n skill → skill 
            { desc = "Nagato restores " ⧺ tshow (numActive "Human Path" n) ⧺ " health and deals " ⧺ tshow (numActive "Human Path" n) ⧺ " piercing damage to an enemy. If the target deals any damage next turn, the damage and healing of this skill will be set to the damage they dealt for 1 turn and its cost will remain increased." }
        }
      ]
    , [ newSkill
        { label   = "Preta Path"
        , desc    = "Nagato absorbs attacks against him, countering all enemy skills next turn. Each countered skill restores 10 health to Nagato and steals a random chakra from its user."
        , classes = [Mental, Ranged, Multi]
        , cost    = χ [Nin]
        , cd      = 4
        , effects = [(Self, apply (-1) [ParryAll All 4])]
        }
      ]  
    , [ newSkill
        { label   = "Naraka Path"
        , desc    = "Nagato tracks a target's damage for 2 turns. If the target is an ally, they are healed for the damage total. If the target is an enemy, they are damaged for the damage total. If the target did not cause any damage, their chakra costs are modified for 2 turns: increased by 1 random chakra if an enemy, decreased by 1 random chakra if an ally. If [Preta Path] countered 2 or more skills last turn, this skill affects all allies and enemies."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Gen]
        , cd      = 4
        , effects = [ (XAlly, trapPer (-2) TrackDamage heal
                            • trap (-2) OnDamage § remove "Naraka Path"
                            • bomb (-2) [] [(Expire, apply 2 [Unexhaust])]) 
                    , (Enemy, trapPer (-2) TrackDamage damage
                            • trap (-2) OnDamage § remove "Naraka Path"
                            • bomb (-2) [] [(Expire, apply 2 [Exhaust All])])
                    ]
        }
      , newSkill
        { label   = "Naraka Path"
        , desc    = "Nagato tracks a target's damage for 2 turns. If the target is an ally, they are healed for the damage total. If the target is an enemy, they are damaged for the damage total. If the target did not cause any damage, their chakra costs are modified for 2 turns: increased by 1 random chakra if an enemy, decreased by 1 random chakra if an ally. If [Preta Path] countered 2 or more skills last turn, this skill affects all allies and enemies."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Gen]
        , cd      = 4
        , effects = [ (XAllies, trapPer (-2) TrackDamage heal
                              • trap (-2) OnDamage § remove "Naraka Path"
                              • bomb (-2) [] [(Expire, apply 2 [Unexhaust])]) 
                    , (Enemies, trapPer (-2) TrackDamage damage
                              • trap (-2) OnDamage § remove "Naraka Path"
                              • bomb (-2) [] [(Expire, apply 2 [Exhaust All])])
                    ]
        }
    ]
    , [ newSkill
        { label   = "Rinnegan"
        , desc    = "Whenever Nagato deals damage to an enemy, he can use this skill the following turn to damage an enemy for half his damage total from that turn. Whenever Nagato is healed, he can use this skill the following turn to heal himself or an ally for half the amount of health he regained during that turn."
        , classes = [Mental, Multi]
        , cd      = 2
        , changes = \n skill@Skill{..} →
            skill { effects = (hasOwn "Rinnegan" n 
                            ? ((Enemy, perI "Rinnegan" 1 damage 0):)) 
                            ∘ (hasOwn "Rinnegan Heal" n 
                            ? ((XAlly, perI "Rinnegan Heal" 1 heal 0):)) 
                            $ [] 
                  , require = if | hasOwn "Rinnegan"      n 
                                 ∨ hasOwn "Rinnegan Heal" n → Usable
                                 | otherwise                → Unusable
                  } 
        }
      ]
    , [ newSkill 
        { label   = "Preta Path"
        , desc    = "Nagato absorbs attacks against him, countering all enemy skills next turn. Each countered skill restores 10 health to Nagato and steals a random chakra from its user."
        , classes = [Mental, Ranged, Multi]
        , effects = [ (Enemy, steal 1)
                    , (Self,  heal 10 • apply 1 [] 
                            • ifStacks "Preta Path" 2 § vary 1 2 1) 
                    ]
        }
      ]
    ] 
    [ (PerDamaged, \n i → N.addOwnStacks 1 "Rinnegan"      3 0 n $ i ÷ 2)
    , (PerHealed,  \n i → N.addOwnStacks 1 "Rinnegan Heal" 3 0 n $ i ÷ 2)
    ]
  , Character
    "Hanzō"
    "Resurrected by Kabuto, Hanzō the Salamander was the leader of Amegakure. In combination with his unrivaled combat prowess, the lethal venom sac implanted in his body makes him a feared legend throughout the world."
    [ [ newSkill
        { label   = "Major Summoning: Ibuse"
        , desc    = "Hanzō summons his fabled salamander to the battlefield, which starts out with 30 health. All damage to Hanzō is split between him and Ibuse. While active, this skill becomes [Poison Fog][b][b]."
        , classes = [Chakra, Summon, Unreflectable, Unremovable, Multi]
        , cost    =  χ [Rand, Rand, Rand]
        , cd      = 6
        , effects = [ (Self, ifnotI "Venom Sac" 
                             § hide' "Ibuse" 0 [Ward Affliction 0.5] 
                             ° addStacks "Major Summoning: Ibuse" 30
                             ° vary 0 0 1
                             ° (trapPer' 0 PerDamaged
                                § removeStacks "Major Summoning: Ibuse")
                             ° trap' 0 (OnDamaged All)
                               (ifnotI "Major Summoning: Ibuse" 
                                § remove "Ibuse"
                                ° removeTrap "Ibuse"
                                ° vary 0 0 0
                                ° cancelChannel "Poison Fog")
                           • ifI "Venom Sac" § remove "Venom Sac" 
                                             ° alterCd 0 0 (-2))
                    , (Self, remove "Venom Sac")
                    ]
        }
      , newSkill
        { label   = "Poison Fog"
        , desc    = "Ibuse opens its mouth to reveal a noxious cloud of deadly poison, dealing 10 affliction damage to all enemies until Ibuse dies. Cannot be used while active."
        , classes = [Ranged, Single, Unreflectable]
        , cost    = χ [Blood, Blood]
        , channel = Ongoing 0
        , effects = [(Enemies, afflict 10)]
        }
      ]
    , [ newSkill
        { label   = "Sickle Dance"
        , desc    = "Hanzō slashes an enemy with his sickle, dealing 15 piercing damage to them immediately and 5 affliction damage for 2 turns. During [Major Summoning: Ibuse], Ibuse swallows the target, stunning their non-mental skills for 1 turn and dealing 10 additional affliction damage."
        , classes = [Bane, Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [(Enemy, pierce 15 • apply 2 [ Afflict 15]
                           • ifI "Major Summoning: Ibuse" 
                             § afflict 10 ° apply 1 [Stun All])]
        , changes = changeWith "Major Summoning: Ibuse" 
                  $ \_ skill → skill { skPic = True }
        }
      ]
    , [ newSkill
        { label   = "Venom Sac"
        , desc    = "The first enemy to use a non-mental skill on Hanzō next turn will rupture his implanted venom sac, taking 20 affliction damage every turn and causing Hanzō to take 10 affliction damage every turn. When Hanzō summons Ibuse or if [Major Summoning: Ibuse] is already active, Hanzō will replace his venom sac with Ibuse's, ending [Major Summoning: Ibuse], curing himself of this skill, and decreasing the current cooldown of [Major Summoning: Ibuse] by 3."
        , classes = [Bane, InvisibleTraps]
        , cost    = χ [Blood]
        , effects = [(Self, trapFrom (-1) (OnHarmed NonMental) 
                          $ apply 0 [Afflict 20]
                          • ifnotI "Ibuse" ∘ self § apply 0 [Afflict 20]
                          • ifI "Ibuse" ∘ self 
                            § remove "Major Summoning: Ibuse"
                            ° vary 0 0 0 ° alterCd 0 0 (-2)
                            ° cancelChannel "Poison Fog")]
        }
      ]
    , invuln "Block" "Hanzō" [Physical]
    ] []
  , Character
    "Ameyuri Ringo"
    "Resurrected by Kabuto, Ameyuri was one of the Seven Swordsmen of the Mist. Wielding Baki, the legendary twin lightning blades, Ameyuri cuts down her enemies using paralyzing electricity."
    [ [ newSkill
        { label   = "Lightning Fang"
        , desc    = "Ameyuri sends cascading bolts of lightning across the battlefield, applying 2 turns of Electricity to all enemies. Whenever someone affected by Electricity uses a skill, Electricity on them is refreshed to its maximum duration, and everyone affected by Electricity receives 5 affliction damage that bypasses invulnerability. Reapplying Electricity extends its duration instead of stacking."
        , classes = [Bane, Chakra, Ranged, Extending]
        , cost    = χ [Nin, Rand]
        , cd      = 4
        , effects = [(Enemies, apply' "Electricity" 2 []
                             • ifnotU "electrocuted"
                               § ( trap' 0 (OnAction All) 
                                 ∘ ifU "Electricity"
                                   $ refresh "Electricity" 
                                   • everyone § ifU "Electricity" (afflict 5)
                                 )
                             • hide' "electrocuted" 0 [])]
        }
      ] 
    , [ newSkill
        { label   = "Depth Charge"
        , desc    = "Ameyuri surrounds herself with lightning and electrocutes an opponent, dealing 30 damage. Deals affliction damage if the target is affected by Electricity. Next turn, enemies who use a skill on Ameyuri will have 1 turn of Electricity applied to them."
        , classes = [Bane, Chakra, Melee, Extending]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, ifU "Electricity"   § afflict 30
                            • ifnotU "Electriciy" § damage 30) 
                    , (Self, trapFrom 1 (OnHarmed All) 
                             $ apply' "Electricity" 1 []
                             • ifnotU "electrocuted"
                               § ( trap' 0 (OnAction All) 
                                 ∘ ifU "Electricity"
                                   $ refresh "Electricity" 
                                   • everyone § ifU "Electricity" (afflict 5)
                                 )
                             • hide' "electrocuted" 0 [])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Thunder Gate"
        , desc    = "With the twin blades of Kiba plunged into the ground, Ameyuri calls down lightning from the sky to incinerate the battlefield around an enemy, dealing 30 piercing damage to them. Deals 10 additional damage per enemy affected by Electricity. Removes 1 turn of Electricity from all enemies."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Nin]
        , cd      = 4
        , effects = [(Enemy, perAffected "Electricity" 10 pierce 30
                           • everyone § hasten 1 "Electricity")]
        }
      ]
    , invuln "Parry" "Ameyuri" [Physical]
    ] []
  , Character
    "Kushimaru Kuriarare"
    "Resurrected by Kabuto, Kushimaru was one of the Seven Swordsmen of the Mist. Wielding Nuibari, the legendary razor-wire longsword, Kushimaru stitches together his enemies to prevent them from acting."
    [ [ newSkill
        { label   = "Needle and Thread"
        , desc    = "Kushimaru skewers an enemy with Nuibari, dealing 20 piercing damage and marking them for 1 turn. During [Stitching Spider], this skill deals 5 additional damage and also targets all other enemies affected by [Stitching Spider]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy,    withI "Stitching Spider" 5 pierce 20 • tag 1) 
                    , (XEnemies, ifU "Stitching Spider" § pierce 25 ° tag 1)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Stitching Spider"
        , desc    = "Kushimaru lays a trap of wires on the ground. For 3 turns, enemies who use physical skills will receive 10 piercing damage and be marked until after this skill ends."
        , classes = [Physical, Ranged, InvisibleTraps]
        , cost    = χ [Tai, Rand]
        , cd      = 2
        , channel = Control 3
        , effects = [(Enemies, prolong 1 "Stitching Spider" 
                             • trap (-2) (OnAction Physical) §
                               ifI "Stitching Spider" (pierce 10 ° tag 1))]
        }
      ]
    , [ newSkill
        { label   = "Wire Crucifixion"
        , desc    = "Kushimaru stitches up all enemies affected by [Needle and Thread], dealing 15 piercing damage and ending their Action and Control skills in progress."
        , require = HasU "Needle and Thread"
        , classes = [Physical, Ranged]
        , cost    = χ [Tai]
        , effects = [(Enemies, pierce 15 • interrupt)]
        }
      ]
    , invuln "Parry" "Kushimaru" [Physical]
    ] []
  , Character
    "Jinpachi Munashi"
    "Resurrected by Kabuto, Jinpachi was one of the Seven Swordsmen of the Mist. Wielding Shibuki, the legendary explosive blade, Jinpachi builds up stockpiles of paper bombs that he can detonate simultaneously."
    [ [ newSkill
        { label   = "Blast Sword"
        , desc    = "Jinpachi swings his sword at an enemy, dealing 30 damage and making them immune to effects from allies for 1 turn. If Jinpachi does not have any Paper Bombs, he loses 15 health. Otherwise, he spends one Paper Bomb."
        , classes = [Chakra, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy, damage 30 • apply 1 [Seal])
                    , (Self,  ifnotI "Paper Bomb" § sacrifice 0 15 
                           •  removeStack "Paper Bomb")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Shibuki Bomb Reload"
        , desc    = "Adding it to his sword, Jinpachi gains 1 Paper Bomb. Each Paper Bomb provides 5 points of damage reduction."
        , classes = [Physical]
        , effects = [(Self, apply' "Paper Bomb" 0 [Reduce All 5])]
        }
      ]
    , [ newSkill
        { label   = "Multiple Explosions of Death"
        , desc    = "Jinpachi sets off a chain reaction of bombs around himself, dealing 40 damage to an enemy and 40 damage to a random enemy. Requires at least two Paper Bombs. If Jinpachi only has two Paper Bombs, he loses 30 health. Spends all Paper Bombs."
        , require = HasI 2 "Paper Bomb"
        , classes = [Chakra, Ranged]
        , cost    = χ [Tai, Rand]
        , cd      = 2
        , effects = [ (Self,  ifnotStacks "Paper Bomb" 3 § sacrifice 0 30
                              • remove "Paper Bomb") 
                    , (Enemy,  damage 40)
                    , (REnemy, damage 40)
                    ]
        }
      ]
    , invuln "Parry" "Jinpachi" [Physical]
    ]
    []
  , Character
    "Gengetsu Hōzuki"
    "Resurrected by Kabuto, Gengetsu was the second Mizukage of the Hidden Mist Village. Charismatic and carefree, he cheerfully offers tips to his opponents on how to beat him. He is especially fond of one-on-one duels."
    [ [ newSkill
        { label   = "Major Summoning: Giant Clam"
        , desc    = "Gengetsu summons a huge clam that exudes illusory mist for 4 turns. Each turn, a random member of his team becomes a mirage, reflecting the first harmful skill used on them next turn, and a random member of his team gains 80 destructible defense for 1 turn. If the clam's destructible defense is destroyed, this skill is canceled."
        , classes = [Chakra, Summon]
        , cost    = χ [Nin, Gen, Rand]
        , channel = Ongoing 4
        , cd      = 5
        , effects = [ (RAlly, apply 1 [Reflect]) 
                    , (RAlly, defend 1 80 
                            • onBreak ∘ self 
                              § cancelChannel "Major Summoning: Giant Clam")]
        }
      ]
    , [ newSkill
        { label   = "Water Pistol"
        , desc    = "Gengetsu fires a droplet of water like a bullet at an enemy, dealing 10 piercing damage and killing them if their health drops to 10 or lower. Deals 10 additional damage and bypasses invulnerability during [Major Summoning: Giant Clam]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand]
        , effects = [(Enemy, withI "Major Summoning: Giant Clam" 10 pierce 10 
                           • ifHealthU 0 10 § kill)]
        , changes = changeWith "Major Summoning: Giant Clam" 
                  $ addClass Bypassing
        }
      ]
    , [ newSkill
        { label   = "Steaming Danger Tyranny Boy"
        , desc    = "Gengetsu isolates an enemy by repeatedly blasting the rest of their team back with a childlike figure of himself. For 2 turns, Gengetsu and his target are invulnerable to everyone else and cannot use skills on anyone else. At the start of the duel, both participants have their health set to 30. When the duel ends, they are restored to their health before the duel if still alive."
        , classes = [Chakra, Ranged, Bypassing, Unremovable]
        , cost    = χ [Nin, Rand]
        , cd      = 3
        , effects = [ (Enemy, perHealthU id (\hp → bomb 2 [Duel, Taunt] 
                             [(Done, setHealth hp)])
                            • setHealth 30
                            • self § perHealthU id (\hp → bomb 2 [Duel, Taunt] 
                             [(Done, setHealth hp)])
                            • self § setHealth 30)]
        }
      ]
    , invuln "Mirage" "Gengetsu" [Mental]
    ] []
  , Character
    "Rasa"
    "Resurrected by Kabuto, Rasa was the fourth Kazekage of the Hidden Sand Village and the father of the Sand Siblings. Cold and calculating, Rasa buries his enemies beneath crushingly heavy gold dust that they must fight their way out of to survive."
    [ [ newSkill
        { label   = "Magnet Technique"
        , desc    = "Rasa floods the enemy team with waves of gold, dealing 10 damage to them and applying 10 permanent destructible barrier to each. The skills of enemies who have destructible barrier from this skill cost an additional random chakra."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [(Enemies, damage 10 
                             • withU "Gold Dust Waterfall" 10 
                               (bar 0 wait' § apply 1 [Exhaust All]) 10)]
        }
      ]
    , [ newSkill
        { label   = "Gold Dust Waterfall"
        , desc    = "A towering tidal wave of gold slams down on an enemy, dealing 35 damage and applying 30 permanent destructible barrier. Next turn, [Gold Dust Wave] and [24-Karat Barricade] will apply twice as much destructible barrier to them."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin, Nin]
        , cd      = 2
        , effects = [(Enemy, damage 35 • bar 0 wait' wait 30 • tag 1)]
        }
      ]
    , [ newSkill
        { label   = "24-Karat Barricade"
        , desc    = "Rasa constructs a golden blockade in front of an enemy. If they use a harmful skill next turn, it will be countered and they will gain 20 permanent destructible barrier."
        , classes = [Physical, Ranged, Invisible]
        , cost    = χ [Nin]
        , cd      = 2
        , effects = [(Enemy, trap 1 (OnCounter All) 
                             § withU "Gold Dust Waterfall" 20 
                               (bar 0 wait' wait) 20)]
        }
      ]
    , invuln "Gold Dust Shield" "Rasa" [Physical]
    ] []
  , Character
    "Mū"
    "Resurrected by Kabuto, Mū was the second Tsuchikage of the Hidden Rock Village. Unfailingly polite, he intends to ensure that his village benefits from the war. By manipulating matter at the atomic level, he disintegrates the defenses of his enemies."
    [ [ newSkill
        { label   = "Particle Beam"
        , desc    = "Mū blasts an enemy with a ray of high-energy atomic particles, dealing 25 piercing damage. Deals 10 additional damage if the target is invulnerable. Deals 5 fewer damage and costs 1 ninjutsu chakra during [Fragmentation]."
        , classes = [Chakra, Ranged, Bypassing]
        , cost    = χ [Nin, Rand]
        , effects = [(Enemy, withInvulnU 10 
                            (withI "Fragmentation" (-5) damage) 25)]
        , changes = changeWith "Fragmentation" $ setCost [Nin]
        }
      ]
    , [ newSkill
        { label   = "Fragmentation"
        , desc    = "Mū's body undergoes fission and splits into two. For 2 turns, Mū ignores stuns and reduces damage against him by half. If Mū's health reaches 0 during this skill, he regains 15 health and this skill ends."
        , classes = [Chakra]
        , cost    = χ [Nin]
        , cd      = 4
        , effects = [(Self, apply 2 [Focus, Ward All 0.5] 
                          • trap 2 OnRes § setHealth 15 
                                         ° remove      "Fragmentation"
                                         ° removeTrap "Fragmentation")]
        }
      ]
    , [ newSkill
        { label   = "Atomic Dismantling"
        , desc    = "Mū shatters the atomic bonds within an enemy, dealing 40 piercing damage and demolishing their destructible defense and his destructible barrier. Deals 5 fewer damage and costs 1 ninjutsu chakra and 1 random chakra during [Fragmentation]."
        , classes = [Chakra, Ranged, Bypassing]
        , cost    = χ [Nin, Rand, Rand]
        , cd      = 1
        , effects = [(Enemy, demolish • withI "Fragmentation" (-5) pierce 40)]
        , changes = changeWith "Fragmentation" $ setCost [Nin, Rand]
        }
      ]
    , invuln "Dustless Bewildering Cover" "Mū" [Chakra]
    ] []{-
  , Character
    "A"
    "Resurrected by Kabuto, A was the third Raikage of the Hidden Cloud Village. Legendary for his resilience and fortitude, A "-}
  ]
