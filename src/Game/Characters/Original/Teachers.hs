{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Teachers (teacherCs) where

import StandardLibrary
import Game.Functions
import Game.Game
import Game.Structure

teacherCs :: [Character]
teacherCs =
  [ Character
    "Iruka Umino"
     "A chūnin from the Hidden Leaf Village, Iruka is a kind instructor who has mentored Naruto throughout his youth. He fights to keep his allies safe and protect them from harm, growing stronger out of desperation the closer to death he is."
    [ [ newSkill
        { label   = "Shuriken Throw"
        , desc    = "Iruka throws a shuriken at an enemy, dealing 20 damage plus 10 per 25 health Iruka has lost."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai]
        , effects = [(Enemy, perHealthI (lost 25 10) damage 20)]
        }
      ]
    , [ newSkill
        { label   = "Ally Shield"
        , desc    = "Using himself as a shield, Iruka makes an ally invulnerable for 1 turn."
        , classes = [Physical]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(XAlly, apply 1 [Immune All])]
        }
      ]
    , [ newSkill
        { label   = "Capture and Arrest"
        , desc    = "Iruka traps an enemy in an area filled with his paper bombs. If the target uses a new harmful skill during their next turn, they will receive 40 damage, and physical and chakra skills will deal 25 additional damage to them for 1 turn."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin]
        , cd      = 2
        , effects = [(Enemy, trap 1 OnHarm 
                             § damage 40 
                             ° apply 1 [Bleed Physical 25, Bleed Chakra 25])]
        }
      ]
    , invuln "Parry" "Iruka" [Physical]
    ] []
  , Character
    "Mizuki"
    "A chūnin from the Hidden Leaf Village, Mizuki is an unpleasant instructor who betrays his allies without hesitation in order to succeed. Unless forced into direct combat, he slips into the shadows and ambushes his enemies at their weakest."
    [ [ newSkill
        { label   = "Kunai Assault"
        , desc    = "Mizuki throws a series of kunai at an enemy, dealing 15 damage for 2 turns. Deals all 30 damage instantly during [Successful Ambush]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , channel = Action 2
        , effects = [(Enemy, damage 15)]
        }
      , newSkill
        { label   = "Kunai Assault"
        , desc    = "Mizuki throws a series of kunai at an enemy, dealing 15 damage for 2 turns. Deals all 30 damage instantly during [Successful Ambush]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, damage 30)]
        }
      ]
    , [ newSkill
        { label   = "Execution Shuriken"
        , desc    = "Mizuki throws one of his two giant shurikens at an enemy, dealing 10 damage plus 10 per 20 health the target has lost. Deals 30 additional damage during [Successful Ambush]."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai, Rand]
        , charges = 2
        , effects = [ (Enemy, perHealthU (lost 20 10) damage 10
                            • withI "Genjutsu Ambush Tactics" 30 damage 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Genjutsu Ambush Tactics"
        , desc    = "Mizuki lurks in the shadows. If he does not take any new damage next turn, he becomes invulnerable for 1 turn and his other skills are empowered."
        , classes = [Mental, InvisibleTraps]
        , cost    = χ [Gen]
        , cd      = 1
        , effects = [(Self, trap (-1) (OnDamaged All) 
                            § remove "Ambush Preparation"
                          • bombWith' [Hidden] "Ambush Preparation" (-1) [] 
                            [(Expire, self 
                                      § vary' 1 "Kunai Assault" "Kunai Assault" 
                                      ° apply (-1) [Immune All])])]
        }
      ]
    , invuln "Dodge" "Mizuki" [Physical]
    ] []
  , Character
    "Kakashi Hatake"
    "Team 7's jōnin squad leader, Kakashi puts the life of his teammates above all else. Known as the Copy Ninja, Kakashi analyzes and duplicates abilities used against him."
    [ [ newSkill
        { label   = "Sharingan"
        , desc    = "Kakashi anticipates any attacks against him, copying them faster than their user. All harmful skills that target Kakashi next turn will be reflected."
        , classes = [Chakra, Melee, Invisible]
        , cost    = χ [Nin, Tai]
        , cd      = 1
        , effects = [(Self, apply 1 [ReflectAll])]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Ninja Hounds"
        , desc    = "A pack of giant dogs bite at an enemy, stunning the target's non-mental skills for 1 turn."
        , classes = [Physical, Ranged, Summon]
        , cost    = χ [Nin, Rand]
        , cd      = 2
        , effects = [(Enemy, apply 1 [Stun NonMental])]
        }
      ]
    , [ newSkill
        { label   = "Lightning Blade"
        , desc    = "Kakashi devastates an enemy with a blast of lightning chakra, dealing 50 piercing damage. Instantly kills targets affected by [Summoning: Ninja Hounds]."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Tai]
        , cd      = 1
        , effects = [(Enemy, ifU    "Summoning: Ninja Hounds" kill
                           • ifnotU "Summoning: Ninja Hounds" § pierce 50)]
        }
      ]
    , invuln "Hide" "Kakashi" [Mental]
    ] []
  , Character
    "Anko Mitarashi"
    "A former student of Orochimaru who bears his Curse Mark, Anko is now a jōnin teacher in the Hidden Leaf Village. She uses various poisons and forbidden techniques learned from Orochimaru to dismantle her enemies."
    [ [ newSkill
        { label   = "Dual Pin"
        , desc    = "Anko pins herself to an enemy by stabbing a kunai through her hand, dealing 5 damage and preventing the target from reducing damage or becoming invulnerable for 1 turn."
        , classes = [Physical, Melee]
        , channel = Control 1
        , effects = [ (Enemy, damage 5 • apply 1 [Expose])
                    , (Self,  vary "Dragon Flame" "Twin Snake Sacrifice"
                            • tag' "Twin Snake Sacrifice" 1)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Dragon Flame"
        , desc    = "Fire scorches the battlefield, dealing 10 affliction damage to all enemies for 2 turns. All of Anko's damage is increased by 5 against targets affected by [Dragon Flame]. During [Dual Pin], this skill becomes [Twin Snake Sacrifice][n][n]."
        , classes = [Bane, Ranged]
        , cost    = χ [Nin]
        , cd      = 2
        , effects = [(Enemies, apply 2 [Link 5, Afflict 10])]
        }
      , newSkill
        { label   = "Twin Snake Sacrifice"
        , desc    = "Kills Anko and the target of [Dual Pin]."
        , require = HasU "Dual Pin"
        , classes = [Melee]
        , cost    = χ [Nin, Nin]
        , effects = [ (Enemies, kill)
                    , (Self,    kill')
                    ]
        }
      ]
    , [ newSkill
        { label   = "Striking Shadow Snakes"
        , desc    = "Numerous snakes attack an enemy, dealing 20 damage instantly and 5 affliction damage each turn for 3 turns."
        , classes = [Physical, Melee]
        , cost    = χ [Nin, Rand]
        , effects = [(Enemy, damage 20 • apply 3 [Afflict 5])]
        }
      ]
    , invuln "Dodge" "Anko" [Physical]
    ] []
  , Character
    "Hayate Gekkō"
    "A jōnin exam proctor from the Hidden Leaf Village, Hayate is calm and composed despite his poor health and chronic cough. He slips in and out of the shadows, gradually recovering his strength and honing his expert swordsmanship."
    [ [ newSkill
        { label   = "Secret Sword"
        , desc    = "Hayate leaps and attacks an enemy from above, dealing 15 damage. Permanently increases Hayate's damage by 5."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy, damage 15)
                    , (Self,  apply 0 [Strengthen All 5])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Crescent Moon Dance"
        , desc    = "Performing a genjutsu-aided triple sword strike, Hayate deals 30 damage to an enemy. Catching his opponents off guard, he also counters the first harmful skill used on him in the next turn."
        , classes = [Mental, Melee]
        , cost    = χ [Gen, Rand]
        , cd      = 1
        , effects = [ (Enemy, damage 30)
                    , (Self,  apply 1 [Counter All])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Transparency Technique"
        , desc    = "Hayate melds into shadows, increasing his damage by 10 for 3 turns. Each turn, he gains 1 turn of damage reduction: 25 points on the first turn, 15 points on the second, and 5 points on the third."
        , classes = [Mental, Unremovable]
        , cost    = χ [Gen]
        , cd      = 3
        , effects = [(Self, apply 3 [Strengthen All 10] 
                           • apply 1 [Reduce All 25]
                           • delay (-1) § apply 1 [Reduce All 15]
                           • delay (-2) § apply 1 [Reduce All 5])]
        }
      ]
    , invuln "Dodge" "Hayate" [Physical]
    ] []
  , Character
    "Kurenai Yuhi"
    "Team 8's jōnin squad leader, Kurenai is caring and brave. A master of genjutsu, Kurenai traps her enemies in inescapable illusions."
    [ [ newSkill
        { label   = "Demonic Illusion: Entrap"
        , desc    = "Kurenai hinders an enemy with her genjutsu, dealing 10 damage. For 2 turns, the target's non-affliction damage is weakened by 10, the chakra costs of their skills is increased by 1 random chakra, and they cannot reduce damage or become invulnerable. Adds 5 destructible defense to Kurenai's next [Illusory Tree Meld]."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen]
        , cd      = 2
        , effects = [ (Enemy, damage 10 • apply 2  
                              [Weaken All 10, Exhaust All, Expose])
                    , (Self,  addStacks "Illusion" 1)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Illusory Tree Meld"
        , desc    = "Kurenai melds into an illusory tree, gaining 10 permanent destructible defense."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(Self, perI "Illusion" 5 (defend 0) 10 
                          • remove "Illusion")]
        }
      ]
    , [ newSkill
        { label   = "Demonic Illusion: Sylvan Fetters"
        , desc    = "Kurenai traps an enemy in a powerful genjutsu, stunning them for 2 turns. Adds 5 destructible defense to Kurenai's next [Illusory Tree Meld] each turn. While active, this skill becomes [Sylvan Fetters Attack][r]."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Gen]
        , cd      = 4
        , channel = Control 2
        , start   = [(Self, vary "Demonic Illusion: Sylvan Fetters" 
                                 "Sylvan Fetters Attack")]
        , effects = [ (Enemy, apply 1 [Stun All])
                    , (Self,  addStacks "Illusion" 1)
                    ]
        }
      , newSkill
        { label   = "Sylvan Fetters Attack"
        , desc    = "Deals 30 piercing damage to the target of [Demonic Illusion: Sylvan Fetters]."
        , require = HasU "Demonic Illusion: Sylvan Fetters"
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , effects = [(Enemy, pierce 30)]
        }
      ]
    , invuln "Vanish" "Kurenai" [Mental]
    ] []
  , Character
    "Asuma Sarutobi"
    "Team 10's squad leader, Asuma is a chronic smoker and the third Hokage's son. He focuses on protecting his team, providing them with defense and taking blows in their stead."
    [ [ newSkill
        { label   = "Flying Swallow"
        , desc    = "Asuma carries out a series of chakra-enhanced knife attacks. For 2 turns, he deals 15 damage to all enemies and provides 15 points of damage reduction to his team. While active, this skill becomes [Finishing Blow][n][r] and [Sharpen Blades] becomes [Flying Kick][t][r]."
        , classes = [Physical, Melee, Uncounterable, Unreflectable]
        , cost    = χ [Nin, Tai]
        , cd      = 2
        , channel = Action 2
        , start   = [(Self, vary "Flying Swallow" "Finishing Blow" 
                          • vary "Sharpen Blades" "Flying Kick")]
        , effects = [ (Enemies, damage 15)
                    , (Allies,  apply 1 [Reduce All 15])
                    , (Self,    remove "Sharpen Blades")
                    ]
        , changes = \n skill@Skill{..} -> skill
              { channel = Action 
                        $ getDur channel + numActive "Sharpen Blades" n
              }
        }
      , newSkill
        { label   = "Finishing Blow"
        , desc    = "Deals 35 damage to an enemy and stuns them for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [(Enemy, damage 35 • apply 1 [Stun All])]
        }
      ]
    , [ newSkill
        { label   = "Sharpen Blades"
        , desc    = "Asuma sharpens his blades, increasing the duration of his next [Flying Swallow] by 1 additional turn."
        , classes = [Physical]
        , cost    = χ [Rand]
        , effects = [(Self, addStack)]
        }
      , newSkill
        { label   = "Flying Kick"
        , desc    = "Deals 35 damage to an enemy."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, damage 35)]
        }
      ]
    , [ newSkill
        { label   = "Self-Sacrifice"
        , desc    = "Asuma continually protects one ally. All harmful skills used on the target will be reflected to Asuma. This skill can be used again with no chakra cost to cancel its effect."
        , classes = [Physical, Melee, Soulbound, Unreflectable]
        , cost    = χ [Rand]
        , effects = [ (XAlly, apply 0 [Redirect All])
                    , (Self,  vary "Self-Sacrifice" "Self-Sacrifice")
                    ]
        }
      , newSkill
        { label   = "Self-Sacrifice"
        , desc    = "Ends the effect of [Self-Sacrifice]."
        , classes = [Physical, Melee, Unreflectable]
        , varicd  = True
        , effects = [(Self, vary "Self-Sacrifice" "" 
                          • everyone § remove "Self-Sacrifice")]
        }
      ]
    , invuln "Parry" "Asuma" [Physical]
    ] []
  , Character
    "Might Guy"
    "Team Guy's jōnin squad leader, Guy is passionate and strong-willed. He treats his teammates like family, especially Lee, who looks up to him as a father figure. His taijutsu prowess is unmatched. Although opening his inner Gates takes a heavy toll on his body, it empowers his blows with inescapable destruction."
    [ [ newSkill
        { label   = "Leaf Hurricane"
        , desc    = "Guy delivers a powerful spinning kick to an enemy, dealing 30 damage. Deals 30 additional damage and bypasses invulnerability during [Sixth Gate Opening]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, withI "Sixth Gate Opening" 30 damage 30)]
        , changes = changeWith "Sixth Gate Opening" § addClass Bypassing
        }
      ]
    , [ newSkill
        { label   = "Sixth Gate Opening"
        , desc    = "Guy loses 40 health down to a minimum of 1 and becomes invulnerable for 2 turns."
        , classes = [Mental]
        , cost    = χ [Tai]
        , cd      = 4
        , effects = [(Self, apply 2 [Immune All] • sacrifice 1 40)]
        }
      ]
    , [ newSkill
        { label   = "Counter Punch"
        , desc    = "Guy singles out an enemy. If the target uses a harmful skill next turn, they will be countered and receive 30 damage."
        , classes = [Physical, Melee, Invisible]
        , cost    = χ [Tai]
        , cd      = 2
        , effects = [(Enemy, trap 1 (OnCounter All) § damage 30)]
        }
      ]
    , invuln "Dodge" "Guy" [Physical]
    ] []
  , Character
    "Baki"
    "A jōnin from the Hidden Sand Village, Baki is a ruthlessly efficient squad leader. He cloaks his attacks in illusions to prevent his enemies from defending against them."
    [ [ newSkill
        { label   = "Sudden Strike"
        , desc    = "Baki performs a blindingly fast attack concealed by decoy illusory trails, dealing 20 piercing damage to an enemy."
        , classes = [Physical, Melee]
        , cost    = χ [Gen]
        , effects = [(Enemy, pierce 20)]
        }
      ]
    , [ newSkill
        { label   = "Wind Sword"
        , desc    = "A maelstrom of stabbing wind deals 40 piercing damage to an enemy."
        , classes = [Physical, Ranged]
        , cost    = χ [Gen, Nin]
        , effects = [(Enemy, pierce 40)]
        }
      ]
    , [ newSkill
        { label   = "Flak Jacket"
        , desc    = "Using a top-of-the-line Hidden Sand Village flak jacket, Baki provides 50 destructible defense to himself or an ally. While the target has destructible defense from this skill, they ignore harmful non-damage effects other than chakra cost changes."
        , classes = [Physical]
        , cost    = χ [Rand, Rand]
        , cd      = 6
        , effects = [(Ally, defend 4 50 • apply 4 [Enrage] • onBreak')]
        }
      ]
    , invuln "Teleport" "Baki" [Chakra]
    ] []
  , Character
    "Shizune"
    "A jōnin from the Hidden Leaf Village, Shizune is a talented medical-nin apprenticed to Tsunade. She is agile and fast, fully capable of holding her own in a fight whenever she isn't healing one of her allies."
    [ [ newSkill
        { label   = "Poison Needle Volley"
        , desc    = "Shizune shoots hidden needles at an enemy, dealing 15 damage."
        , classes = [Bane, Physical, Ranged]
        , cost    = χ [Rand]
        , effects = [(Enemy, damage 15)]
        }
      ]
    , [ newSkill
        { label   = "Poison Fog"
        , desc    = "Shizune spews forth a toxic cloud to poison an enemy, causing them to receive 10 affliction damage for 3 turns. Cannot be used on an enemy already affected by [Poison Fog]."
        , classes = [Bane, Ranged, Single]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, apply 3 [Afflict 10])]
        }
      ]
    , [ newSkill
        { label   = "Regenerative Healing Technique"
        , desc    = "Using a notoriously difficult healing technique, Shizune restores 35 health to herself or an ally and cures them of enemy effects."
        , classes = [Chakra]
        , cost    = χ [Rand, Rand]
        , cd      = 1
        , effects = [(XAlly, cureAll • heal 35)]
        }
      ]
    , invuln "Dodge" "Shizune" [Physical]
    ] []
  ]
