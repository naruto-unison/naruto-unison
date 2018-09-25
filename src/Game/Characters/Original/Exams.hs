{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Exams (examCs) where

import StandardLibrary
import Game.Functions
import Game.Game
import Game.Structure

examCs :: [Character]
examCs =
  [ Character
    "Hanabi Hyūga"
    "The younger sister of Hinata, Hanabi trains endlessly to prove herself. What she lacks in strength, she makes up for with speed and tenacity. Through sheer force of willpower, she endures attacks that would kill anyone else."
    [ [ newSkill
        { label   = "Gentle Fist"
        , desc    = "With a series of blows, Hanabi deals 15 damage to an enemy for 2 turns. Each time they use a skill that removes or steals chakra, they will lose a random chakra."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , channel = Action 2
        , effects = [(Enemy, damage 15 • trap 1 OnChakra § drain 1)]
        }
      ] 
    , [ newSkill
        { label   = "Eight Trigrams Palm Rotation"
        , desc    = "Hanabi spins at an enemy, dealing 15 damage to them for 2 turns. If they use a skill that stuns, they will be stunned for 1 turn. Costs 1 random chakra during [Unyielding Tenacity]."
        , classes = [Chakra, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , channel = Action 2
        , effects = [(Enemy, damage 15 • trap 1 OnStun § apply 1 [Stun All])]
        , changes = changeWith "Unyielding Tenacity" $ setCost [Rand]
        }
      ]
    , [ newSkill
        { label   = "Unyielding Tenacity"
        , desc    = "Gritting her teeth, Hanabi fights to the bitter end. For 2 turns, her health cannot drop below 1, she ignores stuns, and her damage is increased by 5."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [(Self, apply 2 [Endure, Focus, Strengthen All 5])]
        }
      ]
    , invuln "Byakugan Foresight" "Hanabi" [Mental]
    ] []
  , Character
    "Shigure"
    "A genin from the Hidden Rain Village, Shigure is strong but arrogant. He wields up to four needle-filled umbrellas at a time, choosing whether to use them on defense, wide offense, or focused damage."
    [ [ newSkill
        { label   = "Umbrella Toss"
        , desc    = "Shigure tosses his umbrellas upward, gaining four Umbrellas. While Shigure has Umbrellas, this skill becomes [Umbrella Gathering]."
        , classes = [Physical]
        , effects = [(Self, addStacks "Umbrella" 4 • vary 0 0 1)]
        }
      , newSkill
        { label   = "Umbrella Gathering"
        , desc    = "Shigure builds a wall of overlapping umbrellas in front of him. Spends all Umbrellas to provide 10 points of damage reduction per Umbrella for 1 turn."
        , classes = [Physical]
        , effects = [(Self, perI "Umbrella" 10 (applyX 1 § Reduce All) 0
                          • remove "Umbrella" • vary 0 0 0)]
        }
      ]
    , [ newSkill
        { label   = "Senbon Shower"
        , desc    = "Shigure's umbrellas rain down needles, dealing 15 damage to all enemies. Uses up one Umbrella."
        , require = HasI 1 "Umbrella"
        , classes = [Physical, Ranged, Uncounterable, Unreflectable]
        , cost    = χ [Rand]
        , effects = [ (Enemies, damage 15)
                    , (Self,    removeStack "Umbrella" 
                              • ifnotI "Umbrella" § vary 0 0 0)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Senbon Barrage"
        , desc    = "Shigure hurls all of his needles at a single target, dealing 15 damage per Umbrella. Costs 1 fewer random chakra per Umbrella. Uses up all Umbrellas."
        , require = HasI 1 "Umbrella"
        , cost    = χ [Rand, Rand, Rand, Rand]
        , classes = [Physical, Ranged]
        , effects = [ (Enemy, perI "Umbrella" 15 damage 0)
                    , (Self,  remove "Umbrella" • vary 0 0 0)
                    ]
        , changes = \n skill@Skill{..} -> skill 
              { cost = cost - 0{ rand = numActive "Umbrella" n } }
        }
      ]
    , invuln "Umbrella Shield" "Shigure" [Physical]
    ] []
  , Character
    "Kabuto Yakushi"
    "Orochimaru's close assistant and confidant, Kabuto is a brilliant and enigmatic genin. He uses his medical expertise to weaken and expose his opponents."
    [ [ newSkill
        { label   = "Chakra Scalpel"
        , desc    = "Kabuto slices an enemy with a medical scalpel made of chakra, dealing 20 piercing damage. For 1 turn, the target's skills cost an additional random chakra and they receive 5 additional damage from physical and chakra damaging skills."
        , classes = [Bane, Chakra, Melee]
        , cost    = χ [Nin]
        , effects = [(Enemy, pierce 20 • apply 1 [ Exhaust All
                                                 , Bleed Physical 5
                                                 , Bleed Chakra 5
                                                 ])]
        }
      ]
    , [ newSkill
        { label   = "Pre-Healing Technique"
        , desc    = "Kabuto accelerates his cell growth in anticipation of attacks, restoring 15 health for 5 turns and curing himself of baneful effects."
        , classes = [Chakra]
        , cost    = χ [Nin]
        , cd      = 5
        , channel = Ongoing 5
        , start   = [(Self, cureBane)]
        , effects = [(Self, heal 15)]
        }
      ]
    , [ newSkill
        { label   = "Temple of Nirvana"
        , desc    = "Illusory white feathers descend upon the battlefield and lull the enemy team to sleep. Unless an enemy uses a skill next turn, they will be stunned for 1 turn and receive 10 additional damage from physical and chakra skills."
        , classes = [Mental, Ranged, Invisible]
        , cost    = χ [Gen]
        , cd      = 2
        , effects = [(Enemies, trap (-1) (OnAction All) 
                               § remove "Temple of Nirvana Trap" 
                             • bomb' "Temple of Nirvana Trap" (-1) []
                               [(Expire, apply 1 [ Stun All
                                                 , Bleed Physical 10
                                                 , Bleed Chakra 10 
                                                 ])])]
        }
      ]
    , invuln "Dead Soul Technique" "Kabuto" [Chakra]
    ] []
  , Character
    "Dosu Kinuta"
    "One of the three sound genin, Dosu is patient and logical. His sound-projecting gauntlet shatters his opponents' hearing, making them more vulnerable to attacks."
    [ [ newSkill
        { label   = "Resonating Echo Drill"
        , desc    = "Dosu attacks an enemy with his drill, dealing 20 damage and preventing them from reducing damage or becoming invulnerable for 2 turns. Deals 20 additional damage during [Echo Speaker Tuning]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [(Enemy, withI "Echo Speaker Tuning" 20 damage 20
                           • apply 2 [Expose] • tag' "Echoing Sound" 1)]
        }
      ]
    , [ newSkill
        { label   = "Sound Manipulation"
        , desc    = "Dosu bombards an enemy with sound waves, dealing 10 damage. The target permanently receives 5 additional damage from non-affliction skills and their damage is weakened by 5. Deals 10 additional damage if [Resonating Echo Drill] was used on the target last turn. Deals 10 additional damage during [Echo Speaker Tuning]."
        , classes = [Mental, Ranged, Multi]
        , cost    = χ [Gen]
        , cd      = 1
        , effects = [(Enemy, withI "Echo Speaker Tuning" 10
                            (withU "Echoing Sound" 10 damage) 10
                           • apply 0 [Bleed NonAffliction 5, Weaken All 5])]
        }
      ]
    , [ newSkill
        { label   = "Echo Speaker Tuning"
        , desc    = "Dosu fine-tunes his Echo Speaker to produce debilitating sound vibrations, empowering his other skills for 4 turns."
        , classes = [Physical]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [(Self, tag 4)]
        }
      ]
    , invuln "Dodge" "Dosu" [Physical]
    ] []
  , Character
    "Kin Tsuchi"
    "One of the three sound genin, Kin likes to defeat her enemies slowly and painfully, torturing them with her needles and bells. Her moves empower each other when strung together in disorienting harmony."
    [ [ newSkill
        { label   = "Bell Ring Illusion"
        , desc    = "Sanity-shattering soundwaves deal 15 damage to an enemy. If [Shadow Senbon] was used last turn, Kin becomes invulnerable for 1 turn. If [Unnerving Bells] was used last turn, deals 25 additional damage."
        , classes = [Mental, Ranged]
        , cost    = χ [Rand]
        , effects = [(Self,  ifI "Shadow Senbon" § apply 1 [ Immune All]
                            • tag 1)
                    , (Enemy, withI "Unnerving Bells" 25 damage 15)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Shadow Senbon"
        , desc    = "Kin traps an enemy with her surgical needles, preventing them from reducing damage or becoming invulnerable for 2 turns. If [Bell Ring Illusion] was used last turn, deals 10 damage to the target. If [Unnerving Bells] was used last turn, the target is stunned for 1 turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [ (Self,  tag 1)
                    , (Enemy, apply 2 [Expose]
                            • ifI "Bell Ring Illusion" § damage 10
                            • ifI "Unnerving Bells" § apply 1 [Stun All])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Unnerving Bells"
        , desc    = "A cacophony of bell tones saps the strength of an enemy, removing 1 chakra. If [Bell Ring Illusion] was used last turn, the target takes 15 more damage from chakra skills for 1 turn. If [Shadow Senbon] was used last turn, the target takes 15 more damage from physical skills for 1 turn."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen]
        , cd      = 2
        , effects = [ (Self,  tag 1)
                    , (Enemy, drain 1
                            • ifI "Bell Ring Illusion" 
                              § apply 1 [Bleed Chakra 15]
                            • ifI "Shadow Senbon"
                              § apply 1 [Bleed Physical 15])
                    ]
        }
      ]
    , invuln "Foresight" "Kin" [Mental]
    ] []
  , Character
    "Zaku Abumi"
    "One of the three sound genin, Zaku has a strong desire to win and succeed. The tubes implanted in his arm let him create powerful waves of chakra and protect his allies with air currents."
    [ [ newSkill
        { label   = "Slicing Sound Wave"
        , desc    = "Zaku fires a blast of supersonic air at an enemy, dealing 25 damage and enabling the use of [Supersonic Slicing Wave] for 1 turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood]
        , effects = [ (Enemy, damage 25)
                    , (Self,  tag 1)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Wall of Air"
        , desc    = "An invisible barrier of air shields an ally, countering the first harmful non-mental skill used on them next turn."
        , classes = [Physical, Invisible, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(XAlly, apply 1 [Counter NonMental])]
        }
      ]
    , [ newSkill
        { label   = "Supersonic Slicing Wave"
        , desc    = "Zaku boosts his airwaves to terrifying volume, dealing 45 damage to all enemies. Requires [Slicing Sound Wave]."
        , require = HasI 1 "Slicing Sound Wave"
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Rand, Rand]
        , effects = [(Enemies, damage 45)]
        }
      ]
    , invuln "Deflect" "Zaku" [Chakra]
    ] []
  , Character
    "Oboro"
    "A genin from the Hidden Rain Village, Oboro is a vindictive specialist in genjutsu. He conceals himself within a crowd of illusory clones, making it almost impossible for enemies to locate and harm him."
    [ [ newSkill
        { label   = "Exploding Kunai"
        , desc    = "Oboro throws a kunai attached to a paper bomb, dealing 15 damage to all enemies. Costs 1 random chakra during [Fog Clone]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand, Rand]
        , effects = [(Enemies, damage 15)]
        , changes = changeWith "Fog Clone" $ setCost [Rand]
        }
      ]
    , [ newSkill
        { label   = "Underground Move"
        , desc    = "Emerging from the ground behind an enemy, Oboro deals 20 damage  to them and stuns their physical and mental skills for 1 turn. Targets all enemies and costs 1 genjutsu chakra during [Fog Clone]."
        , classes = [Physical, Melee]
        , cost    = χ [Gen, Rand]
        , effects = [(Enemy, damage 20 • apply 1 [Stun Physical, Stun Mental])]
        , changes = changeWith "Fog Clone" $ setCost [Gen] •• targetAll
        }
      ]
    , [ newSkill
        { label   = "Fog Clone"
        , desc    = "Oboro surrounds himself with illusions, providing him with 30 destructible defense and invulnerability to physical and mental skills for 3 turns."
        , classes = [Mental]
        , cost    = χ [Gen, Rand]
        , cd      = 4
        , effects = [(Self, defend 3 30 
                          • apply 3 [Immune Mental, Immune Physical])]
        }
      ]
    , invuln "Hide" "Oboro" [Mental]
    ] []
  , Character
    "Yoroi Akadō"
    "A genin from the fake Hidden Sound Village, Yoroi was a resident of the Hidden Leaf Village before defecting to Orochimaru. Capable but brash, Yoroi likes to toy with his enemies, gradually stealing their strength and health for himself."
    [ [ newSkill
        { label   = "Energy Drain"
        , desc    = "Yoroi touches an enemy, stealing 20 health. For 2 turns, their damage is weakened by 5 and Yoroi's damage is increased by 5."
        , classes = [Chakra, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [ (Enemy, ifI "Chakra Focus" § steal 1
                            • apply 2 [ Weaken All 5] • leech 20 § self . heal)
                    , (Self, apply 2 [Strengthen All 5])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Draining Assault"
        , desc    = "Yoroi charges at an enemy and repeatedly drains their energy, stealing 15 health from them for 3 turns. While active, their damage is weakened by 5 and Yoroi's damage is increased by 5."
        , classes = [Chakra, Melee]
        , cost    = χ [Tai, Rand, Rand]
        , cd      = 3
        , channel = Action 3
        , effects = [ (Enemy, ifI "Chakra Focus" § steal 1 
                            • apply 1 [Weaken All 5] • leech 15 § self . heal)
                    , (Self,  apply 1 [Strengthen All 5])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Chakra Focus"
        , desc    = "Yoroi infuses his attacks with a field of chakra. For 2 turns, using a skill causes it to steal 1 chakra each turn of its action duration."
        , classes = [Chakra]
        , cost    = χ [Rand]
        , cd      = 3
        , effects = [(Self, tag 2)]
        }
      ]
    , invuln "Deflect" "Yoroi" [Chakra]
    ] []
  , Character
    "Misumi Tsurugi"
    "A genin from the fake Hidden Sound Village, Misumi was a resident of the Hidden Leaf Village before defecting to Orochimaru. He twists his muscles and bones to wrap around targets, shielding his allies or choking his opponents."
    [ [ newSkill
        { label   = "Flexible Twisting Joints"
        , desc    = "Misumi latches on to a target with his startlingly loose joints, providing 15 points of damage reduction for 1 turn to an ally or 15 damage to an enemy."
        , classes = [Physical, Melee, Bypassing]
        , cost    = χ [Rand]
        , effects = [ (XAlly, apply 1 [Reduce All 15])
                    , (Enemy, damage 15)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Soft Physique Modification"
        , desc    = "Misumi wraps around an enemy. For 2 turns, all harmful non-mental skills used on Misumi are reflected to that enemy. During this time, the target cannot reduce damage or become invulnerable."
        , classes = [Physical, Melee, Soulbound, Uncounterable, Unreflectable]
        , cost    = χ [Rand, Rand]
        , cd      = 4
        , effects = [(Enemy, apply 2 [Expose] 
                           • self § hide 2 [Redirect NonMental])]
        }
      ]
    , [ newSkill
        { label   = "Tighten Joints"
        , desc    = "By stiffening his joints, Misums gains 15 non-stacking permanent destructible defense. If an enemy is affected by [Soft Physique Modification], they take 20 damage and are stunned for 1 turn."
        , classes = [Physical, Melee, Nonstacking]
        , cost    = χ [Rand]
        , effects = [ (Self, defend 0 15)
                    , (Enemies, ifU "Soft Physique Modification" 
                                § damage 20 ° apply 1 [Stun All])
                    ]
        }
      ]
    , invuln "Block" "Misumi" [Physical]
    ] []
  ]
