{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Original.Exams (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Category -> Text -> Character]
characters =
  [ Character
    "Hanabi Hyūga" 0
    "The younger sister of Hinata, Hanabi trains endlessly to prove herself. What she lacks in strength, she makes up for with speed and tenacity. Through sheer force of willpower, she endures attacks that would kill anyone else."
    [LeafVillage, Genin, Hyuga]
    [ [ Skill.new
        { Skill.name      = "Gentle Fist"
        , Skill.desc      = "With a relentless series of blows, Hanabi deals 15 damage to an enemy for 2 turns. Every time the target uses a skill that gains, depletes, or absorbs chakra, their team will be depleted of 1 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                damage 15
                trap 1 OnChakra $
                    deplete 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Palm Rotation"
        , Skill.desc      = "Hanabi spins at an enemy, dealing 15 damage to them for 2 turns. If they use a skill that stuns or disables, they will be stunned for 1 turn. Costs [r] during [Unyielding Tenacity]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                damage 15
                trap 1 OnStun $
                    apply 1 skillName [Stun All]
          ]
        , Skill.changes   = changeWith "Unyielding Tenacity" $ setCost [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Unyielding Tenacity"
        , Skill.desc      = "Gritting her teeth, Hanabi fights to the bitter end. For 2 turns, her health cannot drop below 1, she ignores stuns and disabling effects, and her damage is increased by 5."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 2 skillName
                [ Endure
                , Focus
                , Strengthen [All] Flat 5
                ]
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Hanabi" [Mental] ]
    ]
  , Character
    "Shigure" 0
    "A genin from the Hidden Rain Village, Shigure is strong but arrogant. He wields up to four needle-filled umbrellas at a time, choosing whether to use them on defense, widespread attacks, or focused damage."
    [RainVillage, Genin, TeamLeader]
    [ [ Skill.new
        { Skill.name      = "Umbrella Toss"
        , Skill.desc      = "Shigure tosses his umbrellas upward, gaining four Umbrellas. While Shigure has Umbrellas, this skill becomes [Umbrella Gathering]."
        , Skill.classes   = [Physical, Resource]
        , Skill.effects   =
          [ To Self $ applyStacks "Umbrella" 4
                [ Alternate "Umbrella Toss"
                            "Umbrella Gathering"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Umbrella Gathering"
        , Skill.desc      = "Shigure builds a wall of overlapping umbrellas in front of him. Spends all Umbrellas to provide 10 points of damage reduction per Umbrella for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ To Self do
                stacks <- user numStacks "Umbrella"
                apply 1 skillName [Reduce [All] Flat (stacks * 10)]
                remove "Umbrella"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Senbon Shower"
        , Skill.desc      = "Shigure's umbrellas rain down needles, dealing 15 damage to all enemies. Uses up one Umbrella."
        , Skill.require   = UserHas 1 "Umbrella"
        , Skill.classes   = [Physical, Ranged, Uncounterable, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemies $ damage 15
          , To Self $ removeStack "Umbrella"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Senbon Barrage"
        , Skill.desc      = "Shigure hurls all of his needles at a single target, dealing 15 damage per Umbrella. Costs 1 fewer arbitrary chakra per Umbrella. Uses up all Umbrellas."
        , Skill.require   = UserHas 1 "Umbrella"
        , Skill.cost      = [Rand, Rand, Rand, Rand]
        , Skill.classes   = [Physical, Ranged]
        , Skill.effects   =
          [ To Enemy do
                stacks <- user numStacks "Umbrella"
                damage (15 * stacks)
          , To Self $ remove "Umbrella"
          ]
        , Skill.changes   = changePer "Umbrella" \i ->
                                setCost $ replicate (4 - i) Rand
        }
      ]
    , [ invuln "Umbrella Shield" "Shigure" [Physical] ]
    ]
  , Character
    "Oboro" 0
    "A genin from the Hidden Rain Village, Oboro is a vindictive specialist in genjutsu. He conceals himself within a crowd of illusory clones, making it almost impossible for enemies to locate and harm him."
    [RainVillage, Genin, TeamLeader, Earth, Yin, Water]
    [ [ Skill.new
        { Skill.name      = "Exploding Kunai"
        , Skill.desc      = "Oboro throws a kunai attached to a paper bomb, dealing 15 damage to all enemies. Costs [r] during [Fog Clone]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemies $ damage 15 ]
        , Skill.changes   = changeWith "Fog Clone" $ setCost [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Underground Move"
        , Skill.desc      = "Emerging from the ground behind an enemy, Oboro deals 20 damage to them and stuns their physical and mental skills for 1 turn. Targets all enemies and costs [g] during [Fog Clone]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 skillName
                    [ Stun Physical
                    , Stun Mental
                    ]
          ]
        , Skill.changes   = changeWith "Fog Clone" $ targetAll . setCost [Gen]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fog Clone"
        , Skill.desc      = "Oboro surrounds himself with illusions, gaining 30 destructible defense and invulnerability to physical, mental, and summon skills for 3 turns."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                defend 3 30
                apply 3 skillName
                    [ Invulnerable Mental
                    , Invulnerable Physical
                    , Invulnerable Summon
                    ]
          ]
        }
      ]
    , [ invuln "Hide" "Oboro" [Mental] ]
    ]
  , Character
    "Kabuto Yakushi" 0
    "A genin and former operative of the Hidden Leaf Village's elite Root division, Kabuto is Orochimaru's close assistant and confidant. He hides his brilliance behind a genial façade, but under the surface he remains a cold and calculating spy. In combat, he uses his medical expertise to weaken and expose his opponents."
    [LeafVillage, Orochimaru, Anbu, Genin, Rogue, Sage, TeamLeader, Earth, Water, Wind, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Chakra Scalpel"
        , Skill.desc      = "Kabuto slices an enemy with a medical scalpel made of chakra, dealing 20 piercing damage. For 1 turn, the target's skills cost 1 additional arbitrary chakra and they receive 5 additional damage from physical, chakra, and summon damaging skills."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                pierce 20
                apply 1 skillName
                    [ Exhaust [All]
                    , Bleed [Physical, Chakra, Summon] Flat 5
                    ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Pre-Healing Technique"
        , Skill.desc      = "Kabuto accelerates his cell growth in anticipation of attacks, restoring 15 health for 5 turns and curing himself of bane effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                cureBane
                apply 5 skillName [Heal 15]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Temple of Nirvana"
        , Skill.desc      = "Illusory white feathers descend upon the battlefield and lull the enemy team to sleep. Unless an enemy uses a skill next turn, they will be stunned for 1 turn, during which they will receive 10 additional damage from physical, chakra, and summon skills."
        , Skill.classes   = [Mental, Ranged, Invisible]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies $ trap -1 OnNoAction $
                apply 1 skillName
                    [ Stun All
                    , Bleed [Physical, Chakra, Summon] Flat 10
                    ]
          ]
        }
      ]
    , [ invuln "Dead Soul Technique" "Kabuto" [Chakra] ]
    ]
  , Character
    "Dosu Kinuta" 0
    "One of the three sound genin, Dosu is patient and logical. His sound-projecting gauntlet shatters his opponents' hearing, making them more vulnerable to attacks."
    [SoundVillage, Orochimaru, Genin, TeamLeader]
    [ [ Skill.new
        { Skill.name      = "Resonating Echo Drill"
        , Skill.desc      = "Dosu attacks an enemy with his drill, preventing them from reducing damage or becoming invulnerable for 2 turns and dealing 20 damage. Deals 20 additional damage during [Echo Speaker Tuning]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                apply 2 skillName [Expose]
                bonus <- 20 `bonusIf` user has "Echo Speaker Tuning"
                damage (20 + bonus)
                tag 1 "Echoing Sound"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sound Manipulation"
        , Skill.desc      = "Sound waves bombard an enemy, dealing 10 damage. The target permanently receives 5 additional damage from non-affliction attacks and their damage is weakened by 5. Deals 10 additional damage if [Resonating Echo Drill] was used on the target last turn. Deals 10 additional damage during [Echo Speaker Tuning]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                targetBonus <- 10 `bonusIf` target has "Echoing Sound"
                userBonus   <- 10 `bonusIf` user has "Echo Speaker Tuning"
                damage (10 + targetBonus + userBonus)
                apply Permanent skillName
                    [ Bleed [NonAffliction] Flat 5
                    , Weaken [All] Flat 5
                    ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Echo Speaker Tuning"
        , Skill.desc      = "Dosu fine-tunes his Echo Speaker to produce debilitating sound vibrations, empowering his other skills for 4 turns."
        , Skill.classes   = [Physical, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ tag 4 skillName ]
        }
      ]
    , [ invuln "Dodge" "Dosu" [Physical] ]
    ]
  , Character
    "Kin Tsuchi" 0
    "One of the three sound genin, Kin likes to defeat her enemies slowly and painfully, torturing them with her needles and bells. Her moves empower each other when strung together in disorienting harmony."
    [SoundVillage, Orochimaru, Genin, Yin]
    [ [ Skill.new
        { Skill.name      = "Bell Ring Illusion"
        , Skill.desc      = "Sanity-shattering soundwaves deal 15 damage to an enemy. Deals 25 additional damage if [Unnerving Bells] was used last turn. If [Shadow Senbon] was used last turn, Kin becomes invulnerable for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                senbon <- user has "Shadow Senbon"
                if senbon then
                    apply 1 skillName [Invulnerable All]
                else
                    tag 1 skillName
          , To Enemy do
                bonus <- 25 `bonusIf` user has "Unnerving Bells"
                damage (15 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Senbon"
        , Skill.desc      = "Kin traps an enemy with her surgical needles, preventing them from reducing damage or becoming invulnerable for 2 turns. If [Bell Ring Illusion] was used last turn, deals 10 damage to the target. If [Unnerving Bells] was used last turn, the target is stunned for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ tag 1 skillName
          ,  To Enemy do
                apply 2 skillName [Expose]
                whenM (user has "Bell Ring Illusion") $
                    damage 10
                whenM (user has "Unnerving Bells") $
                    apply 1 skillName [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Unnerving Bells"
        , Skill.desc      = "A cacophony of bell tones saps the strength of an enemy, depleting 1 random chakra. If [Bell Ring Illusion] was used last turn, the target receives 15 more damage from chakra skills for 1 turn. If [Shadow Senbon] was used last turn, the target receives 15 more damage from physical skills for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ tag 1 skillName
          , To Enemy do
                deplete 1
                whenM (user has "Bell Ring Illusion") $
                    apply 1 skillName [Bleed [Chakra] Flat 15]
                whenM (user has "Shadow Senbon") $
                    apply 1 skillName [Bleed [Physical] Flat 15]
          ]
        }
      ]
    , [ invuln "Foresight" "Kin" [Mental] ]
    ]
  , Character
    "Zaku Abumi" 0
    "One of the three sound genin, Zaku has a strong desire to win and succeed. The tubes implanted in his arm let him create powerful waves of chakra and protect his allies with air currents."
    [SoundVillage, Orochimaru, Genin]
    [ [ Skill.new
        { Skill.name      = "Slicing Sound Wave"
        , Skill.desc      = "Zaku fires a blast of supersonic air at an enemy, dealing 25 damage and enabling the use of [Supersonic Slicing Wave] for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy $ damage 25
          , To Self $ tag 1 skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wall of Air"
        , Skill.desc      = "An invisible barrier of air shields an ally, countering the first non-mental skill an enemy uses on them next turn."
        , Skill.classes   = [Physical, Invisible, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To XAlly $ trap 1 (Counter NonMental) (return ()) ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Supersonic Slicing Wave"
        , Skill.desc      = "Zaku boosts his airwaves to terrifying volume, dealing 45 damage to all enemies. Requires [Slicing Sound Wave]."
        , Skill.require   = UserHas 1 "Slicing Sound Wave"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand, Rand]
        , Skill.effects   =
          [ To Enemies $ damage 45 ]
        }
      ]
    , [ invuln "Deflect" "Zaku" [Chakra] ]
    ]
  , Character
    "Yoroi Akadō" 0
    "A genin from the fake Hidden Sound Village, Yoroi was a resident of the Hidden Leaf Village before defecting to Orochimaru. Capable but brash, Yoroi likes to toy with his enemies, gradually stealing their strength and health for himself."
    [SoundVillage, Orochimaru, Genin, Rogue, Water, Yin]
    [ [ Skill.new
        { Skill.name      = "Energy Drain"
        , Skill.desc      = "Yoroi makes physical contact with an enemy and steals 20 health from them. For 2 turns, their damage is weakened by 5 and Yoroi's damage is increased by 5."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                whenM (user has "Chakra Focus") $
                    absorb 1
                apply 2 skillName [Weaken [All] Flat 5]
                leech 20 heal
          , To Self $ apply 2 skillName [Strengthen [All] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Draining Assault"
        , Skill.desc      = "Yoroi charges at an enemy and repeatedly drains their energy, stealing 15 health from them for 3 turns. While active, their damage is weakened by 5 and Yoroi's damage is increased by 5."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Tai, Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemy do
                whenM (user has "Chakra Focus") $
                    absorb 1
                apply 1 skillName [Weaken [All] Flat 5]
                leech 15 heal
          , To Self $ apply 1 skillName [Strengthen [All] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Focus"
        , Skill.desc      = "Yoroi infuses his attacks with a field of chakra. For 2 turns, using a skill causes it to absorb 1 random chakra from the enemy team each turn of its action duration."
        , Skill.classes   = [Chakra, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self $ tag 2 skillName ]
        }
      ]
    , [ invuln "Deflect" "Yoroi" [Chakra] ]
    ]
  , Character
    "Misumi Tsurugi" 0
    "A genin from the fake Hidden Sound Village, Misumi was a resident of the Hidden Leaf Village before defecting to Orochimaru. He twists his muscles and bones to wrap around targets, shielding his allies or choking his opponents."
    [SoundVillage, Orochimaru, Genin, Rogue, Water]
    [ [ Skill.new
        { Skill.name      = "Flexible Twisting Joints"
        , Skill.desc      = "Misumi latches on to a target with his startlingly loose joints, providing 15 points of damage reduction for 1 turn to an ally or 15 damage to an enemy."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To XAlly $ apply 1 skillName [Reduce [All] Flat 15]
          , To Enemy $ damage 15
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Soft Physique Modification"
        , Skill.desc      = "Misumi wraps around an enemy. For 2 turns, skills that enemies use on Misumi are reflected onto the target. During this time, the target cannot reduce damage or become invulnerable."
        , Skill.classes   = [Physical, Melee, Soulbound, Unremovable, Uncounterable, Unreflectable, Atemporal]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                apply 2 skillName [Expose]
                targetSlot <- target slot
                targeting Self $ apply 2 skillName [Redirect targetSlot]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tighten Joints"
        , Skill.desc      = "By stiffening his joints, Misumi gains 15 non-stacking permanent destructible defense. If an enemy is affected by [Soft Physique Modification], they take 20 damage and are stunned for 1 turn."
        , Skill.require   = TargetHas 1 "Soft Physique Modification"
        , Skill.classes   = [Physical, Melee, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ defend Permanent 15
          , To Enemies do
                damage 20
                apply 1 skillName [Stun All]
          ]
        }
      ]
    , [ invuln "Block" "Misumi" [Physical] ]
    ]
  ]
