{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Original.Leaders (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Orochimaru"
    "One of three legendary sannin, Orochimaru intends to learn every technique in existence. Driven by an insatiable hunger for power, he gathers chakra any way he can, even if it means harming his allies."
    [LeafVillage, Orochimaru, Anbu, Sannin, Rogue, TeamLeader, Wind, Lightning, Earth, Water, Fire, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Kusanagi"
        , Skill.desc      = "Striking swiftly, Orochimaru demolishes an enemy's destructible defense and his own destructible barrier, then deals 25 piercing damage to the target."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                pierce 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Curse Mark"
        , Skill.desc      = "Orochimaru places a curse mark on an ally or enemy, dealing 15 affliction damage and granting the target 1 random chakra."
        , Skill.classes   = [Bane, Melee, Bypassing]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                gain [Rand]
                afflict 15
          , To XAlly do
                gain [Rand]
                sacrifice 0 15
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Manda"
        , Skill.desc      = "Orochimaru summons the great serpent Manda, who deals 45 damage to an enemy. Once used, this skill becomes [Paralyzing Bite][r][r]. Every turn that Orochimaru is alive, he gains 1 random chakra."
        , Skill.classes   = [Summon, Melee, Unreflectable]
        , Skill.cost      = [Blood, Nin, Tai]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Enemy $ damage 45 ]
        , Skill.effects   =
          [ To Self do
                gain [Rand]
                hide 1 [Alternate "Major Summoning: Manda" "Paralyzing Bite"]
          ]
        }
      , Skill.new
        { Skill.name      = "Paralyzing Bite"
        , Skill.desc      = "Stuns an enemy for 1 turn and deals 25 damage when the effect ends."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ bomb 1 [Stun All] [ To Expire $ damage 25 ] ]
        }
      ]
    , [ invuln "Earth Clone" "Orochimaru" [Chakra] ]
    ]
  , Character
    "Jiraiya"
    "One of three legendary sannin, Jiraiya is a lecherous frog hermit who travels the world in search of knowledge. His toad summoning techniques and fire manipulation wreak destruction upon enemy teams."
    [LeafVillage, Sannin, Sage, TeamLeader, Fire, Wind, Earth, Water, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Giant Flame Bomb"
        , Skill.desc      = "Jiraiya spits out a burst of fire that deals 20 affliction damage to an enemy and 10 to the rest of their team."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy $ afflict 20
          , To XEnemies $ afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Toad Mouth Trap"
        , Skill.desc      = "The esophagus of a humongous toad swallows the battlefield for 2 turns. Within the esophagus, enemies cannot reduce damage or become invulnerable. While active, if an enemy uses a skill that deals non-affliction damage to Jiraiya or his allies, their target will become invulnerable for 1 turn."
        , Skill.classes   = [Summon, Ranged, Bypassing]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 3
        , Skill.dur       = Ongoing 2
        , Skill.effects   =
          [ To Enemies $ apply 1 [Expose]
          , To Allies $
                trap 1 (OnDamaged NonAffliction) $ apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Gamabunta"
        , Skill.desc      = "Jiraiya summons the great toad Gamabunta, who deals 25 affliction damage to all enemies. Once used, this skill becomes [Toad Oil Bomb][n][r]. Every turn that Jiraiya is alive, Gamabunta causes all enemies to receive 5 additional damage from affliction skills."
        , Skill.classes   = [Bane, Summon, Ranged, Unreflectable, Unremovable]
        , Skill.cost      = [Blood, Gen, Tai]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Enemies $ afflict 25 ]
        , Skill.effects   =
          [ To Enemies $ apply 1 [Bleed [Affliction] Flat 5]
          , To Self $
                hide 1 [Alternate "Major Summoning: Gamabunta" "Toad Oil Bomb"]
          ]
        }
      , Skill.new
        { Skill.name      = "Toad Oil Bomb"
        , Skill.desc      = "Gamabunta douses the enemy team in dense oil and Jiraiya ignites it. The resulting blaze deals 15 affliction damage to all enemies for 2 turns."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ apply 2 [Afflict 15] ]
        }
      ]
    , [ invuln "Needle Jizou" "Jiraiya" [Physical] ]
    ]
  , Character
    "Tsunade"
    "One of three legendary sannin, Tsunade is a gambler and sake enthusiast with a short temper and terrible luck. Her abilities require large amounts of chakra, but their healing power is unrivaled."
    [LeafVillage, Sannin, Lightning, Earth, Water, Fire, Yin, Yang, Senju]
    [ [ Skill.new
        { Skill.name      = "Heavenly Kick of Pain"
        , Skill.desc      = "Tsunade performs an axe kick on an enemy, dealing 30 damage and stunning their physical and mental skills for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 30
                apply 1 [Stun Physical, Stun Mental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mitotic Regeneration"
        , Skill.desc      = "Tsunade unleashes the ultimate regeneration technique upon herself, regaining all lost health and curing herself of enemy effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                cureAll
                setHealth 100
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Katsuyu"
        , Skill.desc      = "Tsunade summons the great slug Katsuyu, who heals her team for 35 health. Once used, this skill becomes [Slug Division][n]. Every turn that Tsunade is alive, Katsuyu restores 5 health to her team."
        , Skill.classes   = [Summon, Unremovable, Unreflectable]
        , Skill.cost      = [Blood, Gen, Nin]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Allies $ heal 40 ]
        , Skill.effects   =
          [ To Allies $ heal 5
          , To Self $
                hide 1 [Alternate "Major Summoning: Katsuyu" "Slug Division"]
          ]
        }
      , Skill.new
        { Skill.name      = "Slug Division"
        , Skill.desc      = "Katsuyu splits into many smaller slugs that restore 10 health to Tsunade's team and provide them with 10 permanent destructible defense."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Allies do
                heal 10
                defend 0 10
          ]
        }
      ]
    , [ invuln "Block" "Tsunade" [Physical] ]
    ]
  , Character
    "Hiruzen Sarutobi"
    "Called the Supreme Shinobi, Hiruzen is the third Hokage. During his prime, he was considered the strongest ninja in history. His trump card ability brings certain doom to both Hiruzen and his enemy."
    [LeafVillage, Kage, Sensor, Fire, Wind, Lightning, Earth, Water, Yin, Yang, Sarutobi]
    [ [ Skill.new
        { Skill.name      = "Dragon Flame Bomb"
        , Skill.desc      = "Hiruzen engulfs an enemy in flame, dealing 30 affliction damage and causing them to receive 10 additional damage from affliction skills for 1 turn."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                afflict 30
                apply 1 [Bleed [Affliction] Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Reaper Death Seal"
        , Skill.desc      = "Hiruzen unleashes the God of Death upon an enemy in exchange for his soul. Each turn, the God of Death deals 40 affliction damage to the target and 20 affliction damage to Hiruzen. Both are permanently stunned, and the target cannot reduce damage or become invulnerable."
        , Skill.classes   = [Ranged, Unreflectable, Unremovable, Soulbound, Bypassing]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ apply 0 [Afflict 40, Stun All, Expose]
          , To Self $ apply 0 [Afflict 20, Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Enma"
        , Skill.desc      = "Hiruzen summons the great monkey Enma, who makes his team invulnerable for 1 turn. Once used, this skill becomes [Adamantine Prison][n]. Every turn that Hiruzen is alive, Enma deals 5 damage to all enemies and provides 5 points of damage reduction to Hiruzen's team."
        , Skill.classes   = [Summon, Melee, Unreflectable, Unremovable]
        , Skill.cost      = [Gen, Nin, Tai]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Allies $ apply 1 [Invulnerable All] ]
        , Skill.effects   =
          [ To Enemies $ damage 5
          , To Allies $ apply 1 [Reduce [All] Flat 5]
          , To Self $
                hide 1 [Alternate "Major Summoning: Enma" "Adamantine Prison"]
          ]
        }
      , Skill.new
        { Skill.name      = "Adamantine Prison"
        , Skill.desc      = "Enma clones and transforms into a diamond-hard cage around Hiruzen's team. For 1 turn, their health is prevented from dropping below 1, and all damage they receive—including piercing and affliction—is reduced by 20."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Allies $ apply 1 [Endure, Reduce [Affliction] Flat 20] ]
        }
      ]
    , [ invuln "Mud Wall" "Hiruzen" [Physical] ]
    ]
  ]

