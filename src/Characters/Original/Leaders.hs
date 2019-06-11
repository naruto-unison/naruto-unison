{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Characters.Original.Leaders (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Jiraiya"
    "One of three legendary sannin, Jiraiya is a lecherous frog hermit who travels the world in search of knowledge. His toad summoning techniques and fire manipulation wreak destruction upon enemy teams."
    [ [ Skill.new
        { Skill.name      = "Giant Flame Bomb"
        , Skill.desc      = "Jiraiya spits out a burst of fire, dealing 20 affliction damage to an enemy and 10 to the rest of their team."
        , Skill.classes   = [Ranged]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Enemy    $ damage 20
          , p XEnemies $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Toad Mouth Trap"
        , Skill.desc      = "The esophagus of a humongous toad swallows the battlefield for 2 turns. Within the esophagus, enemies cannot reduce damage or become invulnerable, and each member of Jiraiya's team becomes invulnerable for 1 turn if they are damaged by a new non-affliction skill."
        , Skill.classes   = [Chakra, Ranged, Summon, Bypassing]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 3
        , Skill.channel   = Ongoing 2
        , Skill.start     =
          [ p Allies $
                trap 2 (OnDamaged NonAffliction) $ apply 1 [Invulnerable All]
          ]
        , Skill.effects   =
          [ p Enemies $ apply 1 [Expose] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Gamabunta"
        , Skill.desc      = "Jiraiya summons the great toad Gamabunta, who deals 25 affliction damage to all enemies. Once used, this skill permanently becomes [Toad Oil Bomb][n][r]. All enemies permanently receive 5 additional damage from affliction skills."
        , Skill.classes   = [Ranged, Summon, Unreflectable, Unremovable]
        , Skill.cost      = k [Blood, Gen, Tai]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Enemies do
                afflict 25
                apply 0 [Bleed Affliction Flat 5]
          , p Self $ vary "Major Summoning: Gamabunta" "Toad Oil Bomb"
          ]
        }
      , Skill.new
        { Skill.name      = "Toad Oil Bomb"
        , Skill.desc      = "Deals 15 affliction to all enemies for 2 turns."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies $ apply 2 [Afflict 15] ]
        }
      ]
    , [ invuln "Needle Jizou" "Jiraiya" [Physical] ]
    ] []
  , Character
    "Tsunade"
    "One of three legendary sannin, Tsunade is a gambler and sake enthusiast with a short temper and terrible luck. Her abilities require large amounts of chakra, but their healing power is unrivaled."
    [ [ Skill.new
        { Skill.name      = "Heavenly Kick of Pain"
        , Skill.desc      = "Tsunade performs an axe kick on an enemy, dealing 30 damage and stunning their physical and mental skills for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 30
                apply 1 [Stun Physical, Stun Mental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mitotic Regeneration"
        , Skill.desc      = "Tsunade unleashes the ultimate regeneration technique upon herself, regaining all lost health and curing herself of enemy effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self do
                cureAll
                setHealth 100
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Katsuyu"
        , Skill.desc      = "Tsunade summons the great slug Katsuyu, who heals her team for 40 health. Once used, this skill permanently becomes [Slug Division][n]. Each turn that Tsunade is alive, her team regains 5 health."
        , Skill.classes   = [Chakra, Summon, Unremovable, Unreflectable]
        , Skill.cost      = k [Blood, Gen, Nin]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Allies $ heal 40
          , p Self   $ vary "Major Summoning: Katsuyu" "Slug Division"
          ]
        , Skill.effects   =
          [ p Allies $ heal 5 ]
        }
      , Skill.new
        { Skill.name      = "Slug Division"
        , Skill.desc      = "Provides 15 permanent destructible defense to Tsunade's team."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Allies $ defend 0 15 ]
        }
      ]
    , [ invuln "Block" "Tsunade" [Physical] ]
    ] []
  , Character
    "Orochimaru"
    "One of three legendary sannin, Orochimaru intends to learn every technique in existence. Driven by an insatiable hunger for power, he employs various methods to increase his chakra, even if he has to harm his allies in the process."
    [ [ Skill.new
        { Skill.name      = "Kusanagi"
        , Skill.desc      = "Striking swiftly, Orochimaru demolishes an enemy's destructible defense and his own destructible barrier before dealing 25 piercing damage to the target."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                demolishAll
                pierce 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Curse Mark"
        , Skill.desc      = "Orochimaru places a curse mark on an ally or enemy, dealing 15 affliction damage and granting the target 1 random chakra."
        , Skill.classes   = [Melee, Bypassing]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy do
                afflict 15
                gain [Rand]
          , p XAlly do
                sacrifice 0 15
                gain [Rand]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Manda"
        , Skill.desc      = "Orochimaru summons the great serpent Manda, who deals 45 damage to an enemy. Once used, this skill permanently becomes [Paralyzing Bite][r][r]. Each turn that Orochimaru is alive, he gains 1 random chakra."
        , Skill.classes   = [Physical, Melee, Summon, Unreflectable]
        , Skill.cost      = k [Blood, Nin, Tai]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Enemy $ damage 45
          , p Self  $ vary "Major Summoning: Manda" "Paralyzing Bite"
          ]
        , Skill.effects   =
          [ p Self $ gain [Rand] ]
        }
      , Skill.new
        { Skill.name      = "Paralyzing Bite"
        , Skill.desc      = "Stuns an enemy for 1 turn and deals 25 damage when the effect ends."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ bomb 1 [Stun All] [ p Expire $ damage 25 ] ]
        }
      ]
    , [ invuln "Earth Clone" "Orochimaru" [Chakra] ]
    ] []
  , Character
    "Hiruzen Sarutobi"
    "Called the Supreme Shinobi, Hiruzen is the third Hokage. During his prime, he was considered the strongest ninja in history. His trump card ability brings certain doom to both Hiruzen and his enemy."
    [ [ Skill.new
        { Skill.name      = "Dragon Flame Bomb"
        , Skill.desc      = "Hiruzen engulfs an enemy in flame, dealing 30 affliction damage and causing them to receive 10 additional damage from affliction skills for 1 turn."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemy do
                afflict 30
                apply 1 [Bleed Affliction Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Reaper Death Seal"
        , Skill.desc      = "Hiruzen unleashes the God of Death upon an enemy in exchange for his soul. Each turn, the God of Death deals 40 affliction damage to the target and 20 affliction damage to Hiruzen. Both are permanently stunned, and the target cannot reduce damage or become invulnerable. Hiruzen is permanently stunned. Ends when Hiruzen dies."
        , Skill.classes   = [Ranged, Unreflectable, Unremovable, Soulbound, Bypassing]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemy $ apply 0 [Afflict 40, Stun All, Expose]
          , p Self  $ apply 0 [Afflict 20, Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Enma"
        , Skill.desc      = "Hiruzen summons the great monkey Enma, who makes his team invulnerable for 1 turn. Once used, this skill permanently becomes [Adamantine Prison][n]. As long as Hiruzen is alive, he provides 5 points of damage reduction to his team."
        , Skill.classes   = [Physical, Melee, Summon, Unreflectable, Unremovable]
        , Skill.cost      = k [Gen, Nin, Tai]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Allies do
                apply 1 [Invulnerable All, Endure]
                apply 0 [Reduce All Flat 5 ]
          , p Self $ vary "Major Summoning: Enma" "Adamantine Prison"
          ]
        }
      , Skill.new
        { Skill.name      = "Adamantine Prison"
        , Skill.desc      = "Provides 15 permanent destructible defense to Hiruzen's team and prevents their health from dropping below 1 for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Allies do
                defend 0 15
                apply 1 [Endure]
          ]
        }
      ]
    , [ invuln "Mud Wall" "Hiruzen" [Physical] ]
    ] []
  ]

