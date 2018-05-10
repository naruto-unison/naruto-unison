{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Leaders (leaderCs) where

import Game.Functions
import Game.Game
import Game.Structure

leaderCs ∷ [Character]
leaderCs =
  [ Character
    "Jiraiya"
    "One of three legendary sannin, Jiraiya is a lecherous frog hermit who travels the world in search of knowledge. His toad summoning techniques and fire manipulation wreak destruction upon enemy teams."
    [ [ newSkill
        { label   = "Giant Flame Bomb"
        , desc    = "Jiraiya spits out a burst of fire, dealing 20 affliction damage to an enemy and 10 to the rest of their team."
        , classes = [Ranged]
        , cost    = χ [Nin]
        , effects = [ (Enemy,    damage 20)
                    , (XEnemies, damage 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Toad Mouth Trap"
        , desc    = "Jiraiya summons the esophagus of a humongous toad around the battlefield for 2 turns. Within the esophagus, enemies cannot reduce damage or become invulnerable, and each member of Jiraiya's team becomes invulnerable for 1 turn if they are damaged by a new non-affliction skill."
        , classes = [Chakra, Ranged, Summon]
        , cost    = χ [Gen]
        , cd      = 3
        , channel = Ongoing 2
        , start   = [(Allies, trap 2 (OnDamaged NonAffliction) 
                              § apply 1 [Immune All])]
        , effects = [(Enemies, apply 1 [Expose])]
        }
      ]
    , [ newSkill
        { label   = "Major Summoning: Gamabunta"
        , desc    = "Jiraiya summons the great toad Gamabunta, who deals 25 affliction damage to all enemies. Once used, this skill permanently becomes [Toad Oil Bomb][n][r]. All enemies permanently receive 5 additional damage from affliction skills."
        , classes = [Ranged, Summon, Unreflectable, Unremovable]
        , cost    = χ [Blood, Gen, Tai]
        , channel = Ongoing 0
        , start   = [ (Enemies, afflict 25 • apply 0 [Bleed Affliction 5])
                    , (Self,    vary 0 2 1)
                    ]
        }
      , newSkill
        { label   = "Toad Oil Bomb"
        , desc    = "Deals 15 affliction to all enemies for 2 turns."
        , classes = [Bane, Chakra, Ranged]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [(Enemies, apply 2 [Afflict 15])]
        }
      ]
    , invuln "Needle Jizou" "Jiraiya" [Physical]
    ] []
  , Character
    "Tsunade"
    "One of three legendary sannin, Tsunade is a gambler and sake enthusiast with a short temper and terrible luck. Her abilities require large amounts of chakra, but their healing power is unrivaled."
    [ [ newSkill
        { label   = "Heavenly Kick of Pain"
        , desc    = "Tsunade performs an axe kick on an enemy, dealing 30 damage and stunning their physical and mental skills for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, damage 30 
                           • apply 1 [Stun Physical, Stun Mental])]
        }
      ]
    , [ newSkill
        { label   = "Mitotic Regeneration"
        , desc    = "Tsunade unleashes the ultimate regeneration technique upon herself, regaining all lost health and curing herself of enemy effects."
        , classes = [Chakra]
        , cost    = χ [Nin, Nin]
        , cd      = 3
        , effects = [(Self, cureAll • setHealth 100)]
        }
      ]
    , [ newSkill
        { label   = "Major Summoning: Katsuyu"
        , desc    = "Tsunade summons the great slug Katsuyu, who heals her team for 40 health. Once used, this skill permanently becomes [Slug Division][n]. Each turn that Tsunade is alive, her team regains 5 health."
        , classes = [Chakra, Summon, Unremovable, Unreflectable]
        , cost    = χ [Blood, Gen, Nin]
        , channel = Ongoing 0
        , start   = [ (Allies, heal 40)
                    , (Self,   vary 0 2 1)
                    ]
        , effects = [(Allies, heal 5)]
        }
      , newSkill
        { label   = "Slug Division"
        , desc    = "Provides 15 permanent destructible defense to Tsunade's team."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin]
        , cd      = 2
        , effects = [(Allies, defend 0 15)]
        }
      ]
    , invuln "Block" "Tsunade" [Physical]
    ] []
  , Character
    "Orochimaru"
    "One of three legendary sannin, Orochimaru intends to learn every technique in existence. Driven by an insatiable hunger for power, he employs various methods to increase his chakra, even if he has to harm his allies in the process."
    [ [ newSkill
        { label   = "Kusanagi"
        , desc    = "Orochimaru strikes swiftly, demolishing an enemy's destructible defense and his destructible barrier, then dealing 25 piercing damage to the target."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, demolish • pierce 25)]
        }
      ]
    , [ newSkill
        { label   = "Curse Mark"
        , desc    = "Orochimaru places a curse mark on an ally or enemy, bdealing 15 affliction damage and granting the target 1 chakra."
        , classes = [Melee, Bypassing]
        , cd      = 3
        , effects = [ (Enemy, afflict 15 • gain [Rand])
                    , (XAlly, sacrifice 0 15 • gain [Rand])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Major Summoning: Manda"
        , desc    = "Orochimaru summons the great serpent Manda, who deals 45 damage to an enemy. Once used, this skill permanently becomes [Paralyzing Bite][r][r]. Each turn that Orochimaru is alive, he gains a random chakra."
        , classes = [Physical, Melee, Summon, Unreflectable]
        , cost    = χ [Blood, Nin, Tai]
        , channel = Ongoing 0
        , start   = [ (Enemy, damage 45)
                    , (Self, vary 0 2 1)
                    ]
        , effects = [(Self, gain [Rand])]
        }
      , newSkill
        { label   = "Paralyzing Bite"
        , desc    = "Stuns an enemy for 1 turn and deals 25 damage when the effect ends."
        , classes = [Bane, Physical, Melee]
        , cost    = χ [Rand, Rand]
        , cd      = 1
        , effects = [(Enemy, bomb 1 [Stun All] [(Expire, damage 25)])]
        }
      ]
    , invuln "Earth Clone" "Orochimaru" [Chakra]
    ] []
  , Character
    "Hiruzen Sarutobi"
    "Called the God of Shinobi, Hashirama is the third Hokage. During his prime, he was considered the strongest ninja in history. His trump card ability brings certain doom to both Hiruzen and his enemy."
    [ [ newSkill
        { label   = "Dragon Flame Bomb"
        , desc    = "Hiruzen engulfs an enemy in flame, dealing 30 affliction damage and causing them to receive 10 additional damage from affliction skills for 1 turn."
        , classes = [Bane, Ranged]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, afflict 30
                            • apply 1 [Bleed Affliction 10])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Reaper Death Seal"
        , desc    = "Hiruzen unleashes the God of Death upon an enemy in exchange for his soul. Each turn, the God of Death deals 40 affliction damage to the target and 20 affliction damage to Hiruzen. Both are permanently stunned, and the target cannot reduce damage or become invulnerable. Hiruzen is permanently stunned. Ends when Hiruzen dies."
        , classes = [Ranged, Unreflectable, Unremovable, Soulbound, Bypassing]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, apply 0 [Afflict 40, Stun All, Expose])
                    , (Self,  apply 0 [Afflict 20, Stun All])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Major Summoning: Enma"
        , desc    = "Hiruzen summons the great monkey Enma, who makes his team invulnerable for 1 turn. Once used, this skill permanently becomes [Adamantine Prison][n]. As long as Hiruzen is alive, he provides 5 points of damage reduction to his team."
        , classes = [Physical, Melee, Summon, Unreflectable, Unremovable]
        , cost    = χ [Gen, Nin, Tai]
        , channel = Ongoing 0
        , start   = [ (Allies, apply 1 [Immune All, Endure]
                             • apply 0 [Reduce All 5])
                    , (Self,   vary 0 2 1)
                    ]
        }
      , newSkill
        { label   = "Adamantine Prison"
        , desc    = "Provides 15 permanent destructible defense to Hiruzen's team and prevents their health from dropping below 1 for 1 turn."
        , classes = [Physical]
        , cost    = χ [Nin]
        , cd      = 3
        , effects = [(Allies, defend 0 15 • apply 1 [Endure])]
        }
      ]
    , invuln "Mud Wall" "Hiruzen" [Physical]
    ] []
  ]
