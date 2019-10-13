{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Shippuden.Leaders (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Orochimaru"
    "One of three legendary sannin, Orochimaru has cheated death time and time again. As his body slowly rots away, he is forced to discard it and find a new host to possess."
    [ [ Skill.new
        { Skill.name      = "Body Replacement Substitution"
        , Skill.desc      = "This skill can only be used when Orochimaru's health is at or below 20. Having sustained grievous injuries, Orochimaru's body has reached the end of its usefulness. He sheds it like a second skin, restoring his health to 60."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.varicd    = True
        , Skill.effects   =
          [ To Self $ setHealth 60 ]
        , Skill.changes   =
            \n x -> if health n > 20 then x { Skill.require = Unusable } else x
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ten Thousand Snakes Wave"
        , Skill.desc      = "A horde of sword-brandishing snakes pours from Orochimaru's mouth and deals 20 affliction damage to an enemy. Next turn, all stun skills used by the target will have their duration reduced by 1 turn."
        , Skill.classes   = [Physical, Bane, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy do
                afflict 20
                apply 1 [Throttle 1 $ Any Stun]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kusanagi"
        , Skill.desc      = "Orochimaru pins down an enemy with his legendary sword, dealing 20 piercing damage to them for 2 turns. While active, the target's physical and chakra skills are stunned."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 3
        , Skill.dur       = Control 2
        , Skill.effects   =
          [ To Enemy do
                pierce 20
                apply 1 [Stun Physical, Stun Chakra]
          ]
        }
      ]
    , [ invuln "Summoning: Triple Rashōmon" "Orochimaru" [Summon] ]
    ]
  , Character
    "Jiraiya"
    "One of three legendary sannin, Jiraiya has accepted Naruto as his student. Famed as the Toad Sage, he believes that Naruto is the child spoken of in the prophecies, and that it is his responsibility to teach him to save the world rather than destroy it."
    [ [ Skill.new
        { Skill.name      = "Giant Flame Bomb"
        , Skill.desc      = "Jiraiya spits out a burst of fire, dealing 20 damage to an enemy. Once used, this skill becomes [Toad Oil Bomb][r]."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                stacks <- targetStacks "Toad Oil Bomb"
                afflict (10 * stacks)
          , To Self  $ vary "Giant Flame Bomb" "Toad Oil Bomb"
          ]
        }
      , Skill.new
        { Skill.name      = "Toad Oil Bomb"
        , Skill.desc      = "Jiraiya spits a dense projectile of oil that deals 10 affliction damage to an enemy and ends their Action and Control skills in progress. All subsequent uses of [Giant Flame Bomb] on the target will deal 10 additional affliction damage. Once used, this skill becomes [Giant Flame Bomb][r]."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                afflict 10
                interrupt
                addStack
          , To Self $ vary "Giant Flame Bomb" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Toad Subjugation Shadow Manipulation"
        , Skill.desc      = "Jiraiya merges with an enemy's shadow. Next turn, skills that enemies use on Jiraiya will be reflected onto the target."
        , Skill.classes   = [Mental, Invisible, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                targetSlot <- target slot
                self $ apply 2 [Redirect targetSlot]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Raging Lion's Mane"
        , Skill.desc      = "Chakra-laced hair as strong as steel cables grows from Jiraiya's head and wrap around him or one of his allies. For 3 turns, enemies who use melee physical skills on the target will take 25 damage, and enemies who use ranged physical skills on the target will take 15 damage, and their melee and physical skills will be stunned for 1 turn."
        , Skill.classes   = [Invisible, Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Ally do
                trapFrom 3 (OnHarmed Melee) $ whenM (targetHas "mane") $
                    damage 25
                trapFrom 3 (OnHarmed Ranged) $ whenM (targetHas "mane") do
                    damage 15
                    apply 1 [Stun All]
                trapFrom 3 (OnHarmed Physical) $ flag' "mane"
          ]
        }
      ]
    , [ invuln "Summoning: Gamaken" "Jiraiya" [Summon] ]
    ]
  , Character
    "Tsunade"
    "One of three legendary sannin, Tsunade has become the fifth Hokage. Knowing the Hidden Leaf Village's fate depends on her, she holds nothing back. Even if one of her allies is on the verge of dying, she can keep them alive long enough for her healing to get them back on their feet."
    [ [ Skill.new
        { Skill.name      = "Heaven Spear Kick"
        , Skill.desc      = "Tsunade spears an enemy with her foot, dealing 20 piercing damage to them. If an ally is affected by [Healing Wave], their health cannot drop below 1 next turn. Spends a Seal if available to deal 20 additional damage and demolish the target's destructible defense and Tsunade's destructible barrier."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                has <- userHas "Strength of One Hundred Seal"
                when has demolishAll
                pierce (20 + if has then 20 else 0)
          , To Allies $ whenM (targetHas "Healing Wave") $ apply 1 [Endure]
          , To Self do
              remove "Strength of One Hundred Seal"
              vary "Strength of One Hundred Seal" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Healing Wave"
        , Skill.desc      = "Tsunade pours chakra into an ally, restoring 30 health to them immediately and 10 health each turn for 2 turns. Spends a Seal if available to restore 10 additional health immediately and last 3 turns."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly do
                has <- userHas "Strength of One Hundred Seal"
                heal (20 + if has then 10 else 0)
                apply (if has then (-3) else (-2)) [Heal 10]
          , To Self do
                remove "Strength of One Hundred Seal"
                vary "Strength of One Hundred Seal" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Strength of One Hundred Seal"
        , Skill.desc      = "Tsunade activates her chakra-storing Seal, restoring 25 health and empowering her next skill. Spends a Seal if available to instead restore 50 health to Tsunade and gain 2 random chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                heal 25
                tag 0
                vary "Strength of One Hundred Seal"
                     "Strength of One Hundred Seal"
          ]
        }
      , Skill.new
        { Skill.name      = "Strength of One Hundred Seal"
        , Skill.desc      = "Tsunade activates her chakra-storing Seal, restoring 25 health and empowering her next skill. Spends a Seal if available to instead restore 50 health to Tsunade and gain 2 random chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                heal 50
                gain [Rand, Rand]
                vary "Strength of One Hundred Seal" baseVariant
                remove "Strength of One Hundred Seal"
          ]
        }
      ]
    , [ invuln "Block" "Tsunade" [Physical] ]
    ]
  , Character
    "Ōnoki"
    "The third Tsuchikage of the Hidden Rock Village, Onoki is the oldest and most stubborn Kage. His remarkable ability to control matter on an atomic scale rapidly grows in strength until it can wipe out a foe in a single attack."
    [ [ Skill.new
        { Skill.name      = "Earth Golem"
        , Skill.desc      = "A golem of rock emerges from the ground, providing 10 permanent destructible defense to his team and dealing 10 damage to all enemies."
        , Skill.classes   = [Chakra, Physical, Melee]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Allies  $ defend 0 104
          , To Enemies $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lightened Boulder"
        , Skill.desc      = "Ōnoki negates the gravity of an ally, providing 10 points of damage reduction to them for 2 turns. While active, the target cannot be countered or reflected."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly $ apply 2 [Reduce All Flat 10, AntiCounter] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Atomic Dismantling"
        , Skill.desc      = "The atomic bonds within an enemy shatter, dealing 20 piercing damage. Every time this skill is used, its damage increases by 10."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Atomic Dismantling"
                pierce (20 + 10 * stacks)
          , To Self addStack
          ]
        }
      ]
    , [ invuln "Flight" "Ōnoki" [Chakra] ]
    ]
  , Character
    "Mei Terumi"
    "The third Mizukage of the Hidden Mist Village, Mei works tirelessly to help her village overcome its dark history and become a place of kindness and prosperity. Her corrosive attacks eat away at the defenses of her enemies."
    [ [ Skill.new
        { Skill.name      = "Solid Fog"
        , Skill.desc      = "Mei exhales a cloud of acid mist that deals 15 affliction damage to an enemy for 3 turns."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy $ apply 3 [Afflict 15] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Bomb"
        , Skill.desc      = "Water floods the battlefield, preventing all enemies from reducing damage or becoming invulnerable for 1 turn and dealing 20 piercing damage to them."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                apply 1 [Expose]
                pierce 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lava Monster"
        , Skill.desc      = "Mei spits a stream of hot lava, dealing 10 affliction damage to all enemies and removing 20 destructible defense from them for 3 turns."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemies do
              demolish 20
              afflict 10
          ]
        }
      ]
    , [ invuln "Flee" "Mei" [Physical] ]
    ]
  , Character
    "Fukasaku and Shima"
    "Revered as the Two Great Sage Toads, Fukasaku and Shima serve Lord Elder Gamamaru in administrating Mount Myōboku. They taught Jiraiya and Naruto how to absorb chakra from natural energy. If left uninterrupted, their sound-based genjutsu can disable their enemies with ease."
    [ [ Skill.new
        { Skill.name      = "Frog Song"
        , Skill.desc      = "Croaking in unison, the Toad Sages emit destabilizing sound waves that deal 20 affliction damage to a target for 2 turns and increase the chakra costs of their skills by 1 additional arbitrary chakra."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                afflict 20
                apply 1 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Dust"
        , Skill.desc      = "Shima exhales a cloud of dust that conceals her and her allies, making them invulnerable to ranged skills for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Allies $ apply 1 [Invulnerable Ranged] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Demonic Illusion: Gamarinsho"
        , Skill.desc      = "Fukasaku and Shima gradually weave a melodic harmony with each other. If this skill is used three times in a row, all enemies will be stunned for 2 turns. Using this skill cancels its previous stuns."
        , Skill.classes   = [Mental, Ranged, Resource]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Self do
                stacks <- userStacks "Harmony"
                remove "Harmony"
                addStacks' 1 "Harmony"
                    if stacks < 3 then stacks + 1 else 1
          , To Enemies do
                remove "Demonic Illusion: Gamarinsho"
                stacks <- userStacks "Harmony"
                when (stacks == 3) $ apply 2 [Stun All]
          ]
        }
      ]
    , [ invuln "Reverse Summoning" "Fukasaku and Shima" [Summon] ]
    ]
  ]
