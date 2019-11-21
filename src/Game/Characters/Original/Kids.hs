{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Original.Kids (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Naruto Uzumaki"
    "A genin from Team 7, Naruto is an orphan with the goal of becoming Hokage. His signature Shadow Clones copy his moves to perform powerful combos and wield the Rasengan."
    [LeafVillage, Eleven, Genin, Jinchuriki, Sage, Sensor, Wind, Lightning, Earth, Water, Fire, Yin, Yang, Uzumaki]
    [ [ Skill.new
        { Skill.name      = "Naruto Uzumaki Barrage"
        , Skill.desc      = "Using his version of the Lions Barrage, Naruto deals 20 damage to an enemy. Deals 10 additional damage during [Shadow Clones]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` userHas "Shadow Clones"
                damage (20 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rasengan"
        , Skill.desc      = "Naruto hits an enemy with an orb of chakra, dealing 45 damage to them and stunning their skills for 1 turn. Requires [Shadow Clones]."
        , Skill.require   = HasI 1 "Shadow Clones"
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 45
                apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Clones"
        , Skill.desc      = "Naruto creates multiple shadow clones who hide him and fight in his place, providing 15 points of damage reduction for 4 turns."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self $ apply 4 [Reduce [All] Flat 15] ]
        }
      ]
    , [ invuln "Sexy Technique" "Naruto" [Chakra] ]
    ]
  , Character
    "Sakura Haruno"
    "A genin from Team 7, Sakura is intelligent but self-conscious. Her recent training from Tsunade has prepared her to heal her allies and knock back her enemies."
    [LeafVillage, Eleven, Genin, Earth, Water, Yin, Yang, Uchiha]
    [ [ Skill.new
        { Skill.name      = "KO Punch"
        , Skill.desc      = "Sakura punches an enemy with all her strength, dealing 20 damage and stunning their physical and mental skills for 1 turn. Deals 10 additional damage during [Inner Sakura]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` userHas "Inner Sakura"
                damage (20 + bonus)
                apply 1 [Stun Mental, Stun Physical]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Healing Technique"
        , Skill.desc      = "Using basic healing techniques, Sakura restores 25 health to herself or an ally."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Ally $ heal 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inner Sakura"
        , Skill.desc      = "Sakura's inner self surfaces and urges her on. For 4 turns, Sakura gains 10 points of damage reduction and ignores harmful status effects."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 4 [Enrage, Reduce [All] Flat 10] ]
        }
      ]
    , [ invuln "Substitution Technique" "Sakura" [Chakra] ]
    ]
  , Character
    "Sasuke Uchiha"
    "A genin from Team 7, Sasuke seeks vengeance against his brother for slaughtering the rest of their clan. Using his Sharingan, Sasuke targets his opponent's weak spots and anticipates their attacks."
    [LeafVillage, Genin, Uchiha]
    [ [ Skill.new
        { Skill.name      = "Lions Barrage"
        , Skill.desc      = "Copying a taijutsu combo that Lee used on him, Sasuke deals 30 damage to an enemy. Deals 15 additional damage if the target is affected by [Sharingan]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 15 `bonusIf` targetHas "Sharingan"
                damage (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Sasuke attacks an enemy with a bolt of lightning, dealing 30 piercing damage. Deals 25 additional damage if the target is affected by [Sharingan]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 25 `bonusIf` targetHas "Sharingan"
                pierce (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Sasuke focuses his gaze on an enemy. For 4 turns, he gains 15 points of damage reduction and the enemy cannot reduce damage or become invulnerable."
        , Skill.classes   = [Mental, Ranged, Soulbound, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 4 [Reduce [All] Flat 15]
          , To Enemy $ apply 4 [Expose]
          ]
        }
      ]
    , [ invuln "Block" "Sasuke" [Physical] ]
    ]
  , Character
    "Kiba Inuzuka"
    "A genin from Team 8, Kiba is short-tempered and impulsive. His powerful taijutsu skills are amplified when he fuses with his dog, Akamaru, and transforms into a double-headed monster."
    [LeafVillage, Eleven, Genin, Earth, Yang, Inuzuka]
    [ [ Skill.new
        { Skill.name      = "Wolf Fang"
        , Skill.desc      = "Kiba projects a vacuum vortex at an enemy, dealing 30 damage. Deals 5 additional damage if the target is affected by [Dynamic Marking]. Costs 1 taijutsu chakra during [Two-Headed Wolf]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` targetHas "Dynamic Marking"
                damage (30 + bonus)
          ]
        , Skill.changes   =
            changeWithChannel "Two-Headed Wolf" \x -> x { Skill.cost = [Tai] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Two-Headed Wolf"
        , Skill.desc      = "Kiba and Akamaru transform into giant beasts, gaining 15 points of damage reduction and dealing 15 damage to all enemies for 3 turns. Deals 5 additional damage to enemies affected by [Dynamic Marking]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemies do
              bonus <- 5 `bonusIf` targetHas "Dynamic Marking"
              damage (15 + bonus)
          , To Self $ apply 1 [Reduce [All] Flat 15]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dynamic Marking"
        , Skill.desc      = "Akamaru sprays urine on an enemy, preventing them from reducing damage or becoming invulnerable for 3 turns. Cannot be used on an enemy already affected by this skill."
        , Skill.require   = HasU 0 "Dynamic Marking"
        , Skill.classes   = [Bane, Ranged]
        , Skill.effects   =
          [ To Enemy $ apply 3 [Expose] ]
        }
      ]
    , [ invuln "Smoke Bomb" "Kiba" [Physical] ]
    ]
  , Character
    "Shino Aburame"
    "A genin from Team 8, Shino is reserved and tactical. He directs the insects living within his body to leech chakra from his enemies and protect his teammates."
    [LeafVillage, Eleven, Genin, Earth, Fire, Yang, Aburame]
    [ [ Skill.new
        { Skill.name      = "Chakra Leech"
        , Skill.desc      = "Chakra-draining bugs attack an enemy, dealing 20 affliction damage and absorbing 1 random chakra. Deals 5 additional damage per target's stack of [Parasite]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                stacks <- targetStacks "Parasite"
                afflict (20 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Parasite"
        , Skill.desc      = "Shino directs one of his bugs to attach itself to an enemy. For 4 turns, the target's damage is weakened by 5."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 4 [Weaken [All] Flat 5] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wall of Insects"
        , Skill.desc      = "A massive swarm of insects surrounds Shino's team, providing 20 permanent destructible defense to them."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Allies $ defend Permanent 20 ]
        }
      ]
    , [ invuln "Hide" "Shino" [Mental] ]
    ]
  , Character
    "Hinata Hyūga"
    "A genin from Team 8, Hinata is the shy and withdrawn heiress of the Hyūga clan. With her clan's signature Byakugan, she senses her opponent's energy flow and blocks it at key points to remove their chakra."
    [LeafVillage, Eleven, Genin, Fire, Lightning, Hyuga, Uzumaki]
    [ [ Skill.new
        { Skill.name      = "Gentle Fist"
        , Skill.desc      = "Using the Hyūga clan's signature taijutsu style, Hinata deals 20 damage to an enemy for 2 turns. During [Byakugan], this skill depletes 1 random chakra of the opposing team each turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
            [ To Enemy do
                  whenM (userHas "Byakugan") $ deplete 1
                  damage 20
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Sixty-Four Palms"
        , Skill.desc      = "Hinata deals 15 damage to all enemies and provides 10 destructible defense to her team for 1 turn. Deals 5 additional damage during [Byakugan]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemies do
                bonus <- 5 `bonusIf` userHas "Byakugan"
                damage (15 + bonus)
          , To Allies $ defend 1 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Byakugan"
        , Skill.desc      = "Hinata activates her Byakugan, gaining 15 points of damage reduction for 4 turns."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 4 [Reduce [All] Flat 15] ]
        }
      ]
    , [ invuln "Block" "Hinata" [Physical] ]
    ]
  , Character
    "Ino Yamanaka"
    "A genin from Team 10, Ino is as confident as she is vain. She projects the strength of her will directly into the minds of her opponents, bypassing their physical defenses and rendering them helpless."
    [LeafVillage, Eleven, Genin, Sensor, Earth, Water, Fire, Yin, Yang, Yamanaka]
    [ [ Skill.new
        { Skill.name      = "Mind Destruction"
        , Skill.desc      = "Ino launches a mental assault on an enemy, stunning their non-mental skills and preventing them from reducing damage or becoming invulnerable for 1 turn. Acting through them, she lashes out and deals 30 piercing damage to a random enemy."
        , Skill.classes   = [Mental, Ranged, Bypassing]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun NonMental, Expose]
          , To REnemy $ pierce 30
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mind Transfer"
        , Skill.desc      = "Ino takes over the mind of an enemy, stunning them and preventing them from reducing damage or becoming invulnerable for 4 turns. While active, this skill becomes [Art of the Valentine][r]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Gen]
        , Skill.cooldown  = 3
        , Skill.dur       = Control 4
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun All, Expose]
          , To Self $ hide 1 [Alternate "Mind Transfer" "Art of the Valentine"]
          ]
        }
      , Skill.new
        { Skill.name      = "Art of the Valentine"
        , Skill.desc      = "Acting through her victim's body, Ino deals 25 damage to an enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy $ damage 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Hair Trap"
        , Skill.desc      = "Ino endows a strand of hair with chakra to create an ensnaring trap near an enemy. If they use a skill on Ino or her allies during their next turn, their cooldowns will increase by 1 turn for 2 turns."
        , Skill.classes   = [Chakra, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ trap 1 OnHarm $ apply 2 [Snare 1] ]
        }
      ]
    , [ invuln "Block" "Ino" [Physical] ]
    ]
  , Character
    "Shikamaru Nara"
    "A genin from Team 10, Shikamaru is considered the smartest of the Konoha 11. He manipulates shadows to disable and attack his enemies. Whomever he fights, it's only a matter of time before he comes up with the perfect plan to defeat them."
    [LeafVillage, Eleven, Genin, Fire, Earth, Yin, Nara]
    [ [ Skill.new
        { Skill.name      = "Meditate"
        , Skill.desc      = "Shikamaru sits down and contemplates an enemy. Over the next 5 turns, he composes a strategy against them. Cannot be used on an enemy already affected by this skill."
        , Skill.require   = HasU 0 "Meditate"
        , Skill.classes   = [Mental, Ranged, Uncounterable, Unreflectable]
        , Skill.effects   =
          [ To Enemy $ tag 5 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Strangle"
        , Skill.desc      = "Shadow tendrils choke Shikamaru's enemies, preventing them from reducing damage or becoming invulnerable for 1 turn and dealing 15 damage. Lasts 1 additional turn on enemies affected by [Meditate]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
            [ To Enemies do
                  bonus <- 1 `bonusIf` targetHas "Meditate"
                  apply (1 + bonus) [Expose]
                  damage 15
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Possession"
        , Skill.desc      = "Shikamaru captures all enemies in shadows, stunning their non-mental skills for 1 turn. Lasts 1 additional turn on enemies affected by [Meditate]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies do
                bonus <- 1 `bonusIf` targetHas "Meditate"
                apply (1 + bonus) [Stun NonMental]
          ]
        }
      ]
    , [ invuln "Hide" "Shikamaru" [Mental] ]
    ]
  , let loadout = [0, 0, 0] in
    Character
    "Chōji Akimichi"
    "A genin from Team 10, Chōji is a voracious eater and loyal friend. His clan's special pills immensely magnify his innate strength and unlock different abilities, but the toll they take on his body can kill him if he pushes himself too far, too fast."
    [LeafVillage, Eleven, Genin, Earth, Fire, Yang, Akimichi]
    [ [ Skill.new
        { Skill.name      = "Spinach Pill"
        , Skill.desc      = "Chōji eats the mildest Akimichi pill, losing 5 health down to a minimum of 1 and gaining the strength he needs to protect his friends. While alive, he provides 5 points of damage reduction to his allies."
        , Skill.classes   = [Chakra, Nonstacking]
        , Skill.dur       = Action Permanent
        , Skill.start     =
          [ To Self do
                sacrifice 1 5
                alternate loadout 1
          ]
        , Skill.effects   =
          [ To XAllies $ apply' "Protected" 1 [Reduce [All] Flat 5] ]
        }
      , Skill.new
        { Skill.name      = "Obstructing Tackle"
        , Skill.desc      = "Chōji charges an enemy, dealing 20 damage to them and weakening their damage by 20 for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Weaken [All] Flat 20]
          ]
        }
      , Skill.new
        { Skill.name       = "Partial Expansion"
        , Skill.desc       = "Chōji greatly enlarges one of his arms and swings it through the enemy team, dealing 20 damage."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemies $ damage 20 ]
        }
      , Skill.new
        { Skill.name      = "Justice Punch"
        , Skill.desc      = "Chōji slams his fist into an enemy and knocks them to the ground, stunning them for 1 turn and preventing them from reducing damage or becoming invulnerable, then deals 25 damage to them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Stun All, Expose]
                damage 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Curry Pill"
        , Skill.desc      = "Chōji eats the first two Akimichi pills in one go, losing 15 health down to a minimum of 1 and unlocking huge reserves of chakra in addition to immense physical strength. While alive, he provides 10 points of damage reduction to his allies."
        , Skill.classes   = [Chakra, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.dur       = Action Permanent
        , Skill.start     =
          [ To Self do
                cancelChannel "Spinach Pill"
                sacrifice 1 15
                alternate loadout 2
          ]
        , Skill.effects   =
          [ To XAllies $ apply' "Protected" 1 [Reduce [All] Flat 10] ]
        }
      , Skill.new
        { Skill.name      = "Human Boulder"
        , Skill.desc      = "Chōji enlarges his body, tucks in his limbs, and uses chakra to propel himself into an unstoppable roll. For 2 turns, he deals 10 damage and 5 piercing damage to an enemy. While active, Chōji gains 15 points of damage reduction and ignores stuns and disabling effects."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                damage 10
                pierce 5
          , To Self $ apply 1 [Focus, Reduce [All] Flat 15]
          ]
        }
      , Skill.new
        { Skill.name      = "Full Expansion"
        , Skill.desc      = "Chōji greatly enlarges himself and body-slams the enemy team, dealing 30 damage and stunning them for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                damage 30
                apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "Chakra Wings"
        , Skill.desc      = "Chōji's overflowing chakra erupts around him. For 3 turns, it restores 15 health and bestows 1 random chakra. While active, Chōji does not take damage from [Chili Pill]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 5
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Self do
                heal 15
                gain [Rand]
                hide' "unchili" -1 []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chili Pill"
        , Skill.desc      = "Chōji swallows all three Akimichi pills, losing 10 health down to a minimum of 1 and gaining so much chakra that butterfly wings of pure energy erupt from his back. While alive, he loses 15 health per turn, provides 15 points of damage reduction to his allies, and ignores stuns and disabling effects."
        , Skill.classes   = [Chakra, Nonstacking]
        , Skill.cost      = [Rand, Rand]
        , Skill.dur       = Action Permanent
        , Skill.start     =
          [ To Self do
                sacrifice 1 10
                alternate loadout 3
          ]
        , Skill.effects   =
          [ To XAllies $ apply' "Protected" 1 [Reduce [All] Flat 15]
          , To Self do
                unlessM (userHas "unchili") $ sacrifice 0 15
                apply 1 [Focus, Alternate "Block" "Block", Face]
          ]
        , Skill.stunned   =
          [ To Self do
                unlessM (userHas "unchili") $ sacrifice 0 15
                apply 1 [Alternate "Block" "Block", Face]
          ]
        }
      , Skill.new
        { Skill.name      = "Curry Pill"
        , Skill.desc      = "Chōji eats the second Akimichi pill, losing 5 health down to a minimum of 1 and unlocking huge reserves of chakra. While alive, he provides 10 points of damage reduction to his allies."
        , Skill.classes   = [Chakra, Nonstacking]
        , Skill.dur       = Action Permanent
        , Skill.start     =
          [ To Self do
                cancelChannel "Spinach Pill"
                sacrifice 1 5
                alternate loadout 2
          ]
        , Skill.effects   =
          [ To XAllies $ apply' "Protected" 1 [Reduce [All] Flat 10] ]
        }
      , Skill.new
        { Skill.name      = "Chili Pill"
        , Skill.desc      = "Chōji eats the third Akimichi pill and gains so much chakra that butterfly wings of pure energy erupt from his back. While alive, he loses 15 health per turn, provides 15 points of damage reduction to his allies, and ignores stuns and disabling effects."
        , Skill.classes   = [Chakra, Nonstacking]
        , Skill.dur       = Action Permanent
        , Skill.start     =
          [ To Self do
                cancelChannel "Curry Pill"
                alternate loadout 3
          ]
        , Skill.effects   =
          [ To XAllies $ apply' "Protected" 1 [Reduce [All] Flat 15]
          , To Self do
                unlessM (userHas "unchili") $ sacrifice 0 15
                apply 1 [Focus, Alternate "Block" "Block", Face]
          ]
        , Skill.interrupt =
          [ To Self do
                unlessM (userHas "unchili") $ sacrifice 0 15
                apply 1 [Alternate "Block" "Block", Face]
          ]
        }
      , Skill.new
        { Skill.name      = "Butterfly Bombing"
        , Skill.desc      = "Chōji focuses all of his chakra into a fist and drives it into an enemy, dealing 45 damage and killing them if their health reaches 20 or lower."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Enemy do
                damage 45
                targetHealth <- target health
                when (targetHealth <= 20) kill
          ]
        }
      ]
    , [ invuln "Block" "Chōji" [Physical]
      , Skill.new
        { Skill.name      = "Block"
        , Skill.desc      = "Chōji becomes invulnerable for 1 turn. While active, Chōji does not take damage from [Chili Pill]."
        , Skill.classes   = [Physical]
        , Skill.cooldown  = 4
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                apply 1 [Invulnerable All]
                hide' "unchili" -1 []
          ]
        }
      ]
    ]
  , Character
    "Rock Lee"
    "A member of team Guy, Lee is a genin whose inability to use ninjutsu and genjutsu has led him to devote his life to mastering taijutsu. His strength increases with every strike, building up unstoppable momentum as the fight goes on."
    [LeafVillage, Eleven, Genin]
    [ [ Skill.new
        { Skill.name      = "Ferocious Fist"
        , Skill.desc      = "For 3 turns, Lee deals 10 damage to an enemy and gains 10 points of damage reduction. Deals 15 additional damage during [Fifth Gate Opening]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemy do
                bonus <- 15 `bonusIf` userHas "Fifth Gate Opening"
                damage (10 + bonus)
                tag 1
          , To Self $ apply 1 [Reduce [All] Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Primary Lotus"
        , Skill.desc      = "Having opened the first gate, Lee uses a high-powered taijutsu to deal 30 damage to an enemy. Deals 10 additional damage to the target of [Ferocious Fist]. Deals 30 additional damage during [Fifth Gate Opening]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                targetBonus <- 10 `bonusIf` targetHas "Ferocious Fist"
                userBonus   <- 30 `bonusIf` userHas "Fifth Gate Opening"
                damage (30 + targetBonus + userBonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fifth Gate Opening"
        , Skill.desc      = "Lee cures himself of enemy effects, loses 50 health down to a minimum of 1, and becomes invulnerable for 2 turns. While active, this skill becomes [Hidden Lotus][t][t]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                cureAll
                apply 2 [ Invulnerable All
                        , Alternate "Fifth Gate Opening" "Hidden Lotus"
                        ]
                sacrifice 1 50
          ]
        }
      , Skill.new
        { Skill.name      = "Hidden Lotus"
        , Skill.desc      = "Pushing his body far past its limits, Lee deals 100 damage to an enemy with an onslaught of punches."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Tai]
        , Skill.effects   =
          [ To Enemy $ damage 100 ]
        }
      ]
    , [ invuln "Block" "Lee" [Physical] ]
    ]
  , Character
    "Tenten"
    "A member of Team Guy, Tenten is a fiercely independent weapon specialist who intends to become a legendary kunoichi. She keeps an entire arsenal sealed in scrolls to rain down upon the battlefield, wounding and disabling her opponents."
    [LeafVillage, Eleven, Genin]
    [ [ Skill.new
        { Skill.name      = "Unsealing Technique"
        , Skill.desc      = "Tenten launches a barrage of weapons at an enemy, dealing 20 damage to them and 10 damage to the rest of their team. All enemies are further weakened to Tenten's next [Rising Dragon Control]."
        , Skill.classes   = [Physical, Ranged, Uncounterable]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To XEnemies $ damage 10
          , To Enemies do
                addStack
                whenM (userHas "Rising Twin Dragons") addStack
          , To Self $ remove "Rising Twin Dragons"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rising Dragon Control"
        , Skill.desc      = "Tenten's weapons scattered across the battlefield shoot upward, spending all stacks of [Unsealing Technique] to deal 5 damage plus 10 per stack to all enemies. For 1 turn, physical, chakra, and summon damage of all enemies is weakened by 5 plus 10 per stack spent."
        , Skill.classes   = [Physical, Ranged, Uncounterable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemies do
                stacks <- targetStacks "Unsealing Technique"
                damage (5 + 10 * stacks)
                bonus <- 1 `bonusIf` userHas "Rising Twin Dragons"
                apply (1 + bonus)
                    [Weaken [Physical, Chakra, Summon] Flat (5 + 10 * stacks)]
                remove "Unsealing Technique"
          ,  To Self $ remove "Rising Twin Dragons"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rising Twin Dragons"
        , Skill.desc      = "Summoning scrolls conceal Tenten in a cloud of smoke, rendering her invulnerable to physical, chakra, and summon skills for 1 turn. The scrolls aid her the next time she uses one of her other skills. If she uses [Unsealing Technique], it adds 1 additional stack of [Unsealing Technique]. If she uses [Rising Dragon Control], the duration of its effect is increased by 1 turn. Cannot be used while active."
        , Skill.require   = HasI 0 "Rising Twin Dragons"
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                tag Permanent
                apply' "Twin Dragons Defense" 1 [ Invulnerable Physical
                                                , Invulnerable Chakra
                                                , Invulnerable Summon
                                                ]
          ]
        }
      ]
    , [ invuln "Spiked Boulder Shield" "Tenten" [Physical] ]
    ]
  , Character
    "Neji Hyūga"
    "A member of team Guy, Neji is the cold and fatalistic prodigy of the Hyūga clan. He has mastered numerous techniques, from crippling blows to whirling invulnerability."
    [LeafVillage, Eleven, Genin, Fire, Earth, Water, Hyuga]
    [ [ Skill.new
        { Skill.name      = "Gentle Fist"
        , Skill.desc      = "Using the Hyūga clan's signature taijutsu, Neji deals 25 damage to an enemy for 2 turns. During this time, that enemy's damage is weakened by 5."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
            [ To Enemy do
                damage 25
                apply 1 [Weaken [All] Flat 5]
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Palm Rotation"
        , Skill.desc      = "Neji becomes invulnerable for 1 turn and deals 15 damage to all enemies."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
            [ To Self $ apply 1 [Invulnerable All]
            , To Enemies $ damage 15
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Sixty-Four Palms"
        , Skill.desc      = "Neji seals an enemy's chakra nodes shut with a rapid sequence of blows, dealing 40 damage and depleting 1 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                deplete 1
                damage 40
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Neji" [Mental] ]
    ]
  , Character
    "Gaara"
    "The youngest of the sand siblings, Gaara is the unstable jinchūriki of Shukaku, the one-tailed beast. With its aid, Gaara manipulates the sand in his gourd to crush his enemies. The shell of sand around his body makes him invulnerable to all but the strongest of blows."
    [SandVillage, Genin, Jinchuriki, Sensor, Wind, Earth, Lightning, SandClan]
    [ [ Skill.new
        { Skill.name      = "Sand Coffin"
        , Skill.desc      = "Sand surrounds an enemy, stunning their non-mental skills for 2 turns. While active, the enemy cannot reduce damage or become invulnerable and this skill becomes [Sand Burial][n][n]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Control 2
        , Skill.effects   =
          [ To Self $ hide 1 [Alternate "Sand Coffin" "Sand Burial"]
          , To Enemy $ apply 1 [Expose, Stun NonMental]
          ]
        , Skill.interrupt =
          [ To Self $ remove "sand coffin" ]
        }
      , Skill.new
        { Skill.name      = "Sand Burial"
        , Skill.desc      = "With a clenched fist, Gaara crushes the target of [Sand Coffin] into nothingness, killing them."
        , Skill.require   = HasU 1 "Sand Coffin"
        , Skill.classes   = [Physical, Ranged, Uncounterable, Unreflectable]
        , Skill.cost      = [Nin, Nin]
        , Skill.effects   =
          [ To Enemy kill ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Clone"
        , Skill.desc      = "Gaara creates a clone of sand that mimics him and provides defense. Until an enemy uses a skill that deals non-affliction damage to him, Gaara ignores harmful status effects. Cannot be used while active."
        , Skill.require   = HasI 0 "Sand Clone"
        , Skill.classes   = [Physical, Unremovable]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                apply Permanent [Enrage]
                trap' Permanent (OnDamaged NonAffliction) do
                    remove "Sand Clone"
                    removeTrap "Sand Clone"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Armor"
        , Skill.desc      = "By covering himself with sand, Gaara gains 40 non-stacking permanent destructible defense."
        , Skill.classes   = [Nonstacking, Chakra]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ defend Permanent 40 ]
        }
      ]
    , [ invuln "Sand Shield" "Gaara" [Physical] ]
    ]
  , Character
    "Kankurō"
    "One of the three sand siblings, Kankurō is a master puppeteer. His strategically-placed poison bombs pierce through enemy defenses. In order to harm him, his enemies will have to go through his army of puppets first."
    [SandVillage, Genin, Wind, Lightning, Earth, Water, SandClan]
    [ [ Skill.new
        { Skill.name      = "Iron Maiden"
        , Skill.desc      = "Two of Kankurō's puppets trap and stab an enemy, dealing 30 piercing damage."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 30 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Poison Bomb"
        , Skill.desc      = "One of Kankurō's puppets creates a cloud of smoke that deals 10 affliction damage to all enemies."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ afflict 10 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Puppet Technique"
        , Skill.desc      = "Kankurō fashions a chakra-controlled puppet which serves as a decoy, increasing his damage by 5 for 4 turns and providing 15 permanent destructible defense."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                defend Permanent 15
                apply 4 [Strengthen [All] Flat 5]
          ]
        }
      ]
    , [ invuln "Substitution Technique" "Kankurō" [Chakra] ]
    ]
  , Character
    "Temari"
    "The older sister of Gaara and Kankurō, Temari is a cruel and blunt expert in war-fan combat. She uses her massive iron fan to generate powerful gusts of wind that repel attacks on her team."
    [SandVillage, Genin, Wind, SandClan]
    [ [ Skill.new
        { Skill.name       = "Cyclone Scythe"
        , Skill.desc       = "Temari creates a razor sharp wind with her fan that hits an enemy, dealing 20 damage. The following turn, she is invulnerable to non-mental skills."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self $ apply 1 [Invulnerable NonMental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Blade Dance"
        , Skill.desc      = "The wind weasel Kamatari uses a massive version of Cyclone Scythe to deal 35 damage to all enemies."
        , Skill.classes   = [Summon, Ranged]
        , Skill.cost      = [Nin, Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies $ damage 35 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sandstorm"
        , Skill.desc      = "Temari kicks up dust with her fan, granting her team invulnerability for 1 turn and weakening all enemies' damage by 15 for 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Allies $ apply 1 [Invulnerable All]
          , To Enemies $ apply 2 [Weaken [All] Flat 15]
          ]
        }
      ]
    , [ invuln "Block" "Temari" [Physical] ]
    ]
  ]
