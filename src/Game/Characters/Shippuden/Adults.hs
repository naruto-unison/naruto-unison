{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Shippuden.Adults (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Kakashi Hatake"
    "For most of his life, Kakashi has avoided using Kamui—his Sharingan's ultimate ability—unless absolutely necessary, due to the mental and physical strain. Those days are over. With years of practice and refinement behind him, Kakashi can now rely on Kamui's dimensional warping to torture his enemies and make his allies intangible."
    [LeafVillage, AlliedForces, Jonin, TeamLeader, Lightning, Water, Earth, Fire, Wind, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Lightning Beast Fang"
        , Skill.desc      = "Kakashi creates a lightning hound out of his Lightning Blade, which deals 25 piercing damage to an enemy. If the target is damaged, they will be stunned for 1 turn. During the next turn, this skill becomes [Lightning Blade Finisher][n][r]."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                trap' 1 (OnDamaged All) $ apply 1 [Stun All]
                pierce 25
          , To Self $ apply 1
                [Alternate "Lightning Beast Fang" "Lightning Blade Finisher"]
          ]
        }
      , Skill.new
        { Skill.name      = "Lightning Blade Finisher"
        , Skill.desc      = "Deals 35 piercing damage to an enemy. Deals 15 additional damage if the target is stunned or affected by [Lightning Beast Fang]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
              bonusA <- 10 `bonusIf` targetHas "Lightning Beast Fang"
              bonusB <- 10 `bonusIf` target stunned
              pierce (35 + max bonusA bonusB)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kamui"
        , Skill.desc      = "If used on an enemy, deals 40 piercing damage to them, increases their cooldowns by 1 turn, and increases the costs of their skills by 1 arbitrary chakra. If used on an ally, cures them of enemy effects and makes them invulnerable for 1 turn."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = [Blood, Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 40
                apply 1 [Snare 1, Exhaust [All]]
          , To XAlly do
                cureAll
                apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Team Tactics"
        , Skill.desc      = "For 3 turns, the cooldowns of Kakashi's allies are decreased by 1 turn. While active, the first skill used by an enemy will replace this skill for 1 turn. Copied skills cannot copy other skills and do not transform into alternates."
        , Skill.classes   = [Mental, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To XAllies $ apply 3 [Snare (-1)]
          , To Enemies $ trap 3 (OnAction All) do
                copyLast 1
                everyone $ removeTrap "Team Tactics"
          ]
        }
      ]
    , [ invuln "Shadow Clone" "Kakashi" [Chakra] ]
    ]
  , Character
    "Asuma Sarutobi"
    "Having somehow managed to avoid lung cancer, Asuma remains the leader of Team 10. Using techniques he learned from the Fire Temple, he hinders his opponents and instantly executes weak enemies."
    [LeafVillage, Jonin, TeamLeader, Wind, Fire, Sarutobi]
    [ [ Skill.new
        { Skill.name      = "Thousand Hand Strike"
        , Skill.desc      = "Asuma summons Kannon, the Fire Temple's patron spirit, which provides him with 40 permanent destructible defense and deals 25 damage to an enemy. The following turn, this skill becomes [Kannon Strike][r]. When [Kannon Strike] ends, this skill is disabled for 1 turn."
        , Skill.require   = HasI 0 "Overheating"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 25
          , To Self do
                defend 0 40
                apply 1 [Alternate "Thousand Hand Strike" "Kannon Strike"]
          ]
        }
      , Skill.new
        { Skill.name      = "Kannon Strike"
        , Skill.desc      = "Deals 20 damage to an enemy. This skill remains [Kannon Strike] for another turn."
        , Skill.classes   = [Physical, Melee, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self do
                tag' "Overheating" 2
                prolong 1 "Thousand Hand Strike"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Burning Ash"
        , Skill.desc      = "Asuma continually exhales a cloud of combustible ash upon his enemies, increasing the cooldowns of their skills by 1 turn every turn. While active, this skill becomes [Burning Ash: Ignite][b]."
        , Skill.classes   = [Bane, Ranged, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.dur       = Action 0
        , Skill.effects   =
          [ To Enemies $ apply 0 [Snare 1]
          , To Self $ hide 1 [Alternate "Burning Ash" "Burning Ash: Ignite"]
          ]
        , Skill.stunned   =
          [ To Self $ hide 1 [Alternate "Burning Ash" "Burning Ash: Ignite"] ]
        , Skill.interrupt =
          [ To Self $ remove "burning ash" ]
        }
      , Skill.new
        { Skill.name      = "Burning Ash: Ignite"
        , Skill.desc      = "Asuma strikes a piece of flint between his teeth, producing a spark that sets fire to his piles of ash and burns them away. The fire deals 10 affliction damage to each enemy per stack of [Burning Ash] on them."
        , Skill.classes   = [Bane, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies do
                stacks <- targetStacks "Burning Ash"
                afflict (10 * stacks)
          , To Self do
                cancelChannel "Burning Ash"
                everyone $ remove "Burning Ash"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Decapitate"
        , Skill.desc      = "Asuma mercilessly slaughters an enemy whose health is at or below 25."
        , Skill.classes   = [Physical, Melee, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                health <- target health
                when (health <= 25) kill
          ]
        }
      ]
    , [ invuln "Dodge" "Asuma" [Physical] ]
    ]
  , Character
    "Might Guy"
    "Over the past few years, Guy has learned restraint. By gradually opening his Gates in sequence, he avoids the risk of burning out before the battle is won."
    [LeafVillage, AlliedForces, Jonin, TeamLeader, Fire, Lightning]
    [ [ Skill.new
        { Skill.name      = "Nunchaku"
        , Skill.desc      = "Using his signature Twin Fangs weapons, Guy deals 10 damage to an enemy for 3 turns. While active, if an enemy uses a physical skill on him, he will deal 10 damage to them. Deals 5 additional damage on the first turn per stack of [Single Gate Release]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 3
        , Skill.start     =
          [ To Self flag ]
        , Skill.effects   =
          [ To Self $ trapFrom 1 (OnHarmed Physical) $ damage 10
          , To Enemy do
                firstTurn <- userHas "nunchaku"
                if firstTurn then do
                    stacks <- userStacks "Single Gate Release"
                    damage (10 + 5 * stacks)
                else
                    damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fiery Kick"
        , Skill.desc      = "Guy slams his leg into an enemy, dealing 35 damage and weakening their damage by 20 for 1 turn. Deals 5 additional damage per stack of [Single Gate Release]. At 6 stacks of [Single Gate Release], this skill becomes [Asakujaku][b][t]. At 7 stacks of [Single Gate Release], this skill becomes [Hirudora][b][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Single Gate Release"
                damage (35 + 5 * stacks)
                apply 1 [Weaken [All] Flat 20]
          ]
        }
      , Skill.new
        { Skill.name      = "Asakujaku"
        , Skill.desc      = "With unparalleled speed and power, Guy deals 60 damage to an enemy and stuns them for 1 turn. At 7 stacks of [Single Gate Release], this skill becomes [Hirudora][b][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 60
                apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "Hirudora"
        , Skill.desc      = "Using one single punch, Guy deals 300 damage to an enemy."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   =
          [ To Enemy $ damage 300 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Single Gate Release"
        , Skill.desc      = "Guy opens one of his internal Gates, losing 5 health and gaining 5 points of permanent damage reduction."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.charges   = 7
        , Skill.effects   =
          [ To Self do
                sacrifice 0 5
                stacks <- userStacks "Single Gate Release"
                apply 0 $ Reduce [All] Flat 5 : case stacks of
                    5 -> [Alternate "Fiery Kick" "Asakujaku"]
                    6 -> [Alternate "Fiery Kick" "Hirudora"]
                    _ -> []
          ]
        }
      ]
    , [ invuln "Block" "Guy" [Physical] ]
    ]
  , Character
    "Maki"
    "A jōnin from the Hidden Sand Village, Maki studied under Pakura, the Hero of the Hidden Sand. As a member of the Allied Shinobi Forces Sealing Team, Maki must put aside her long-held grudge against the Hidden Stone Village for killing her teacher."
    [SandVillage, AlliedForces, Jonin]
    [ [ Skill.new
        { Skill.name      = "Binding Cloth"
        , Skill.desc      = "Maki deploys a roll of cloth from within a seal and wraps it around herself, gaining 50% damage reduction for 1 turn. If an enemy uses a skill on Maki, the cloth wraps around them, stunning their physical and melee skills for 1 turn."
        , Skill.classes   = [Physical, Ranged, Invisible, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                apply 1 [Reduce [All] Percent 50]
                trapFrom 1 (OnHarmed All) $ apply 1 [Stun Physical, Stun Melee]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Humidified Cloth"
        , Skill.desc      = "Maki soaks a strip of cloth in steam and lashes out with it, dealing 20 piercing damage to an enemy and preventing them from affecting Maki's team for 1 turn."
        , Skill.classes   = [Physical, Ranged, Unremovable]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [BlockEnemies]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Cloth Paralysis"
        , Skill.desc      = "Maki binds an enemy in rolls of cloth, stunning their chakra and ranged skills for 2 turns. While active, melee skills deal 5 additional damage to the target."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy $
                apply 2 [Stun Chakra, Stun Ranged, Bleed [Melee] Flat 5]
          ]
        }
      ]
    , [ invuln "Cloth Shield" "Maki" [Physical] ]
    ]
  , Character
    "Chiyo"
    "A widely-respected puppeteer and former leader of the Hidden Sand Village's Puppet Brigade, Elder Chiyo has a lifetime of combat experience. Her numerous puppets sow chaos among her enemies and shield her from harm, allowing her to use her other skills with impunity."
    [SandVillage]
    [ [ Skill.new
        { Skill.name      = "Assault Blade"
        , Skill.desc      = "Hovering kunai deal 15 piercing damage to a targeted enemy and a random enemy. During [Ten Puppets Collection], this skill becomes [Three Treasure Suction Crush][r][r]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 15
          , To REnemy $ pierce 15
          ]
        }
      , Skill.new
        { Skill.name      = "Three Treasure Suction Crush"
        , Skill.desc      = "Three puppets in a triangle formation create a vacuum hurricane which sucks in an enemy, dealing 30 damage to them and stunning their non-mental skills for 1 turn. Deals affliction damage if the target is affected by [Lion Sealing]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Stun NonMental]
                has <- targetHas "Lion Roar Sealing"
                if has then afflict 30 else damage 30
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ten Puppets Collection"
        , Skill.desc      = "Chiyo commands a brigade of puppets, gaining 50 permanent destructible defense. Each turn that Chiyo has destructible defense from this skill, her puppets deal 10 damage to a random enemy. While active, this skill becomes [Lion Roar Sealing][b]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.dur       = Action 0
        , Skill.start     =
          [ To Self do
                defend 0 50
                onBreak'
          ]
        , Skill.effects   =
          [ To REnemy $ damage 10
          , To Self $ hide 1
                [ Alternate "Assault Blade" "Three Treasure Suction Crush"
                , Alternate "Ten Puppets Collection" "Lion Roar Sealing"
                ]
          ]
        , Skill.stunned   =
          [ To Self $ hide 1
                [ Alternate "Assault Blade" "Three Treasure Suction Crush"
                , Alternate "Ten Puppets Collection" "Lion Roar Sealing"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Lion Roar Sealing"
        , Skill.desc      = "Chiyo uses an advanced sealing technique on an enemy, making them ignore helpful effects and preventing them from reducing damage or becoming invulnerable for 2 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy $ apply 2 [Expose, Seal] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Self-Sacrifice Reanimation"
        , Skill.desc      = "Chiyo prepares to use her forbidden healing technique on an ally. The next time their health reaches 0, their health is fully restored, they are cured of harmful effects, and Chiyo's health is reduced to 1."
        , Skill.classes   = [Chakra, Invisible, Soulbound]
        , Skill.cost      = [Blood, Nin]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To XAlly $ trap 0 OnRes do
                cureAll
                setHealth 100
                self $ setHealth 1
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Chiyo" [Chakra] ]
    ]
  , Character
    "Akatsuchi"
    "A jōnin from the Hidden Stone Village, Akatsuchi is cheerful and excitable. He uses brute strength and rock golems to pummel his enemies to the ground."
    [StoneVillage, AlliedForces, Jonin, Earth]
    [ [ Skill.new
        { Skill.name      = "High-Speed Assault"
        , Skill.desc      = "Akatsuchi punches an enemy with all his might, dealing 25 damage. Costs 1 taijutsu chakra during [Stone Golem]."
        , Skill.classes   = [Physical, Melee, Uncounterable]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   = [ To Enemy $ damage 25 ]
        , Skill.changes   =
            changeWithChannel "Stone Golem" \x -> x { Skill.cost = [Tai] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Stone Golem"
        , Skill.desc      = "A golem of rock rampages across the battlefield, dealing 15 damage to all enemies for 2 turns and providing Akatsuki with 25% damage reduction."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemies $ damage 15
          , To Self $ apply 1 [Reduce [All] Percent 25]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Devour"
        , Skill.desc      = "A stone golem attacks an enemy, dealing 15 damage and depleting 1 genjutsu or taijutsu chakra."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 15
                deplete1 [Gen, Tai]
          ]
        }
      ]
    , [ invuln "Dodge" "Akatsuchi" [Physical] ]
    ]
  , Character
    "Kurotsuchi"
    "A jōnin from the Hidden Stone Village, Kurotsuchi is the Third Tsuchikage's granddaughter. Witty and self-assured, Kurotsuchi is famed for her unflinching resolve in the face of danger."
    [StoneVillage, AlliedForces, Jonin, Fire, Earth, Water, Yin, Kamizuru]
    [ [ Skill.new
        { Skill.name      = "Lava Quicklime"
        , Skill.desc      = "Kurotsuchi expels a mass of quicklime from her mouth, dealing 25 damage to an enemy and gaining 50% damage reduction for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 25
                tag 1
          , To Self $ apply 1 [Reduce [All] Percent 50]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Trumpet"
        , Skill.desc      = "Kurotsuchi cups her hand to her mouth and expels a jet of water, dealing 20 damage to an enemy. If the target was damaged by Lava Quicklime last turn, their physical and chakra skills are stunned for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                whenM (targetHas "Lava Quicklime") $
                    apply 1 [Stun Physical, Stun Chakra]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Falling Earth Spears"
        , Skill.desc      = "Spikes of stone and mud erupt from the ground, dealing 15 damage to all enemies and making them invulnerable to each other."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                damage 15
                apply 1 [Alone]
          ]
        }
      ]
    , [ invuln "Dodge" "Kurotsuchi" [Physical] ]
    ]
  , Character
    "Kitsuchi"
    "A jōnin from the Hidden Stone Village, Kitsuchi is the Third Tsuchikage's son and Kurotsuchi's father. He commands the Allied Shinobi Forces Second Division, a responsibility he takes with the utmost seriousness."
    [StoneVillage, AlliedForces, Jonin, Earth]
    [ [ Skill.new
        { Skill.name      = "Rock Fist"
        , Skill.desc      = "A massive stone hand punches an enemy, dealing 35 damage and disabling the countering and reflecting effects of their skills for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 35
                apply 1 [ Disable Counters
                        , Disable $ Only Reflect
                        , Disable $ Any ReflectAll
                        ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Erupt"
        , Skill.desc      = "A mountain bursts from the ground under Kitsuchi's enemies, dealing 10 damage to them and providing him with 20% damage reduction for 1 turn. For 1 turn, stuns, disabling effects, counters, and reflects applied by enemies will last 1 fewer turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ apply 1 [Reduce [All] Percent 20]
          , To Enemies do
                damage 10
                apply 1 [ Throttle 1 Counters
                        , Throttle 1 $ Only Reflect
                        , Throttle 1 $ Any ReflectAll
                        , Throttle 1 Stuns
                        ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sandwiching Mountain"
        , Skill.desc      = "Two rock formations slam into an enemy from either side, dealing 45 damage to them and stunning their physical and mental skills for 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                damage 45
                apply 2 [Stun Physical, Stun Mental]
          ]
        }
      ]
    , [ invuln "Rock Shelter" "Kitsuchi" [Physical] ]
    ]
  , Character
    "Ittan"
    "A chūnin from the Hidden Stone Village, Ittan is battle-hardened and level-headed. By reshaping the terrain, Ittan turns the battlefield to his advantage."
    [StoneVillage, AlliedForces, Chunin, Earth]
    [ [ Skill.new
        { Skill.name      = "Battlefield Trenches"
        , Skill.desc      = "By raising and lowering ground levels, Ittan alters the battlefield in his favor. For 2 turns, all enemies receive 20% more damage and Ittan gains 15 points of damage reduction."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 2 [Reduce [All] Flat 15]
          , To Enemies $ apply 2 [Bleed [All] Percent 20]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mobile Core"
        , Skill.desc      = "Ittan disrupts the ground under an enemy, dealing 30 damage to them and weakening their damage by 10 for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 30
                apply 1 [Weaken [All] Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Earth Dome"
        , Skill.desc      = "A shield of rock protects Ittan and one of his allies, making them invulnerable to ranged skills for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ apply 1 [Invulnerable Ranged]
          , To Ally $ apply 1 [Invulnerable Ranged]
          ]
        }
      ]
    , [ invuln "Trench Defense" "Ittan" [Physical] ]
    ]
  , Character
    "C"
    "A jōnin from the Hidden Cloud Village, C is one of the Raikage's bodyguards. Reliable and dutiful, C supports his allies with healing and sensing."
    [CloudVillage, AlliedForces, Jonin, Sensor, Lightning, Yin]
    [ [ Skill.new
        { Skill.name      = "Sensory Technique"
        , Skill.desc      = "C strikes a random enemy while detecting the flow of chakra, dealing 20 damage to them. If an enemy uses a skill on C next turn, he will become invulnerable for 1 turn."
        , Skill.classes   = [Mental, Nonstacking, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To REnemy $ damage 20
          , To Self $ trap 1 (OnHarmed All) $ apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Healing Technique"
        , Skill.desc      = "C restores 25 health to himself or an ally."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Ally $ heal 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flash Pillar"
        , Skill.desc      = "A flash of lightning blinds and disorients an enemy, dealing 35 damage to them and making them invulnerable to allies for 1 turn."
        , Skill.classes   = [Bane, Mental, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 35
                apply 1 [Alone]
          ]
        }
      ]
    , [ invuln "Parry" "C" [Physical] ]
    ]
  , Character
    "Atsui"
    "A chūnin from the Hidden Cloud Village, Atsui is a hot-headed hotshot whose favorite word is 'hot' and whose name literally means 'Hot'. A staggeringly complex character with hidden depths, Atsui's skills are as diverse as his multifaceted personality."
    [CloudVillage, AlliedForces, Chunin, Fire]
    [ [ Skill.new
        { Skill.name      = "Burning Blade"
        , Skill.desc      = "Fire envelops Atsui's sword and surrounds him, providing him with 10 points of damage reduction for 3 turns. While active, enemies who use skills on Atsui will take 10 affliction damage."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                apply 3 [Reduce [All] Flat 10]
                trapFrom 3 (OnHarmed All) $ afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fire Wall"
        , Skill.desc      = "Fire erupts around Atsui's enemies. Next turn, enemies who use skills will take 20 affliction damage. Costs 1 ninjutsu chakra during [Burning Blade]."
        , Skill.classes   = [Bane, Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ trap 1 (OnAction All) $ afflict 20 ]
        , Skill.changes   =
            changeWith "Burning Blade" \x -> x { Skill.cost = [Nin] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flame Slice"
        , Skill.desc      = "With a slash of his blade, Atsui creates an arc of flame that deals 25 piercing damage to an enemy. Deals 10 additional damage during [Burning Blade]."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` userHas "Burning Blade"
                damage (25 + bonus)
          ]
        }
      ]
    , [ invuln "Parry" "Atsui" [Physical] ]
    ]
  , Character
    "Omoi"
    "A chūnin from the Hidden Cloud Village, Omoi has an overactive imagination that feeds into his pessimism. A master swordsman, Omoi is quick to spot vulnerabilities in his opponents' defenses."
    [CloudVillage, AlliedForces, Chunin, Lightning]
    [ [ Skill.new
        { Skill.name      = "Back Slice"
        , Skill.desc      = "Omoi sets up a feint technique, preparing to spin and catch his enemies off-balance. Next turn, enemies who use a skill on Omoi will be countered and receive 20 piercing damage. Once used, this skill becomes [Crescent Moon Slice][t][r]."
        , Skill.classes   = [Physical, Melee, Invisible]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Self do
                trapFrom 1 (CounterAll All) $ pierce 20
                hide 0 [Alternate "Back Slice" "Crescent Moon Slice"]
          ]
        }
      , Skill.new
        { Skill.name      = "Crescent Moon Slice"
        , Skill.desc      = "Following up his reverse attack with a forward slash, Omoi prevents an enemy from reducing damage or becoming invulnerable for 1 turn and deals 35 piercing damage. Once used, this skill becomes [Back Slice][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Expose]
                pierce 35
          , To Self $ remove "back slice"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Thunderbolt"
        , Skill.desc      = "Omoi channels electricity into an enemy, dealing 25 damage and preventing them from affecting their allies for 1 turn."
        , Skill.classes   = [Chakra, Melee, Bane]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 25
                apply 1 [BlockAllies]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Paper Bomb"
        , Skill.desc      = "Omoi sets an explosive tag as a trap near himself or an ally. The first enemy to use a skill on the target will take 20 damage. This skill stacks."
        , Skill.classes   = [Physical, Ranged, Invisible, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Ally do
                addStack
                targetSlot <- target slot
                trap 0 (OnHarmed All) do
                    removeTrap "Paper Bomb"
                    remove "Paper Bomb"
                trapFrom 0 (OnHarmed All) do
                    stacks <- withTarget targetSlot $ targetStacks "Paper Bomb"
                    damage (20 * stacks)
          ]
        }
      ]
    , [ invuln "Parry" "Omoi" [Physical] ]
    ]
  , Character
    "Dodai"
    "A jōnin from the Hidden Cloud Village, Dodai is an impassive utilitarian with decades of experience as a sensor. He possesses the unusual ability to create rubbery lava by combining fire and earth chakra."
    [CloudVillage, AlliedForces, AlliedForces, Jonin, Fire, Earth]
    [ [ Skill.new
        { Skill.name      = "Rubber Wall"
        , Skill.desc      = "A force-absorbing barrier springs up in front of Dodai's team, reducing damage to them by 20% for 3 turns and causing them to ignore stuns and disabling effects."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Allies $ apply 3 [Reduce [All] Percent 20, Focus] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sensory Technique"
        , Skill.desc      = "Dodai scans an enemy's psyche for weaknesses, dealing 20 piercing damage and disabling the stuns and disabling effects of their skills for 1 turn. If the target was hit by [Rubber Sphere and Rope] within the past 4 turns, their physical and chakra skills are stunned."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 20
                has <- targetHas "Rubber Sphere and Rope"
                if has then
                    apply 1 [Disable Stuns, Stun Physical, Stun Chakra]
                else
                    apply 1 [Disable Stuns]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rubber Sphere and Rope"
        , Skill.desc      = "Dodai lobs a huge rubber ball at an enemy, dealing 35 damage. With the enemy distracted, he quickly pulls a random nearby ally out of harm's way, making them invulnerable for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 35
                tag 4
          , To RXAlly $ apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ invuln "Dodge" "Dodai" [Physical] ]
    ]
  , Character
    "Darui"
    "A jōnin of the Hidden Cloud Village, Darui is the fourth Raikage's right hand. Despite his laid-back attitude and characteristic slouch, he fights with incredible speed and ferocity."
    [CloudVillage, AlliedForces, Jonin, Lightning, Water, Wind]
    [ [ Skill.new
        { Skill.name      = "Laser Circus"
        , Skill.desc      = "Energy beams shoot out of Darui's hand, stunning an enemy's mental and ranged skills and dealing 20 piercing damage to all other enemies. Deals 5 additional damage to other enemies affected by [Water Wall]."
        , Skill.classes   = [Bane, Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun Mental, Stun Ranged]
          , To XEnemies do
                bonus <- 5 `bonusIf` targetHas "Water Wall"
                pierce (20 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Wall"
        , Skill.desc      = "Darui establishes a barrier of electrified water, dealing 5 affliction damage to all enemies and marking them for 1 turn."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                afflict 5
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Black Panther"
        , Skill.desc      = "Using a prized technique that the third Raikage passed down to him alone, Darui manifests an elemental made of black lightning that deals 45 piercing damage to an enemy. Deals 5 additional damage if the target is affected by [Water Wall]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` targetHas "Water Wall"
                pierce (45 + bonus)
          ]
        }
      ]
    , [ invuln "Block" "Darui" [Physical] ]
    ]
  , Character
    "Ao"
    "A jōnin from the Hidden Mist Village, Ao is a veteran with a proclivity for body modification. His expertise as a sensor and tracker is aided by the Byakugan he stole from a defeated Hyūga."
    [MistVillage, AlliedForces, Jonin, Sensor, Water, Yin]
    [ [ Skill.new
        { Skill.name      = "Byakugan"
        , Skill.desc      = "Using the power of the eye he stole from a Hyūga, Ao gains 5 points of damage reduction for 3 turns. While active, enemies who use non-mental skills on Ao will take 10 damage."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                apply 3 [Reduce [All] Flat 5]
                trapFrom 3 (OnHarmed NonMental) $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sensory Technique"
        , Skill.desc      = "Ao scans an enemy's psyche for weaknesses, preventing them from reducing damage or becoming invulnerable for 1 turn and dealing 25 damage."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Expose]
                damage 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Barrier Talisman"
        , Skill.desc      = "Ao's earrings shift to form a barrier over targeted body parts. Next turn, enemies who use non-mental skills on Ao will be countered, and their skills will cost 1 additional arbitrary chakra for 1 turn."
        , Skill.classes   = [Chakra, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ trapFrom 1 (CounterAll All) $ apply 1 [Exhaust [All]] ]
        }
      ]
    , [ invuln "Block" "Ao" [Physical] ]
    ]
  , Character
    "Chōjūrō"
    "One of the Seven Swordsmen of the Mist, Chōjūrō is a jōnin who serves as the Mizukage's retainer. Wielding the legendary twin-sword Hiramekarei, Chōjūrō overpowers the defenses of his enemies with its battering-ram chakra blasts."
    [MistVillage, AlliedForces, SevenSwordsmen, Jonin, Water]
    [ [ Skill.new
        { Skill.name      = "Hiramekarei Longsword"
        , Skill.desc      = "Chōjūrō combines the blades of his sword into a long, sweeping weapon, and uses it to deal 10 piercing damage to all enemies."
        , Skill.classes   = [Bypassing, Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ pierce 10 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hiramekarei Hammer"
        , Skill.desc      = "Chōjūrō shapes the chakra of his sword into a sledgehammer, and uses it to demolish an enemy's destructible defense and his own destructible barrier, then deals 50 piercing damage to the target."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                pierce 50
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hiramekarei Twinswords"
        , Skill.desc      = "Chōjūrō separates his sword into two weapons and sends out a volley of bone-mutilating needles that disrupt chakra pathways. Enemies who use physical skills on Chōjūrō next turn will be countered and will take 20 piercing damage."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ trapFrom 1 (CounterAll Physical) $ pierce 20 ]
        }
      ]
    , [ invuln "Dodge " "Chōjūrō" [Physical] ]
    ]
  ]

