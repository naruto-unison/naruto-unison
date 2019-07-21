{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Adults (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Kakashi Hatake"
    "For most of his life, Kakashi has avoided using Kamui—his Sharingan's ultimate ability—unless absolutely necessary, due to the mental and physical strain. Those days are over. With years of practice and refinement behind him, Kakashi can now rely on Kamui's dimensional warping to torture his enemies and make his allies intangible."
    [ [ Skill.new
        { Skill.name      = "Lightning Beast Fang"
        , Skill.desc      = "Kakashi creates a lightning hound out of his Lightning Blade, which deals 25 piercing damage to an enemy. If the target is damaged, they will be stunned for 1 turn. During the next turn, this skill becomes [Lightning Blade Finisher][n][r]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ p Enemy do
                trap' 1 (OnDamaged All) $ apply 1 [Stun All]
                pierce 25
          ,  p Self $ vary' 1 "Lightning Beast Fang" "Lightning Blade Finisher"
          ]
        }
      , Skill.new
        { Skill.name      = "Lightning Blade Finisher"
        , Skill.desc      = "Deals 35 piercing damage to an enemy. Deals 15 additional damage if the target is affected by [Lightning Beast Fang]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ p Enemy do
              bonus <- 15 `bonusIf` targetHas "Lightning Beast Fang"
              pierce (35 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kamui"
        , Skill.desc      = "If used on an enemy, deals 45 piercing damage to them, increases their cooldowns by 1 turn, and increases the costs of their skills by 1 random chakra. If used on an ally, cures them of enemy effects and makes them invulnerable for 1 turn."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = [Blood, Gen]
        , Skill.effects   =
          [ p Enemy $ pierce 45
          , p XAlly do
                cureAll
                apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Team Tactics"
        , Skill.desc      = "For 3 turns, the cooldowns of Kakashi's allies are decreased by 1. While active, the first enemy skill used will replace this skill for 1 turn. Kakashi's copy of the skill has no chakra cost and ends when this skill reverts."
        , Skill.classes   = [Mental, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p XAllies $ apply 3 [Snare (-1)]
          , p Enemies $ apply' "Team Tactics " 3 [Replace 1 All 2 True]
          ]
        }
      ]
    , [ invuln "Shadow Clone" "Kakashi" [Chakra] ]
    ] []
  , Character
    "Asuma Sarutobi"
    "Having somehow managed to avoid lung cancer, Asuma remains the leader of Team 10. Using techniques he learned from the Fire Temple, he hinders his opponents and instantly executes weak enemies."
    [ [ Skill.new
        { Skill.name      = "Thousand Hand Strike"
        , Skill.desc      = "Asuma summons Kannon, the Fire Temple's patron spirit, which provides him with 40 permanent destructible defense and deals 25 damage to an enemy. Next turn, this skill becomes [Kannon Strike][r]. When [Kannon Strike] ends, this skill is disabled for 1 turn."
        , Skill.require   = HasI (-1) "Overheating"
        , Skill.classes   = [Physical, Melee, Summon]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ p Enemy $ damage 25
          , p Self do
                defend 0 40
                vary' 1 "Thousand Hand Strike" "Kannon Strike"
          ]
        }
      , Skill.new
        { Skill.name      = "Kannon Strike"
        , Skill.desc      = "Deals 20 damage to an enemy. This skill remains [Kannon Strike] for another turn."
        , Skill.classes   = [Physical, Melee, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Enemy $ damage 20
          , p Self do
                tag' "Overheating" 2
                vary' 1 "Thousand Hand Strike" "Kannon Strike"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Burning Ash"
        , Skill.desc      = "Asuma continually exhales a cloud of combustible ash upon his enemies, increasing the cooldowns of their skills by 1 turn. While active, this skill becomes [Burning Ash: Ignite][b]."
        , Skill.classes   = [Bane, Ranged, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.channel   = Action 0
        , Skill.start     =
          [ p Self $ vary "Burning Ash" "Burning Ash: Ignite"]
        , Skill.effects   =
          [ p Enemies $ apply 0 [Snare 1] ]
        }
      , Skill.new
        { Skill.name      = "Burning Ash: Ignite"
        , Skill.desc      = "Asuma strikes a piece of flint between his teeth, producing a spark that sets fire to his piles of ash and burns them away. The fire deals 10 affliction damage to each enemy per stack of [Burning Ash] on them."
        , Skill.classes   = [Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ p Enemies do
                stacks <- targetStacks "Burning Ash"
                afflict (10 * stacks)
          , p Self do
                cancelChannel "Burning Ash"
                everyone $ remove "Burning Ash"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Decapitate"
        , Skill.desc      = "Bypassing invulnerability, Asuma mercilessly slaughters an enemy whose health is at or below 25."
        , Skill.classes   = [Physical, Melee, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                health <- target health
                when (health <= 25) kill
          ]
        }
      ]
    , [ invuln "Dodge" "Asuma" [Physical] ]
    ] []
  {-
  , Character
    "Zaji"
    "A chūnin from the Hidden Leaf Village, Zaji loves to boast about his strength and combat prowess. He doesn't actually have either, but he's a decent sensor. By warning his team of incoming attacks, he can protect them from both light and heavy damage."
    [ [ Skill.new
        { Skill.name      = "Chakra Sense"
        , Skill.desc      = "Zaji extends his senses over the battlefield and detects incoming attacks. For 2 turns, attacks that deal 25 baseline damage or lower will not injure Zaji or his allies."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   = [ p Allies $ apply 2 [Threshold 25 ] ]
        }
      ]
    ] []
  -}
{-
  , Character
    "Might Guy"
    "Over the past few years, Guy has learned restraint. By gradually opening his Gates in sequence, he avoids the risk of burning out before the battle is won."
    [ [ Skill.new
        { Skill.name      = "Nunchaku"
        , Skill.desc      = "Using his signature Twin Fangs weapons, Guy deals 10 damage to an enemy for 3 turns. While active, if an enemy uses a harmful physical skill on him, he will deal 10 damage to them. Deals 5 additional damage on the first turn per stack of [Single Gate Release]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.channel   = Action 3
        , Skill.start     = [ p Enemy $ perI "Single Gate Release" 5 damage 10 ]
        , Skill.effects   = [ p Self $  trapFrom 1 (OnHarmed Physical) $ damage 10
                    , p Enemy $ ifnotI "first" $ damage 10
                    ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fiery Kick"
        , Skill.desc      = "Guy slams his leg into an enemy, dealing 35 damage and weakening their damage by 20 for 1 turn. Deals 5 additional damage per stack of [Single Gate Release]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   = [ p Enemy $ perI "Single Gate Release" 5 damage 35
                           • apply 1 [Weaken All 20 ])]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Single Gate Release"
        , Skill.desc      = "Guy opens one of his internal Gates, losing 5 health and gaining 5 points of permanent damage reduction. "
        , Skill.classes   = [Mental, Unremovable]
        , Skill.effects   = [ p Self $ sacrifice 0 5 • apply 0 [Reduce All 5 ] ]
        }
      ]
    , [ invuln "Block" "Guy" [Physical] ]
    ] []-}
  , Character
    "Maki"
    "A jōnin from the Hidden Sand Village, Maki studied under Pakura and mourned her death greatly. As a member of the Allied Shinobi Forces Sealing Team, Maki must put aside her long-held grudge against the Hidden Stone Village for killing her teacher."
    [ [ Skill.new
        { Skill.name      = "Binding Cloth"
        , Skill.desc      = "Maki deploys a roll of cloth from within a seal and wraps it around herself, gaining 50% damage reduction for 1 turn. If an enemy uses a skill on Maki, the cloth wraps around them, stunning their physical and melee skills for 1 turn."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Self do
                apply 1 [Reduce All Percent 50]
                trapFrom 1 (OnHarmed All) $ apply 1 [Stun Physical, Stun Melee]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Humidified Cloth"
        , Skill.desc      = "Maki soaks a strip of cloth in steam and lashes out with it, dealing 20 piercing damage to an enemy and stunning their harmful skills for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ p Enemy do
                damage 20
                apply 1 [Stun Harmful]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Cloth Paralysis"
        , Skill.desc      = "Maki binds an enemy in rolls of cloth, stunning their chakra and ranged skills for 2 turns. While active, Melee skills deal 5 additional damage to the target."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy $ apply 2 [Stun Chakra, Stun Ranged, Bleed Melee Flat 5] ]
        }
      ]
    , [ invuln "Cloth Dome" "Maki" [Physical] ]
    ] []
  , Character
    "Akatsuchi"
    "A jōnin from the Hidden Rock Village, Akatsauchi is cheerful and excitable. He uses brute strength and rock golems to pummel his enemies to the ground."
    [ [ Skill.new
        { Skill.name      = "High-Speed Assault"
        , Skill.desc      = "Akatsuchi punches an enemy with all his might, dealing 25 damage. Costs 1 fewer random chakra during [Stone Golem]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   = [ p Enemy $ damage 25 ]
        , Skill.changes   = changeWith "Stone Golem" $ setCost [Tai]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Stone Golem"
        , Skill.desc      = "A golem of rock rampages across the battlefield, dealing 15 damage to all enemies for 2 turns and providing Akatsuki with 25% damage reduction."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ p Enemies $ damage 15
          , p Self    $ apply 1 [Reduce All Percent 25]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Devour"
        , Skill.desc      = "A stone golem attacks an enemy, dealing 15 damage and depleting 1 random chakra."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                deplete 1
                damage 15
          ]
        }
      ]
    , [ invuln "Dodge" "Akatsuchi" [Physical] ]
    ] []
  , Character
    "Kurotsuchi"
    "A jōnin from the Hidden Rock Village, Kurotsuchi is the Third Tsuchikage's granddaughter. Witty and self-assured, Kurotsuchi is famed for her unflinching resolve in the face of danger."
    [ [ Skill.new
        { Skill.name      = "Lava Quicklime"
        , Skill.desc      = "Kurotsuchi expels a mass of quicklime from her mouth, dealing 25 damage to an enemy and gaining 50% damage reduction for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 25
                tag 1
          , p Self $  apply 1 [Reduce All Percent 50]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Trumpet"
        , Skill.desc      = "Kurotsuchi cups her hand to her mouth and expels a jet of water, dealing 20 damage to an enemy. If the target was damaged by Lava Quicklime last turn, their physical and chakra skills are stunned for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ p Enemy do
                damage 20
                whenM (targetHas "Lava Quicklime") $
                    apply 1 [Stun Physical, Stun Chakra]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Falling Earth Spears"
        , Skill.desc      = "Spikes of stone and mud erupt from the ground, dealing 15 damage to all enemies and making them immune to effects from each other."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ p Enemies do
                damage 15
                apply 1 [Seal]
          ]
        }
      ]
    , [ invuln "Dodge" "Kurotsuchi" [Physical] ]
    ] []
  , Character
    "Ittan"
    "A chūnin from the Hidden Rock Village, Ittan is battle-hardened and level-headed. By reshaping the terrain, Ittan turns the battlefield to his advantage."
    [ [ Skill.new
        { Skill.name      = "Battlefield Trenches"
        , Skill.desc      = "By raising and lowering ground levels, Ittan alters the battlefield in his favor. For 2 turns, all enemies receive 20% more damage and Ittan gains 15 points of damage reduction."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self $ apply 2 [Reduce All Flat 15]
          , p Enemies $ apply 2 [Bleed All Percent 20]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mobile Core"
        , Skill.desc      = "Ittan disrupts the ground under an enemy, dealing 30 damage to them and weakening their damage by 10 for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 30
                apply 1 [Weaken All Flat 10]
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
          [ p Self $ apply 1 [Invulnerable Ranged]
          , p Ally $ apply 1 [Invulnerable Ranged]
          ]
        }
      ]
    , [ invuln "Trench Defense" "Ittan" [Physical] ]
    ] []
  , Character
    "Kitsuchi"
    "A jōnin from the Hidden Rock Village, Kitsuchi is the Third Tsuchikage's son and Kurotsuchi's father. He commands the Allied Shinobi Forces Second Division, a responsibility he takes with the utmost seriousness."
    [ [ Skill.new
        { Skill.name      = "Rock Fist"
        , Skill.desc      = "A massive stone hand punches an enemy, dealing 35 damage and preventing them from countering or reflecting skills for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 35
                apply 1 [Uncounter]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Erupt"
        , Skill.desc      = "A mountain bursts from the ground under Kitsuchi's enemies, dealing 10 damage to them and providing him with 20% damage reduction for 1 turn. For 1 turn, stuns, counters, and reflects applied by enemies will last 1 fewer turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Self $ apply 1 [Reduce All Percent 20]
          , p Enemies do
                damage 10
                apply 1 [ Throttle 1 $ Any Stun
                        , Throttle 1 $ Any Counter
                        , Throttle 1 $ Any CounterAll
                        , Throttle 1 $ Only Reflect
                        , Throttle 1 $ Only ReflectAll
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
        , Skill.channel   = Control 2
        , Skill.effects   =
          [ p Enemy do
                damage 45
                apply 2 [Stun Physical, Stun Mental]
          ]
        }
      ]
    , [ invuln "Rock Shelter" "Kitsuchi" [Physical] ]
    ] []
  , Character
    "C"
    "A jōnin from the Hidden Cloud Village, C is one of the Raikage's bodyguards. Reliable and dutiful, C supports his allies with healing and sensing."
    [ [ Skill.new
        { Skill.name      = "Sensory Technique"
        , Skill.desc      = "C strikes a random enemy while detecting the flow of chakra, dealing 20 damage to them. Next turn, if an enemy uses a skill on C, he will become invulnerable for 1 turn."
        , Skill.classes   = [Mental, Nonstacking, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p REnemy $ damage 20
          , p Self   $ trap 1 (OnHarmed All) $ apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mystical Palm Healing"
        , Skill.desc      = "C restores 25 health to himself or an ally."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ p Ally $ heal 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flash Pillar"
        , Skill.desc      = "A flash of lightning blinds and disorients an enemy, dealing 35 damage to them and making them immune to effects from allies."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 35
                apply 1 [Seal]
          ]
        }
      ]
    , [ invuln "Parry" "C" [Physical] ]
    ] []
  , Character
    "Atsui"
    "A chūnin from the Hidden Cloud Village, Atsui is a hot-headed hotshot whose favorite word is 'Hot' and whose name literally means 'Hot'. An incredibly complex character with hidden depths, Atsui's skills are as diverse as his multifaceted personality."
    [ [ Skill.new
        { Skill.name      = "Burning Blade"
        , Skill.desc      = "Fire envelops Atsui's sword and surrounds him, providing 10 points of damage reduction to him for 3 turns. While active, any enemy who uses a skill on Atsui will receive 10 affliction damage."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                apply 3 [Reduce All Flat 10]
                trapFrom 3 (OnHarmed All) $ afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fire Wall"
        , Skill.desc      = "Fire erupts around Atsui's enemies. Next turn, any enemy who uses a skill will receive 10 affliction damage. Costs 1 fewer random chakra during [Burning Blade]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies $ trap 1 (OnAction All) $ afflict 10 ]
        , Skill.changes   = changeWith "Burning Blade" $ setCost [Nin]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flame Slice"
        , Skill.desc      = "Atsui slashes at an enemy with his fiery blade, sending an arc of flame in their direction that deals 25 piercing damage."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ p Enemy $ damage 25 ]
        }
      ]
    , [ invuln "Parry" "Atsui" [Physical] ]
    ] []
  , Character
    "Tsunade"
    "Tsunade has become the fifth Hokage. Knowing the Hidden Leaf Village's fate depends on her, she holds nothing back. Even if one of her allies is on the verge of dying, she can keep them alive long enough for her healing to get them back on their feet."
    [ [ Skill.new
        { Skill.name      = "Heaven Spear Kick"
        , Skill.desc      = "Tsunade spears an enemy with her foot, dealing 20 piercing damage to them. If an ally is affected by [Healing Wave], their health cannot drop below 1 next turn. Spends a Seal if available to deal 20 additional damage and demolish the target's destructible defense and Tsunade's destructible barrier."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ p Enemy do
                has <- userHas "Strength of One Hundred Seal"
                when has demolishAll
                pierce (20 + if has then 20 else 0)
          , p Allies $ whenM (targetHas "Healing Wave") $ apply 1 [Endure]
          , p Self do
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
          [ p XAlly do
                has <- userHas "Strength of One Hundred Seal"
                heal (20 + if has then 10 else 0)
                apply (if has then (-3) else (-2)) [Heal 10]
          , p Self do
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
          [ p Self do
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
          [ p Self do
                heal 50
                gain [Rand, Rand]
                vary "Strength of One Hundred Seal" baseVariant
                remove "Strength of One Hundred Seal"
          ]
        }
      ]
    , [ invuln "Block" "Tsunade" [Physical] ]
    ] []
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
          [ p Allies  $ defend 0 10
          , p Enemies $ damage 10
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
          [ p XAlly $ apply 2 [Reduce All Flat 10, AntiCounter] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Atomic Dismantling"
        , Skill.desc      = "The atomic bonds within an enemy shatter, dealing 20 piercing damage to them and permanently increasing the damage of this skill by 10."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ p Enemy do
                stacks <- userStacks "Atomic Dismantling"
                pierce (20 + 10 * stacks)
          , p Self addStack
          ]
        }
      ]
    , [ invuln "Flight" "Ōnoki" [Chakra] ]
    ] []
  , Character
    "Mei Terumi"
    "The third Mizukage of the Hidden Mist Village, Mei works tirelessly to help her village overcome its dark history and become a place of kindness and prosperity. Her corrosive attacks eat away at the defenses of her "
    [ [ Skill.new
        { Skill.name      = "Solid Fog"
        , Skill.desc      = "Mei exhales a cloud of acid mist, dealing 15 affliction damage to an enemy for 3 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy $ afflict 15 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Bomb"
        , Skill.desc      = "Water floods the battlefield, dealing 20 piercing damage to all enemies and preventing them from reducing damage or becoming invulnerable for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies do
                pierce 20
                apply 1 [Expose]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lava Monster"
        , Skill.desc      = "Mei spits a stream of hot lava, dealing 10 affliction damage to all enemies and removing 20 destructible defense from them for 3 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Enemies do
              demolish 20
              afflict 10
          ]
        }
      ]
    , [ invuln "Flee" "Mei" [Physical] ]
    ] []
  ]
