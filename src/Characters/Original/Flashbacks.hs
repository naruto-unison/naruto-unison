{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Original.Flashbacks (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Konohamaru Sarutobi"
    "The overly bold grandson of the third Hokage, Konohamaru has a knack for getting into trouble and requiring others to bail him out. His usefulness in battle depends on how willing his teammates are to babysit him."
    [ [ Skill.new
        { Skill.name      = "Refocus"
        , Skill.desc      = "Konohamaru tries his best to concentrate on the fight. For 3 turns, effects from his allies on him are twice as powerful. While active, this skill becomes [Unsexy Technique][n]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                vary' 3 "Refocus" "Unsexy Technique"
                tag 3
                allies . self $ hide 3 [Boost 2]
          ]
        }
      , Skill.new
        { Skill.name      = "Unsexy Technique"
        , Skill.desc      = "Konohamaru distracts an enemy with his modified version of the transformation technique he learned from Naruto. For 1 turn, the target is immune to effects from their allies and cannot reduce damage, become invulnerable, counter, or reflect."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 1 [Seal, Expose, Uncounter] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Throw a Fit"
        , Skill.desc      = "Konohamaru freaks out and punches wildly at an enemy, dealing 10 damage to them for 3 turns. Deals 5 additional damage per skill affecting Konohamaru from his allies."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ To Enemy do
              helpful <- user numHelpful
              damage (10 + 5 * helpful)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Throw a Shuriken"
        , Skill.desc      = "Konohamaru flings a shuriken almost too big for his hands at an enemy, dealing 10 damage and 10 additional damage per skill affecting Konohamaru from his allies."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
              helpful <- user numHelpful
              damage (10 + 10 * helpful)
          ]
        }
      ]
    , [ invuln "Hide?" "Konohamaru" [Mental] ]
    ] []
  , Character
    "Hiashi Hyūga"
    "A jōnin from the Hidden Leaf Village and father to Hinata and Hanabi, Hiashi does not tolerate weakness or failure. All of the Hyūga clan's secret techniques have been passed down to him, and he wields them with unmatched expertise."
    [ [ Skill.new
        { Skill.name      = "Gentle Fist"
        , Skill.desc      = "Hiashi slams an enemy, dealing 20 damage and depleting 1 random chakra. Next turn, he repeats the attack on a random enemy."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.start     =
          [ To Self flag
          , To Enemy do
                deplete 1
                damage 20
          ]
        , Skill.effects   =
          [ To REnemy $ unlessM (userHas "Gentle Fist") do
                damage 20
                deplete 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Palm Rotation"
        , Skill.desc      = "Hiashi spins toward an enemy, becoming invulnerable for 2 turns and dealing 15 damage to the target and 10 to all other enemies each turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 2
        , Skill.start     =
          [ To Self     $ apply 1 [Invulnerable All]
          , To Enemy    $ damage 15
          , To XEnemies $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Air Palm Wall"
        , Skill.desc      = "Hiashi prepares to blast an enemy's attack back. The first harmful skill used on him or his allies next turn will be reflected."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies $ trap (-1) OnReflectAll $
                everyone $ removeTrap "Eight Trigrams Air Palm Wall"
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Hiashi" [Mental] ]
    ] []
  , Character
    "Chōza Akimichi"
    "A jōnin from the Hidden Leaf Village and Chōji's father, Chōza instills confidence in his comrades with his bravery and wisdom. Never one to back down from a fight, he defends his allies and forces the attention of his enemies to himself."
    [ [ Skill.new
        { Skill.name      = "Chain Bind"
        , Skill.desc      = "Chōza slows an enemy, dealing 5 damage and weakening their physical and chakra damage by 10 for 1 turn. Chōza's team gains 5 permanent destructible defense."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 5
                apply 1 [Weaken Physical Flat 10, Weaken Chakra Flat 10]
          , To Allies $ defend 0 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Human Boulder"
        , Skill.desc      = "Chōza transforms into a rolling juggernaut. For 3 turns, he deals 15 damage to an enemy and provides 10 destructible defense to himself and his allies for 1 turn. Each turn, if the target is affected by [Chain Bind], it lasts 1 additional turn on them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ To Allies $ defend 1 10
          ,  To Enemy do
                damage 15
                whenM (targetHas "Chain Bind") $
                    apply' "Chain Bind" 1 [ Weaken Physical Flat 10
                                          , Weaken Chakra Flat 10
                                          ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Partial Expansion"
        , Skill.desc      = "If used on an enemy, the next harmful non-mental skill they use will be countered. If used on an ally, the next harmful non-mental skill used on them will be countered. The person countered will receive 10 damage, bypassing invulnerability."
        , Skill.classes   = [Physical, Melee, Single, Invisible, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To XAlly $ trapFrom 0 (Counter NonMental) $ damage 10
          , To Enemy $ trap 0 (Countered NonMental) $ damage 10
          ]
        }
      ]
    , [ invuln "Block" "Chōza" [Physical] ]
    ] []
  , Character
    "Inoichi Yamanaka"
    "A jōnin from the Hidden Leaf Village and Ino's father, Inoichi can solve practically any dilemma with his analytical perspective. Under his watchful gaze, every move made by his enemies only adds to his strength."
    [ [ Skill.new
        { Skill.name      = "Psycho Mind Transmission"
        , Skill.desc      = "Inoichi invades the mind of an enemy, dealing 20 damage to them for 2 turns. While active, the target cannot use counter or reflect skills."
        , Skill.classes   = [Mental, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.channel   = Control 2
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Uncounter]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sensory Radar"
        , Skill.desc      = "Inoichi steps back and focuses on the tide of battle. Each time an enemy uses a harmful skill, Inoichi will recover 10 health and gain a stack of [Sensory Radar]. While active, this skill becomes [Sensory Radar: Collate][r]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Self $ vary "Sensory Radar" "Sensory Radar: Collate"
          , To Enemies $ trap 0 OnHarm $ self do
                heal 10
                addStack
          ]
        }
      , Skill.new
        { Skill.name      = "Sensory Radar: Collate"
        , Skill.desc      = "Inoichi compiles all the information he has gathered and puts it to use. For every stack of [Sensory Radar], he gains 1 random chakra. Ends [Sensory Radar]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemies $ removeTrap "Sensory Radar"
          ,  To Self do
                vary "Sensory Radar" baseVariant
                stacks <- userStacks "Sensory Radar"
                gain $ replicate stacks Rand
                remove "Sensory Radar"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mental Invasion"
        , Skill.desc      = "Inoichi preys on an enemy's weaknesses. For 4 turns, all invulnerability skills used by the target will have their duration reduced by 1 turn. While active, anyone who uses a harmful mental skill on the target will become invulnerable for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
              apply 4 [Throttle 1 $ Any Invulnerable]
              trapFrom 4 (OnHarmed Mental) $ apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ invuln "Mobile Barrier" "Inoichi" [Chakra] ]
    ] []
  , Character
    "Kushina Uzumaki"
    "Known as the Red-Hot Habanero for her fiery hair and fierce temper, Naruto's mother possesses exceptional chakra necessary to become the nine-tailed fox's jinchūriki. Kushina specializes in unique sealing techniques that bind and incapacitate her enemies."
    [ [ Skill.new
        { Skill.name      = "Double Tetragram Seal"
        , Skill.desc      = "Kushina seals away an enemy's power, dealing 15 piercing damage, stunning them for 1 turn, depleting 1 random chakra, and permanently weakening their damage by 5."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                deplete 1
                damage 15
                apply 1 [Stun All]
                apply 0 [Weaken All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Life Link"
        , Skill.desc      = "Kushina binds her life-force to that of an enemy. For 4 turns, if either dies, the other will die as well. Effect cannot be avoided, prevented, or removed."
        , Skill.classes   = [Mental, Ranged, Bypassing, Unremovable, Uncounterable, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Enemy do
                tag 4
                trap 4 OnDeath $ self kill
                self $ trap 4 OnDeath $
                    everyone $ whenM (targetHas "Life Link") killHard
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Adamantine Sealing Chains"
        , Skill.desc      = "A cage of chain-shaped chakra seals an enemy, removing the effects of helpful skills from them and stunning them for 2 turns. While active, the target is immune to effects from allies and invulnerable."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood, Gen]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                purge
                apply 2 [Stun All, Invulnerable All, Seal]
          ]
        }
      ]
    , [ invuln "Adamantine Covering Chains" "Kushina" [Chakra] ]
    ] []
  , Character
    "Minato Namikaze"
    "Known as the Yellow Flash for his incredible speed and mastery of space-time techniques, Naruto's father is a jōnin squad leader from the Hidden Leaf Village. Minato fights using unique kunai that allow him to teleport arround the battlefield."
    [ [ Skill.new
        { Skill.name      = "Flying Raijen"
        , Skill.desc      = "Minato teleports to a target, becoming invulnerable for 1 turn. If he teleports to an enemy, he deals 30 damage. If he teleports to an ally, the ally becomes invulnerable for 1 turn."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
        [ To Self $ apply 1 [Invulnerable All]
        , To XAllies $
              whenM (targetHas "Space-Time Marking") $
                  apply 1 [Invulnerable All]
        , To Enemies $ whenM (targetHas "Space-Time Marking") $ damage 30
        , To XAlly do
              apply 1 [Invulnerable All]
              whenM (userHas "Space-Time Marking") $ tag' "Space-Time Marking" 1
        , To Enemy do
              damage 30
              whenM (userHas "Space-Time Marking") $ tag' "Space-Time Marking" 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sensory Technique"
        , Skill.desc      = "Minato's senses expand to cover the battlefield, preventing enemies from reducing damage or becoming invulnerable for 2 turns. Each turn, Minato gains 1 random chakra."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 3
        , Skill.channel   = Control 2
        , Skill.effects   =
          [ To Enemies $ apply 1 [Expose]
          , To Self    $ gain [Rand]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Space-Time Marking"
        , Skill.desc      = "For 3 turns, [Flying Raijen] marks its target for 1 turn. Using [Flying Raijen] causes marked allies to become invulnerable for 1 turn and deals 30 damage to marked enemies, bypassing invulnerability."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen, Nin]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ To Self $ tag 3 ]
        }
      ]
    , [ invuln "Flying Light" "Minato" [Physical] ]
    ] []
  , Character
    "Yondaime Minato"
    "Now the fourth Hokage, Minato has been shaped by his responsibilities into a thoughtful and strategic leader. With his space-time jutsu, he redirects the attacks of his enemies and effortlessly passes through their defenses."
    [ [ Skill.new
        { Skill.name      = "Space-Time Marking"
        , Skill.desc      = "Minato opportunistically marks targets to use as teleport destinations for avoiding attacks. Next turn, allies and enemies who do not use a skill will be marked by this skill for 4 turns. Minato gains 5 points of damage reduction for each marked target. This skill stacks."
        , Skill.classes   = [Physical, Ranged, InvisibleTraps]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAllies $ delay 0 $ trap 1 OnNoAction do
                apply' "Space-Time Marking " 3 []
                self $ hide 4 [Reduce All Flat 5]
          , To Enemies $ trap (-1) OnNoAction do
                apply' "Space-Time Marking " (-4) []
                self $ hide 4 [Reduce All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Teleportation Barrier"
        , Skill.desc      = "Space warps around Minato or one of his allies. The first harmful skill used on the target next turn will be reflected."
        , Skill.classes   = [Chakra, Ranged, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Ally $ apply 1 [Reflect] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rasengan"
        , Skill.desc      = "Minato teleports behind an enemy and slams an orb of chakra into them, dealing 20 damage. In quick succession, he teleports between all enemies affected by [Space-Time Marking], dealing 20 damage for every stack of [Space-Time Marking] on them."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Enemies do
                stacks <- targetStacks "Space-Time Marking"
                damage (20 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Round-Robin Raijen"
        , Skill.desc      = "Minato and allies affected by [Space-Time Marking] become invulnerable for 1 turn."
        , Skill.classes   = [Chakra]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                apply 1 [Invulnerable All]
                allies . whenM (targetHas "Space-Time Marking") $
                    apply 1 [Invulnerable All]
          ]
        }
      ]
    ] []
  , let kannon = changeWith "Veritable 1000-Armed Kannon" \_ skill ->
                 skill { Skill.channel = Action 3, Skill.cost = [Blood] }
    in Character
    "Hashirama Senju"
    "The founder and first Hokage of the Hidden Leaf Village, Hashirama is headstrong and enthusiastic. He believes with all his heart that communities should behave as families, taking care of each other and protecting their children from the cruelties of war. Due to a unique genetic mutation, Hashirama is able shape wood into defensive barriers and constructs."
    [ [ Skill.new
        { Skill.name      = "Wooden Dragon"
        , Skill.desc      = "A vampiric dragon made of wood drains chakra from Hashirama's enemies, making him invulnerable to chakra skills for 2 turns. While active, Hashirama absorbs 1 random chakra from his enemies each turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ To Self   $ apply 1 [Invulnerable Chakra]
          , To REnemy $ absorb 1
          ]
        , Skill.changes   = kannon
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wood Golem"
        , Skill.desc      = "A giant humanoid statue attacks an enemy for 2 turns, dealing 20 damage each turn. While active, Hashirama is invulnerable to physical skills."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self  $ apply 1 [Invulnerable Physical]
          ]
        , Skill.changes   = kannon
        }
      ]
    , [ Skill.new
        { Skill.name      = "Veritable 1000-Armed Kannon"
        , Skill.desc      = "A titanic many-handed Buddha statue looms over the battlefield, providing 30 permanent destructible defense to Hashirama and his allies. For the next 3 turns, [Wooden Dragon] and [Wood Golem] cost 1 fewer random chakra and last 1 additional turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Allies $ defend 0 30
          , To Self   $ tag 3
          ]
        }
      ]
    , [ invuln "Foresight" "Hashirama" [Mental] ]
    ] []
  , Character
    "Young Kakashi"
    "A member of Team Minato, Kakashi is the thirteen-year-old son of the legendary White Fang. His early ninjutsu and borrowed sharingan make him the equal of any adult he faces."
    [ [ Skill.new
        { Skill.name      = "White Light Blade"
        , Skill.desc      = "Kakashi deals 20 piercing damage to an enemy with his sword. For 1 turn, the target's non-affliction damage is weakened by 5 and Kakashi's damage is increased by 5."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Weaken All Flat 5]
                whenM (userHas "Sharingan Stun") $ apply 1 [Stun All]
          , To Self $ apply 1 [Strengthen All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amateur Lightning Blade"
        , Skill.desc      = "Using an early form of his signature technique, Kakashi deals 20 piercing damage to one enemy. For 1 turn, the target's non-affliction damage is weakened by 5 and Kakashi's damage is increased by 5."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Weaken All Flat 5]
                whenM (userHas "Sharingan Stun") $ apply 1 [Stun All]
          , To Self $ apply 1 [Strengthen All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Kakashi anticipates an opponent's moves for 2 turns. If they use a skill that depletes or absorbs chakra, Kakashi gains 1 random chakra. If they use a skill that stuns, Kakashi's skills will stun next turn. If they use a skill that damages, Kakashi's damage will be increased by 10 during the next turn."
        , Skill.classes   = [Mental, Ranged, InvisibleTraps]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                trap 1 OnChakra $ self $ gain [Rand]
                trap 1 OnStun   $ self $ gain [Rand]
                trap 1 OnDamage $ self $ apply 1 [Strengthen All Flat 10]
          ]
        }
      ]
    , [ invuln "Parry" "Kakashi" [Physical] ]
    ] []
  , Character
      "Rin Nohara"
      "A chūnin on Team Minato, Rin is a quiet but strong-willed medical-nin. Her priority is always healing her teammates, though she can also defend herself with traps if necessary."
      [ [ Skill.new
        { Skill.name      = "Pit Trap"
        , Skill.desc      = "An enemy falls into a pit and is trapped there for 1 turn. At the end of their turn, the target takes 15 piercing damage. If they used a skill that turn, they take 15 additional damage. While active, Rin gains 15 points of damage reduction."
        , Skill.classes   = [Invisible, Bypassing]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Self $ apply 1 [Reduce All Flat 15]
          , To Enemy do
                trap (-1) (OnAction All) flag
                delay (-1) do
                    bonus <- 15 `bonusIf` targetHas "Pit Trap"
                    pierce (15 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mystical Palm Healing"
        , Skill.desc      = "Rin restores 25 health to herself or an ally and cures the target of enemy effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Ally do
                cureAll
                heal 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Medical Kit"
        , Skill.desc      = "Rin or one of her allies uses her medical kit for 3 turns, restoring 10 health each turn and strengthening their healing skills by 10 points."
        , Skill.classes   = [Physical, Unremovable]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Ally $ apply 3 [Bless 10, Heal 10] ]
        }
      ]
    , [ invuln "Flee" "Rin" [Physical] ]
    ] []
  , Character
    "Obito Uchiha"
    "A member of Team Minato, Obito is treated as a nobody despite his Uchiha heritage. He dreams of becoming Hokage so that people will finally acknowledge him. Accustomed to helping from the sidelines, if he falls in battle, he will lend his strength to his allies."
    [ [ Skill.new
        { Skill.name      = "Piercing Stab"
        , Skill.desc      = "Spotting an opening in his enemy's defense, Obito stabs them to deal 15 piercing damage. Deals 10 additional damage during [Sharingan]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` userHas "Sharingan"
                pierce (15 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Grand Fireball"
        , Skill.desc      = "Obito breathes searing fire on an enemy, dealing 15 affliction damage for 2 turns. During [Sharingan], this skill deals the full 30 affliction damage instantly and has no cooldown."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 2 [Afflict 15] ]
        }
      , Skill.new
        { Skill.name      = "Grand Fireball"
        , Skill.desc      = "Obito breathes searing fire on an enemy, dealing 15 affliction damage for 2 turns. During [Sharingan], this skill deals the full 30 affliction damage instantly and has no cooldown."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Nin]
        , Skill.varicd    = True
        , Skill.effects   =
          [ To Enemy $ afflict 30 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Obito targets an ally. For 4 turns, Obito gains 15 points of damage reduction, and if Obito dies, the ally will gain 5 points of damage reduction and deal 5 additional non-affliction damage."
        , Skill.classes   = [Mental, Unremovable, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To XAlly $ tag 4
          ,  To Self do
                apply 4 [Reduce All Flat 15]
                trap 4 OnDeath $ everyone $ whenM (targetHas "Sharingan") $
                    apply' "Borrowed Sharingan" 0
                        [ Reduce All Flat 5
                        , Strengthen NonAffliction Flat 5
                        ]
          ]
        }
      ]
    , [ invuln "Flee" "Obito" [Physical] ]
    ] []
  , Character
    "Corrupted Obito"
    "After being rescued from the brink of death, Obito has hurried back to the Hidden Leaf Village only to witness Kakashi stab Rin through the heart. With his sanity shattered by trauma and his Mangekyō Sharingan awakened, he wields the wood-shaping abilities of his Zetsu armor to rampage through the senseless hell his life has become."
    [ [ Skill.new
        { Skill.name      = "Cutting Sprigs"
        , Skill.desc      = "A wooden skewer impales an enemy, dealing 20 piercing damage and permanently increasing the damage of this skill on the target by 5. Deals twice as much damage if the target is affected by [Murderous Resolve]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 2 `bonusIf` targetHas "Murderous Resolve"
                stacks <- targetStacks "Cutting Sprigs"
                pierce $ bonus * (20 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mangekyō Sharingan"
        , Skill.desc      = "Obito activates his trauma-awakened Mangekyō eye to counter the next non-mental skill used on him."
        , Skill.classes   = [Chakra, Invisible, Single]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ trapFrom 0 (Counter NonMental) $ tag 1 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Murderous Resolve"
        , Skill.desc      = "Obito's mind snaps and fixates obsessively on an enemy who was countered by [Mangekyō Sharingan] last turn. For 4 turns, the target's damage is weakened by 5 and they are prevented from reducing damage or becoming invulnerable."
        , Skill.require   = HasU "Mangekyō Sharingan"
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Enemy $ apply 4 [Expose, Weaken All Flat 5] ]
        }
      ]
    , [ invuln "Hide" "Obito" [Mental] ]
    ] []
  , Character
    "Masked Man"
    "As the Nine-Tailed Beast rampages across the Hidden Leaf Village, a mysterious masked man appears and tries to bend it to his will. The legendary beast demolishes house after house, laying waste to the defenses of its enemies."
    [ [ Skill.new
        { Skill.name      = "Kamui Chain Combo"
        , Skill.desc      = "The masked man snares an enemy in sealing chains and phases through them, becoming invulnerable to damage and ignoring harmful effects other than chakra cost changes for 1 turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self  $ apply 1 [Invulnerable All]
          , To Enemy $ tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kamui Banishment"
        , Skill.desc      = "The masked man uses a rare space-time technique to warp an enemy to his pocket dimension, dealing 20 piercing damage and making them immune to effects from their allies for 1 turn. While active, the target can only target the masked man or themselves. Deals 20 additional damage and lasts 1 additional turn if the target is affected by [Kamui Chain Combo]."
        , Skill.classes   = [Chakra, Melee, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 2 `bonusIf` targetHas "Kamui Chain Combo"
                pierce (20 * bonus)
                userSlot <- user slot
                apply (1 * bonus) [Seal, Taunt userSlot]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Kurama"
        , Skill.desc      = "The masked man summons the Nine-Tailed Beast to the battlefield to wreak havoc, demolishing the enemy team's destructible defenses and his own destructible barrier. For 3 turns, it deals 25 damage to a random enemy. While active, the masked man and his allies ignore status effects from enemies except chakra cost changes."
        , Skill.classes   = [Chakra, Melee, Summon, Bypassing]
        , Skill.cost      = [Blood, Gen, Tai]
        , Skill.cooldown  = 5
        , Skill.channel   = Ongoing 3
        , Skill.start     =
          [ To Enemies demolishAll ]
        , Skill.effects   =
          [ To REnemy $ damage 25
          , To Allies $ apply 1 [Enrage]
          ]
        }
      ]
    , [ invuln "Teleportation" "The masked man" [Chakra] ]
    ] []
  ]
