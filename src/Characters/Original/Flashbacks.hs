{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Original.Flashbacks (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
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
                trap 4 OnDeath $ self killHard
                self $ trap 4 OnDeath $
                    everyone $ whenM (targetHas "Life Link") killHard
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Adamantine Sealing Chains"
        , Skill.desc      = "A cage of chain-shaped chakra seals an enemy, removing the effects of helpful skills from them and stunning them for 2 turns. While active, the target is invulnerable to allies as well as enemies."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood, Gen]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                purge
                apply 2 [Stun All, Alone, Invulnerable All]
          ]
        }
      ]
    , [ invuln "Adamantine Covering Chains" "Kushina" [Chakra] ]
    ]
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
        , Skill.dur       = Control 2
        , Skill.effects   =
          [ To Enemies $ apply 1 [Expose]
          , To Self    $ gain [Rand]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Space-Time Marking"
        , Skill.desc      = "For 3 turns, [Flying Raijen] marks its target for 1 turn. Using [Flying Raijen] causes marked allies to become invulnerable for 1 turn and deals 30 damage to marked enemies."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen, Nin]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ To Self $ tag 3 ]
        }
      ]
    , [ invuln "Flying Light" "Minato" [Physical] ]
    ]
  , Character
    "Yondaime Minato"
    "Now the fourth Hokage, Minato has been shaped by his responsibilities into a thoughtful and strategic leader. With his space-time jutsu, he redirects the attacks of his enemies and effortlessly passes through their defenses."
    [ [ Skill.new
        { Skill.name      = "Space-Time Marking"
        , Skill.desc      = "Minato opportunistically marks targets to use as teleport destinations for avoiding attacks. Allies who do not use skills this turn and enemies who do not use skills next turn will be marked for 4 turns. Minato gains 5 points of damage reduction for each marked target. This skill stacks."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAllies $ trap (-1) OnNoAction do
                applyWith [Invisible] 4 []
                self $ applyWith [Invisible] 4 [Reduce All Flat 5]
          , To Enemies $ trap (-1) OnNoAction do
                applyWith [Invisible] (-4) []
                self $ applyWith [Invisible] (-4) [Reduce All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Teleportation Barrier"
        , Skill.desc      = "Space warps around Minato or one of his allies. The first skill an enemy uses on the target will be reflected back at them."
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
        , Skill.require   = HasU 1 "Space-Time Marking"
        , Skill.classes   = [Chakra, Bypassing]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self    $ apply 1 [Invulnerable All]
          , To XAllies $ apply 1 [Invulnerable All]
          ]
        }
      ]
    ]
  , Character
    "Hashirama Senju"
    "The founder and first Hokage of the Hidden Leaf Village, Hashirama is headstrong and enthusiastic. He believes with all his heart that communities should behave as families, taking care of each other and protecting their children from the cruelties of war. Due to a unique genetic mutation, Hashirama is able shape wood into defensive barriers and constructs."
    [ [ Skill.new
        { Skill.name      = "Wooden Dragon"
        , Skill.desc      = "A vampiric dragon made of wood drains chakra from Hashirama's enemies, making him invulnerable to chakra skills for 2 turns. While active, Hashirama absorbs 1 random chakra from his enemies each turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Self   $ apply 1 [Invulnerable Chakra]
          , To REnemy $ absorb 1
          ]
        , Skill.changes   =
            changeWith "Veritable 1000-Armed Kannon" \x ->
                 x { Skill.dur     = Action 3, Skill.cost = [Blood] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wood Golem"
        , Skill.desc      = "A giant humanoid statue attacks an enemy for 2 turns, dealing 20 damage each turn. While active, Hashirama is invulnerable to physical skills."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self  $ apply 1 [Invulnerable Physical]
          ]
        , Skill.changes   =
            changeWith "Veritable 1000-Armed Kannon" \x ->
                 x { Skill.dur     = Action 3, Skill.cost = [Blood] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Veritable 1000-Armed Kannon"
        , Skill.desc      = "A titanic many-handed Buddha statue looms over the battlefield, providing 30 permanent destructible defense to Hashirama and his allies. For the next 3 turns, [Wooden Dragon] and [Wood Golem] cost 1 fewer arbitrary chakra and last 1 additional turn."
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
    ]
  , Character
    "Young Kakashi"
    "A member of Team Minato, Kakashi is the thirteen-year-old son of the legendary White Fang. His early ninjutsu and borrowed Sharingan make him the equal of any adult he faces."
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
        , Skill.classes   = [Bane, Chakra, Melee]
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
        , Skill.desc      = "Kakashi anticipates an opponent's moves for 2 turns. If they use a skill that gains, depletes, or absorbs chakra, Kakashi gains 1 random chakra. If they use a skill that stuns or disables, Kakashi's skills will stun next turn. If they use a skill that damages, Kakashi's damage will be increased by 10 during the next turn."
        , Skill.classes   = [Mental, Ranged, Invisible]
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
    ]
  , Character
      "Rin Nohara"
      "A chūnin on Team Minato, Rin is a quiet but strong-willed medical-nin. Her priority is always healing her teammates, though she can also defend herself with traps if necessary."
      [ [ Skill.new
        { Skill.name      = "Pit Trap"
        , Skill.desc      = "An enemy falls into a pit and is trapped there for 1 turn. At the end of their turn, the target takes 15 piercing damage. If they used a skill that turn, they take 15 additional damage. While active, Rin gains 15 points of damage reduction."
        , Skill.classes   = [Physical, Ranged, Invisible, Bypassing]
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
    ]
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
    ]
  , Character
    "Masked Man"
    "As the Nine-Tailed Beast rampages across the Hidden Leaf Village, a mysterious masked man appears and attempts to bend it to his will. The legendary beast demolishes house after house, laying waste to the defenses of its enemies."
    [ [ Skill.new
        { Skill.name      = "Kamui Chain Combo"
        , Skill.desc      = "The masked man snares an enemy in sealing chains and phases through them, becoming invulnerable for 1 turn."
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
        , Skill.desc      = "The masked man uses a rare space-time technique to warp an enemy to his pocket dimension, dealing 20 piercing damage and making them invulnerable to their allies for 1 turn. While active, the target can only target the masked man or themselves. Deals 20 additional damage and lasts 1 additional turn if the target is affected by [Kamui Chain Combo]."
        , Skill.classes   = [Chakra, Melee, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 2 `bonusIf` targetHas "Kamui Chain Combo"
                pierce (20 * bonus)
                userSlot <- user slot
                apply (1 * bonus) [Alone, Taunt userSlot]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Major Summoning: Kurama"
        , Skill.desc      = "The masked man summons the Nine-Tailed Beast to the battlefield to wreak havoc, demolishing the enemy team's destructible defenses and his own destructible barrier. For 3 turns, it deals 25 damage to a random enemy. While active, the masked man and his allies ignore status effects from enemies except chakra cost changes."
        , Skill.classes   = [Summon, Melee, Bypassing]
        , Skill.cost      = [Blood, Gen, Tai]
        , Skill.cooldown  = 5
        , Skill.dur       = Ongoing 3
        , Skill.start     =
          [ To Enemies demolishAll ]
        , Skill.effects   =
          [ To REnemy $ damage 25
          , To Allies $ apply 1 [Enrage]
          ]
        }
      ]
    , [ invuln "Teleportation" "The masked man" [Chakra] ]
    ]
  ]
