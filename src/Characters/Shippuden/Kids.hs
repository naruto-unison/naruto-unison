{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Kids (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Naruto Uzumaki"
    "Naruto's years of training under Jiraiya have made him stronger, wiser, and far more effective at controlling his immense chakra. He has learned how to distribute his chakra efficiently across numerous shadow clones, and can harness the flow of energy within himself to transform and repurpose chakra."
    [ [ Skill.new
        { Skill.name      = "Giant Rasengan"
        , Skill.desc      = "Naruto instantly creates several shadow clones to aid him in wielding the Rasengan. Infused with highly compressed chakra, the Rasengan blasts through his enemy's guard and deals 40 damage. If [Multi Shadow Clone] was used last turn, this skill becomes [Rasenshuriken][n][t]."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ damage 40 ]
        }
      , Skill.new
        { Skill.name      = "Rasenshuriken"
        , Skill.desc      = "Deals 50 piercing damage. Deals 25 additional damage if the target was countered by [Multi Shadow Clone] last turn."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 25 `bonusIf` targetHas "Multi Shadow Clone"
                pierce (50 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Multi Shadow Clone"
        , Skill.desc      = "Naruto creates countless clones hidden in the area around him, who counter the first harmful skill used on him in the next turn."
        , Skill.classes   = [Physical, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                trapFrom 1 (Counter All) $ tag 1
                vary' 1 "Giant Rasengan" "Giant Rasengan"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Boost"
        , Skill.desc      = "Naruto cycles his chakra to transform 2 chakra of any type into 1 ninjutsu chakra and 1 taijutsu chakra. The flow of power cures him of enemy effects and provides 10 points of damage reduction for 1 turn."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                cureAll
                gain [ Nin, Tai]
                apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ invuln "Shadow Clone Save" "Naruto" [Chakra] ]
    ] []
  , Character
    "Sakura Haruno"
    "Sakura's years of training under Tsunade have provided her with an intricate understanding of healing and the human body. Now a chūnin, she has learned how to store her chakra in concentrated points and then unleash it in empowering waves."
    [ [ Skill.new
        { Skill.name      = "Cherry Blossom Clash"
        , Skill.desc      = "Sakura expertly condenses her chakra into her fist and punches an enemy, dealing 25 damage. Spends a Seal if available to deal 10 additional damage to all enemies."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To XEnemies $ whenM (userHas "Seal") $ damage 10
          ,  To Enemy do
                bonus <- 10 `bonusIf` userHas "Seal"
                damage (25 + bonus)
          , To Self do
                removeStack "Seal"
                unlessM (userHas "Seal") do
                    vary "Mystical Palm Healing" baseVariant
                    vary "Strength of One Hundred Seal" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name     = "Mystical Palm Healing"
        , Skill.desc     = "Using advanced healing techniques, Sakura restores half of an ally's missing health and cures them of baneful effects. Spends a Seal if available to have no cooldown and cost 1 random chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly do
                cureBane
                restore 50
          ]
        }
      , Skill.new
        { Skill.name     = "Mystical Palm Healing"
        , Skill.desc     = "Using advanced healing techniques, Sakura restores half of an ally's missing health and cures them of baneful effects. Spends a Seal if available to have no cooldown and cost 1 random chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand]
        , Skill.varicd    = True
        , Skill.effects   =
          [ To XAlly do
                cureBane
                restore 50
          , To Self do
                removeStack "Seal"
                unlessM (userHas "Seal") do
                    vary "Mystical Palm Healing" baseVariant
                    vary "Strength of One Hundred Seal" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Strength of One Hundred Seal"
        , Skill.desc      = "Sakura stores up chakra in a point on her forehead, gaining 3 Seals. Sakura's skills spend Seals to become empowered. While active, this skill becomes [Seal Release]."
        , Skill.classes   = [Chakra, Resource]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                addStacks "Seal" 3
                vary "Mystical Palm Healing" "Mystical Palm Healing"
                vary "Strength of One Hundred Seal" "Seal Release"
          ]
        }
      , Skill.new
        { Skill.name      = "Seal Release"
        , Skill.desc      = "Spends a Seal to restore 25 health to Sakura and cure her of enemy effects."
        , Skill.classes   = [Chakra]
        , Skill.effects   =
          [ To Self do
                cureAll
                heal 25
                removeStack "Seal"
                unlessM (userHas "Seal") do
                    vary "Mystical Palm Healing" baseVariant
                    vary "Strength of One Hundred Seal" baseVariant
          ]
        }
      ]
      , [ invuln "Dodge" "Sakura" [Physical] ]
    ] []
  , Character
    "Sasuke Uchiha"
    "Sasuke's years of training under Orochimaru have made him a master of his elemental aspects. Now that he has absorbed Orochimaru, he has added the sannin's snake abilities to his own lightning attacks, which pierce through the defenses of his enemies. He is regarded as one of the most dangerous and ruthless ninjas alive."
    [ [ Skill.new
        { Skill.name      = "Lightning Flash"
        , Skill.desc      = "Sasuke infuses a spinning shuriken with Chidori and hurls it at an enemy, dealing 30 piercing damage and leaving a trail of electricity that briefly links Sasuke to his enemies."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 30
          , To Self  $ tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Shuriken"
        , Skill.desc      = "Sasuke throws two shuriken rigged with electric wires at an enemy, concealing the lower blade in the shadow of the upper one. Deals 20 piercing damage and an additional 5 if [Lightning Flash] was used last turn. If the target uses a ranged skill next turn, they will receive 10 piercing damage."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` userHas "Lightning Flash"
                damage (20 + bonus)
                trap (-1) (OnAction Ranged) $ pierce 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kirin"
        , Skill.desc      = "A pillar of lightning strikes an enemy, dealing 40 piercing damage. If [Lightning Flash] was used last turn, this skill bypasses invulnerability and deals 40 affliction damage instead."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                has <- userHas "Lightning Flash"
                if has then afflict 40 else pierce 40
          ]
        , Skill.changes   = changeWith "Lightning Flash" $ addClass Bypassing
        }
      ]
    , [ invuln "Snake Shedding" "Sasuke" [Physical] ]
    ] []
  , Character
    "Kiba Inuzuka"
    "Kiba's years with Akamaru have enhanced their bond and teamwork. Now a chūnin, he has learned the alarming ability to transform Akamaru into a bestial humanoid resembling Kiba. As they progress through several stages of shapeshifting, they gradually transform into unstoppable rampaging beasts."
    [ [ Skill.new
        { Skill.name      = "Man-Beast Clone"
        , Skill.desc      = "Akamaru transforms into a bestial copy of Kiba, providing 15 points of damage reduction to Kiba for 4 turns and causing him to ignore stuns. While active, this skill becomes [Three-Headed Wolf][b][b]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.channel   = Action 4
        , Skill.start     =
          [ To Self $ vary "Man-Beast Clone" "Three-Headed Wolf"]
        , Skill.effects   =
          [ To Self $ apply 1 [Ignore $ Any Stun, Reduce All Flat 15] ]
        }
      , Skill.new
        { Skill.name      = "Three-Headed Wolf"
        , Skill.desc      = "Akamaru and Kiba fuse together, ending [Man-Beast Clone]. For 3 turns, Kiba gains 30 points of damage reduction and ignores stuns. While active, this skill becomes [Giant Rotating Fang][t][b][b]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                cancelChannel "Man-Beast Clone"
                vary' 3 "Man-Beast Clone" "Giant Rotating Fang"
                remove "Man-Beast Clone"
                apply 3 [Ignore $ Any Stun, Reduce All Flat 30]
          ]
        }
      , Skill.new
        { Skill.name      = "Giant Rotating Fang"
        , Skill.desc      = "Deals 40 damage to all enemies and stuns them for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Blood, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                damage 40
                apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rotating Fang"
        , Skill.desc      = "Spinning like a buzzsaw, Kiba deals 30 damage to an enemy. Deals 20 damage to the rest of their team during [Three-Headed Wolf]. Deals 20 additional damage to a random enemy during [Man-Beast Clone]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy    $ damage 30
          , To XEnemies $ whenM (userHas "Three-Headed Wolf")  $ damage 20
          , To REnemy   $ whenM (channeling "Man-Beast Clone") $ damage 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fang Over Fang"
        , Skill.desc      = "Kiba launches attack after attack on an enemy, dealing 40 damage to them and permanently lowering their non-affliction damage by 10. Deals 10 additional damage during [Man-Beast Clone]. Deals 20 additional damage during [Three-Headed Wolf]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                apply 0 [Weaken All Flat 10]
                cloneBonus <- 10 `bonusIf` userHas "Man-Beast Clone"
                wolfBonus  <- 20 `bonusIf` userHas "Three-Headed Wolf"
                damage (40 + cloneBonus + wolfBonus)
          ]
        }
      ]
    , [ invuln "Hide" "Kiba" [Mental] ]
    ] []
  , Character
    "Shino Aburame"
    "Shino's years of practice with his loyal bugs have deepened his connection with them. Having attained the rank of chūnin, Shino has learned to breed his insects to favor specific traits. His advanced parasites accumulate invisibly in targets before bursting out all at once."
    [ [ Skill.new
        { Skill.name       = "Insect Swarm"
        , Skill.desc       = "A wave of insects attack an enemy, dealing 15 affliction damage to them for 3 turns and making them immune to effects from allies. While active, this skill becomes [Chakra Leech]."
        , Skill.classes   = [Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.channel   = Action 3
        , Skill.cooldown  = 2
        , Skill.start     =
          [ To Self $ vary "Insect Swarm" "Chakra Leech"
          ,  To Enemy do
                stacks <- targetStacks "Chakra Leech"
                addStacks' (-3) "Chakra Leech " stacks
                remove "Chakra Leech"
          ]
        , Skill.effects   =
          [ To Enemy do
                stacks <- targetStacks "Chakra Leech"
                afflict (15 + 5 * stacks)
                apply 1 [Seal]
          ]
        }
      , Skill.new
        { Skill.name       = "Chakra Leech"
        , Skill.desc       = "Adds 5 damage to the next [Insect Swarm] on an enemy and depletes 1 random chakra."
        , Skill.classes    = [Bane, Physical, Ranged]
        , Skill.effects    =
          [ To Enemy do
                deplete 1
                apply 0 []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Insect Barricade"
        , Skill.desc      = "Colonies of insects hide around Shino or one of his allies, countering the first harmful skill used against them in the next turn. If an enemy is countered, [Gigantic Beetle Infestation] is applied to them and activated. If no enemies are countered, Shino gains a bloodline chakra."
        , Skill.classes   = [Melee, Invisible, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ bomb' "Barricaded" (-1) []
                           [ To Expire $ self $ gain [Blood] ]
          ,  To Ally $ trapFrom 1 (Counter All) do
                stacks <- targetStacks "Gigantic Beetle Infestation"
                damage (25 + 25 * stacks)
                remove "Gigantic Beetle Infestation"
                remove "Chakra Leech"
                self $ remove "Barricaded"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gigantic Beetle Infestation"
        , Skill.desc      = "A small parasitic bug infects an enemy. In 3 turns, the bug will burst out, dealing 25 damage and activating all other copies of this skill on the target."
        , Skill.classes   = [Bane, Melee, Invisible]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy $ bomb 3 [] [ To Expire do
                stacks <- targetStacks "Gigantic Beetle Infestation"
                damage (25 + 25 * stacks)
                remove "Gigantic Beetle Infestation"
                remove "Chakra Leech" ]
          ]
        }
      ]
    , [ invuln "Insect Cocoon" "Shino" [Physical] ]
    ] []
  , Character
    "Hinata Hyūga"
    "With the Chūnin Exam behind her and Naruto's words deep in her heart, Hinata has grown and become stronger. Now that she has mastered the Hyūga clan tactics, she can give life to powerful chakra lions and hinder the chakra paths of her enemies."
    [ [ Skill.new
        { Skill.name      = "Pressure Point Strike"
        , Skill.desc      = "Hinata blocks an enemy's pressure point, dealing 10 damage to them and increasing the costs of their skills by 1 random for 1 turn. Deals 10 additional damage during [Eight Trigrams Sixty-Four Palms]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` userHas "Eight Trigrams Sixty-Four Palms"
                damage (10 + bonus)
                stacks <- targetStacks "Eight Trigrams Sixty-Four Palms"
                apply (1 + stacks) [Exhaust All]
                remove "Eight Trigrams Sixty-Four Palms"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gentle Step Twin Lion Fists"
        , Skill.desc      = "Hinata creates two lions out of chakra. The next 2 times an enemy uses a harmful skill, a chakra lion will attack them, dealing 30 damage and depleting 1 random chakra. Creates a third lion during [Eight Trigrams Sixty-Four Palms]. Cannot be used while active. Ends if Hinata dies."
        , Skill.classes   = [Chakra, Melee, Bypassing, Soulbound, Single, Resource]
        , Skill.cost      = [Blood, Nin]
        , Skill.effects   =
          [ To Self do
                bonus <- 1 `bonusIf` userHas "Eight Trigrams Sixty-Four Palms"
                addStacks "Chakra Lion" (2 + bonus)
          , To Enemies $ trap' 0 OnHarm do
                self $ removeStack "Chakra Lion"
                has <- userHas "Chakra Lion"
                if has then do
                    deplete 1
                    damage 30
                else
                    removeTrap "Gentle Step Twin Lion Fists"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Sixty-Four Palms"
        , Skill.desc      = "For 4 turns, each time an enemy affected by [Pressure Point Strike] uses a harmful skill, Hinata's next [Pressure Point Strike] will last 1 additional turn on them."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self    $ tag 4
          , To Enemies $ trap 4 OnHarm addStack
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Hinata" [Mental] ]
    ] []
  , Character
    "Shikamaru Nara"
    "Once known for his laziniess, Shikamaru has worked tirelessly to become a leader. With years of experience, his plans have become even more convoluted and intricate."
    [ [ Skill.new
        { Skill.name      = "Shadow Sewing"
        , Skill.desc      = "Delicate tendrils of shadow wrap around an enemy, dealing 35 damage and stunning their non-mental skills for 1 turn. While active, this skill becomes [Shadow Sewing: Hold][g]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 35
                apply 1 [Stun NonMental]
                self $ vary' 1 "Shadow Sewing" "Shadow Sewing: Hold"
          ]
        }
      , Skill.new
        { Skill.name      = "Shadow Sewing: Hold"
        , Skill.desc      = "Deals 20 damage to an enemy affected by [Shadow Sewing] and prolongs its stun by 1 turn."
        , Skill.require   = HasU "Shadow Sewing"
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemies do
                damage 20
                apply' "Shadow Sewing" 1 [Stun NonMental]
                self $ vary' 1 "Shadow Sewing" "Shadow Sewing: Hold"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Long-Range Tactics"
        , Skill.desc      = "Shikamaru goes long. For 4 turns, each time Shikamaru uses a harmful skill, if no enemy used a skill last turn that dealt non-affliction damage to him, he will become invulnerable for 1 turn. While active, this skill becomes [Final Explosion][r][r]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                tag 4
                vary' 4 "Long-Range Tactics" "Final Explosion"
                delay (-1) $ trap' (-4) OnHarm $
                    unlessM (userHas "What a Drag") $ apply 1 [Invulnerable All]
                trap' 4 (OnDamaged NonAffliction) $ tag' "What a Drag" 1
          ]
        }
      , Skill.new
        { Skill.name      = "Final Explosion"
        , Skill.desc      = "Deals 100 damage to an enemy affected by [Shadow Sewing] or [Expert Analysis]. Cannot be used if an enemy used a skill that dealt non-affliction damage to Shikamaru last turn."
        , Skill.require   = HasI (-1) "What a Drag"
        , Skill.classes   = [Physical]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                has1 <- targetHas "Shadow Sewing"
                has2 <- targetHas "Expert Analysis"
                when (has1 || has2) $ damage 100
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Expert Analysis"
        , Skill.desc      = "Shikamaru carefully analyzes an enemy to discern their weaknesses. For 3 turns, any time the target uses a skill, they will be prevented from becoming invulnerable, reducing damage, or benefiting from counters and reflects for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap (-3) (OnAction All) $ apply 1 [Expose, Uncounter] ]
        }
      ]
    , [ invuln "Dodge" "Shikamaru" [Physical] ]
    ] []
  , Character
    "Chōji Akimichi"
    "Chōji's years of mastering his clan's techniques have ended the growing chūnin's dependence on Akimichi pills. Now that he can reshape his body at will without having to sacrifice his health, chakra costs are the only remaining limits on his physical power."
    [ [ Skill.new
        { Skill.name      = "Butterfly Bombing"
        , Skill.desc      = "Chōji charges at an enemy for 1 turn, ignoring status effects from enemies except chakra cost changes. At the end of the turn, he deals 30 damage to the target. Increases the costs of Chōji's skills by 2 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand, Rand]
        , Skill.effects   =
          [ To Enemy $ delay (-1) $ damage 30
          ,  To Self do
                  apply 1 [Enrage]
                  hide' "calories" 0 [Exhaust All]
                  hide' "calories" 0 [Exhaust All]
          ]
        }
      , Skill.new
        { Skill.name      = "Butterfly Bombing"
        , Skill.desc      = "Chōji charges at an enemy for 1 turn, ignoring status effects from enemies except chakra cost changes. At the end of the turn, he deals 30 damage to the target. Increases the costs of Chōji's skills by 2 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy $ delay (-1) $ damage 30
          , To Self do
                apply 1 [Enrage]
                hide' "calories" 0 [Exhaust All]
                hide' "calories" 0 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Spiky Human Boulder"
        , Skill.desc      = "Chōji rolls into a ball bristling with needle-like spikes and deals 15 damage to an enemy for 2 turns. While active, Chōji counters non-mental skills. Increases the cost of Chōji's skills by 1 random chakra each turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand, Rand]
        , Skill.channel   = Action 2
        , Skill.start     =
          [ To Enemy $ damage 15
          , To Self do
                trapFrom 2 (CounterAll NonMental) $ return ()
                hide' "calories" 0 [Exhaust All]
          ]
        }
      , Skill.new
        { Skill.name      = "Spiky Human Boulder"
        , Skill.desc      = "Chōji rolls into a ball bristling with needle-like spikes and deals 15 damage to an enemy for 2 turns. While active, Chōji counters non-mental skills. Increases the cost of Chōji's skills by 1 random chakra each turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.channel   = Action 2
        , Skill.start     =
          [ To Enemy $ damage 15
          ,  To Self do
                trapFrom 2 (CounterAll NonMental) $ return ()
                hide' "calories" 0 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Butterfly Mode"
        , Skill.desc      = "Performing an advanced Akimichi technique that would be lethal without precise control over his body, Chōji converts calories into jets of chakra energy that grow from his back like butterfly wings. Once used, this skill permanently becomes [Super-Slam][n][r][r]. Each turn after [Butterfly Mode] is activated, the costs of Chōji's skills decrease by 1 random chakra."
        , Skill.classes   = [Chakra]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ To Self do
                replicateM_ 3 $ hide' "calories" 0 [Exhaust All]
                vary "Butterfly Bombing"   "Butterfly Bombing"
                vary "Spiky Human Boulder" "Spiky Human Boulder"
                vary "Butterfly Mode"      "Super-Slam"
                vary "Block"               "Block"
          ]
        , Skill.effects   =
          [ To Self $ removeStack "calories"]
        }
      , Skill.new
        { Skill.name      = "Super-Slam"
        , Skill.desc      = "Chōji funnels chakra into his hands until they are as powerful as iron jackhammers and slams them into an enemy, dealing 30 damage and curing Chōji of enemy effects. Increases the cost of Chōji's skills by 2 random chakra."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy $ damage 30
          , To Self  $ replicateM_ 2 $ hide' "calories" 0 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Block"
        , Skill.desc      = "Chōji becomes invulnerable for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cooldown  = 4
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Self $ apply 1 [Invulnerable All] ]
        }
      , Skill.new
        { Skill.name      = "Block"
        , Skill.desc      = "Chōji becomes invulnerable for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 1 [Invulnerable All] ]
        }
      ]
    ] []
  , Character
    "Ino Yamanaka"
    "Now a chūnin, Ino takes control of every fight she faces. Her overpowering will steals the skills and secrets of her enemies and forces her allies to fight on no matter the cost. "
    [ [ Skill.new
        { Skill.name      = "Mind Destruction"
        , Skill.desc      = "Ino infiltrates an enemy's mind and prepares to strike at a moment of weakness. Next turn, the target receives 15 damage. If they use a harmful skill, its effects will be nullified and this skill will be replaced by that skill for 1 turn. Ino's copy of their skill has no chakra cost and ends when this skill reverts."
        , Skill.classes   = [Mental, Ranged, Invisible, Unreflectable, Unremovable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                trap (-1) (Countered Uncounterable) $ return ()
                bomb (-1) [Replace 1 All 0 False] [ To Done $ damage 15 ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Proxy Surveillance"
        , Skill.desc      = "Ino's will takes over the battlefield. For 3 turns, she detects all invisible effects and enemy cooldowns. While active, the enemy team's damage reduction skills and destructible defense skills are reduced by 15. If an enemy uses a skill with negative damage reduction, damage to them is increased by its amount. If they use a skill with negative destructible defense, their target is damaged for its amount. If they use a skill with negative destructible barrier, they are damaged for its amount."
        , Skill.classes   = [Mental, Invisible, Uncounterable, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies $ apply 3 [Reveal, Build (-15), Unreduce 15] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mind Transfer Clone"
        , Skill.desc      = "Ino takes control of her allies, forcing them to fight on no matter their condition. For 2 turns, her allies ignore status effects from enemies except chakra cost changes."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To XAllies $ apply 2 [Enrage] ]
        }
      ]
    , [ invuln "Hide" "Ino" [Mental] ]
    ] []
  , Character
    "Rock Lee"
    "Lee's years of training with Gai have taught him not only new abilities, but what it truly means to fight. His strength grows as his allies fall, determined to honor them by finishing their battle."
    [ [ Skill.new
        { Skill.name      = "Leaf Rising Wind"
        , Skill.desc      = "Lee plants his back on the ground and uses his entire body as a spring to kick an enemy with such power that they are launched into the air, dealing 15 damage and lowering the target's non-affliction damage by 15 for 2 turns. Deals 10 additional damage per dead ally. Effect lasts 1 additional turn per dead ally."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                dead <- numDeadAllies
                damage (15 + 10 * dead)
                apply (2 + dead) [Weaken All Flat 15]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name     = "Leaf Hurricane"
        , Skill.desc     = "Lee whirls and builds up momentum, dealing 20 damage to an enemy and gaining 10 points of damage reduction for 1 turn. Deals 10 additional damage and provides 10 additional points of damage reduction if used last turn. Deals 10 additional damage per dead ally."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
              dead  <- numDeadAllies
              bonus <- 10 `bonusIf` userHas "Leaf Great Whirlwind"
              damage (20 + bonus + 10 * dead)
          , To Self do
                bonus <- 10 `bonusIf` userHas "Leaf Great Whirlwind"
                apply 1 [Reduce All Flat (10 + bonus)]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Full Power of Youth"
        , Skill.desc      = "Fighting for his beliefs in the face of adversity, Lee kicks an enemy into the air and slams them back down to earth with such force that light explodes around them. Deals 20 damage, 20 additional damage per dead ally, and 20 additional damage per 30 health Lee has lost."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                dead <- numDeadAllies
                hp   <- user health
                damage $ 20 + 20 * dead + 20 * ((100 - hp) `quot` 30)
          ]
        }
      ]
    , [ invuln "Dodge" "Lee" [Physical] ]
    ] []
  , let loadout = [0, 0, 0]
    in Character
    "Tenten"
    "Now a chūnin, Tenten's arsenal has expanded to a prodigious stockpile of some of the most powerful weapons in existence, including the legendary fan of the Sage of the Six Paths. Taking any excuse to show off the size and variety of her collection, she has assembled multiple item sets to switch out at a moment's notice."
    [ [ Skill.new
        { Skill.name      = "Kunai Grenade"
        , Skill.desc      = "Tenten throws an explosive filled with a frankly ridiculous amount of kunai at an enemy, dealing 20 damage to them and 10 damage to the rest of their team."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy    $ damage 20
          , To XEnemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Tensasai"
        , Skill.desc      = "Using an advanced form of her Rising Twin Dragons technique, Tenten rains blindingly fast projectiles upon the battlefield, dealing 25 piercing damage to all enemies."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ pierce 25 ]
        }
      , Skill.new
        { Skill.name      = "Leaf Fan: Coil of Fire"
        , Skill.desc      = "Using the Sage of the Six Path's legendary battle fan, Tenten blasts her enemies with a sea of raging fire, dealing 15 damage and 15 affliction damage to all enemies, as well as 5 affliction damage for the next 3 turns."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies do
                damage 15
                afflict 15
                apply 3 [Afflict 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chain Spin"
        , Skill.desc      = "Tenten whirls a long chain whip around her team, making them invulnerable to physical skills for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Allies $ apply 1 [Invulnerable Physical] ]
        }
      , Skill.new
        { Skill.name      = "Segmented Iron Dome"
        , Skill.desc      = "Tenten produces a massive metal dome from a very small scroll, providing 25 permanent destructible defense to her and her allies."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Allies $ defend 0 25 ]
        }
      , Skill.new
        { Skill.name      = "Leaf Fan: Coil of Wind"
        , Skill.desc      = "Using the Sage of the Six Path's legendary battle fan, Tenten throws a gust of wind that reflects all non-mental skills used on her or her allies next turn."
        , Skill.classes   = [Physical, Invisible, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Allies $ apply 1 [ReflectAll] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Switch Loadout"
        , Skill.desc      = "Scrolling through her scrolls to the next item set, Tenten gains 5 permanent destructible defense and replaces her other skills. Tenten has 3 item sets."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ To Self do
                defend 0 5
                varyLoadout loadout 1
          ]
        }
      , Skill.new
        { Skill.name      = "Switch Loadout"
        , Skill.desc      = "Scrolling through her scrolls to the next item set, Tenten gains 5 permanent destructible defense and replaces her other skills. Tenten has 3 item sets."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ To Self do
                defend 0 5
                varyLoadout loadout 2
          ]
        }
      , Skill.new
        { Skill.name      = "Switch Loadout"
        , Skill.desc      = "Scrolling through her scrolls to the next item set, Tenten gains 5 permanent destructible defense and replaces her other skills. Tenten has 3 item sets."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ To Self do
                defend 0 5
                varyLoadout loadout 0
          ]
        }
      ]
    , [ invuln "Dodge" "Tenten" [Physical] ]
    ] []
  , Character
    "Neji Hyūga"
    "Having surpassed his peers to reach the rank of jōnin, Neji has spent the intervening years honing his skills. He has learned to supplement his precise pressure-point attacks with devastating chakra waves that demolish the defenses of his opponents."
    [ [ Skill.new
        { Skill.name      = "Eight Trigrams Air Palm"
        , Skill.desc      = "Neji sends a blast of chakra-filled air at an enemy's vital points, dealing 20 damage and depleting 1 random chakra."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                deplete 1
                damage 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Hazan Strike"
        , Skill.desc      = "Neji unleashes a giant wave of chakra at an enemy, demolishing their destructible defense and his own destructible barrier, then dealing 45 damage."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                damage 45
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Sixty-Four Palms"
        , Skill.desc      = "For 2 turns, enemies are prevented from reducing damage or becoming invulnerable. If an enemy uses a harmful skill on Neji during the first turn, it is countered and this skill is replaced for 1 turn by [Pressure Point Strike]."
        , Skill.classes   = [Physical, Mental, Invisible]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ apply 2 [Expose]
          , To Self    $ trapFrom 1 (Counter All) $
                vary' 1 "Eight Trigrams Sixty-Four Palms"
                        "Pressure Point Strike"
          ]
        }
      , Skill.new
        { Skill.name      = "Pressure Point Strike"
        , Skill.desc      = "Deals 5 damage to an enemy, depletes 1 random chakra, and causes this skill to remain [Pressure Point Strike] for another turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                deplete 1
                damage 5
          ,  To Self $ vary' 1 "Eight Trigrams Sixty-Four Palms"
                              "Pressure Point Strike"
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Neji" [Mental] ]
    ] []
  , Character
    "Kazekage Gaara"
    "Gaara's years of soul-searching have made him a powerful force for good, ready to assume the title of Kazekage. No longer concerned with destroying others, he devotes himself to protecting his friends and the Hidden Sand Village."
    [ [ Skill.new
        { Skill.name      = "Partial Sand Coffin"
        , Skill.desc      = "Gaara keeps an enemy away from his allies by encasing one of the target's limbs in sand, stunning their non-mental skills for 1 turn and dealing 20 piercing damage when the effect ends. If they were countered by [Third Eye] last turn, all of their skills are stunned and the damage is dealt immediately. If the target has been affected by this skill before, the stun lasts 1 additional turn."
        , Skill.classes   = [Physical, Ranged, Unremovable]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 1 `bonusIf` targetHas "Clinging Sand"
                has   <- targetHas "Third Eye"
                if has then do
                    apply (1 + bonus) [Stun All]
                    pierce 20
                else
                    bomb (1 + bonus) [Stun NonMental] [ To Expire $ pierce 20 ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Third Eye"
        , Skill.desc      = "A hidden layer of sensing sand surrounds Gaara and his allies. Next turn, the first harmful skill used on his team will be countered and provide its target with 15 permanent destructible defense."
        , Skill.classes   = [Physical, Invisible, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Allies $ trapFrom 1 (Counter All) do
                tag 1
                allies do
                    defend 0 15
                    delay (-1) $ remove "Third Eye"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Prison"
        , Skill.desc      = "Crushing ropes of sand constrict into an airtight prison around an enemy, dealing 30 damage to them. For 2 turns, the target is immune to effects from allies and cannot reduce damage or become invulnerable. Deals 20 additional damage if the target was countered by [Third Eye] last turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                bonus <- 20 `bonusIf` targetHas "Third Eye"
                damage (30 + bonus)
                apply 2 [Seal, Expose]
          ]
        }
      ]
    , [ invuln "Levitating Sand Shield" "Gaara" [Physical] ]
    ] []
    , Character
      "Kankurō"
      "Now a jōnin, Kankurō considers himself one of the greatest puppeteers in history after defeating Sasori. Adding Sasori's body to his collection of puppets, Kankurō uses each puppet for a different purpose."
      [ [ Skill.new
          { Skill.name      = "Sasori Surrogate"
          , Skill.desc      = "Sasori's puppet body attacks an enemy, dealing 15 damage to them for 3 turns. While active, this skill becomes [Hidden Coil Strike][r]."
          , Skill.classes   = [Physical, Ranged]
          , Skill.cost      = [Rand, Rand]
          , Skill.cooldown  = 2
          , Skill.channel   = Action 3
          , Skill.start     =
            [ To Self $ vary "Sasori Surrogate" "Hidden Coil Strike" ]
          , Skill.effects   =
            [ To Enemy $ damage 15 ]
          }
        , Skill.new
          { Skill.name      = "Hidden Coil Strike"
          , Skill.desc      = "Kankurō hooks an enemy with the coil hidden in Sasori's body and pulls the target to him, dealing 10 piercing damage. For 1 turn, the target can only target Kankurō or themselves."
          , Skill.classes   = [Physical, Ranged, Bypassing, Unreflectable]
          , Skill.cost      = [Rand]
          , Skill.effects   =
            [ To Enemy do
                  pierce 10
                  userSlot <- user slot
                  apply 1 [Taunt userSlot]
                  remove "Kuroari Trap"
            ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Kuroari Trap"
          , Skill.desc      = "Kankurō's Kuroari puppet stalks an enemy for 5 turns. If Kankurō uses [Hidden Coil Strike] on the target, the trap is activated immediately; otherwise, it is activated at the end of the 5 turns. Activating the trap applies [Kuroari Ambush] to the target, stunning them for 1 turn and making them invulnerable to everyone but Kankurō. Once used, this skill becomes [Iron Maiden][r][r][r]."
          , Skill.classes   = [Physical, Ranged, Nonstacking, InvisibleTraps, Bypassing, Unreflectable, Unremovable]
          , Skill.cost      = [Rand]
          , Skill.cooldown  = 5
          , Skill.effects   =
            [ To Self  $ vary "Kuroari Trap" "Iron Maiden"
            , To Enemy $ bombWith [Invisible] 5 []
                  [ To Done do
                      userSlot <- user slot
                      apply' "Kuroari Ambush" 1 [Stun All, Seal, Duel userSlot]
                  ]
            ]
          }
        , Skill.new
          { Skill.name      = "Iron Maiden"
          , Skill.desc      = "Kankurō's Karasu puppet snaps shut around an enemy, dealing 20 piercing damage and 40 additional damage if the target is affected by [Kuroari Ambush]. Once used, this skill becomes [Kuroari Trap][r]."
          , Skill.classes   = [Physical, Ranged, Uncounterable, Unreflectable]
          , Skill.cost      = [Rand, Rand]
          , Skill.effects   =
              [ To Enemy do
                    bonus <- 40 `bonusIf` targetHas "Kuroari Ambush"
                    pierce (20 + bonus)
              , To Self $ vary "Kuroari Trap" baseVariant
              ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Salamander Shield"
          , Skill.desc      = "Kankurō's Sanshōuo puppet shields him and his allies, providing 40 permanent destructible defense to Kankurō. While Kankurō has destructible defense from this skill, damage against his allies is reflected to him. Cannot be used while active."
          , Skill.classes   = [Physical, Single, Soulbound, Unremovable, Unreflectable]
          , Skill.cost      = [Rand, Rand, Rand]
          , Skill.cooldown  = 5
          , Skill.effects   =
            [ To Self do
                  defend 0 40
                  onBreak'
            , To XAllies do
                  userSlot <- user slot
                  apply 0 [Redirect All userSlot]
            ]
          }
        ]
      , [ invuln "Puppet Distraction" "Kankurō" [Physical] ]
      ] []
  , Character
    "Temari"
    "The Hidden Sand Village's official ambassador, Temari is a formidable jōnin who uses an equally formidable battle fan. Her gusts of wind cut down the defenses of her enemies and free her allies from afflictions."
    [ [ Skill.new
        { Skill.name      = "Cutting Whirlwind"
        , Skill.desc      = "Temari hurls a burst of wind at a target. If used on an enemy, she deals 15 damage to them and becomes invulnerable to them next turn. If used on an ally, she cures them of baneful effects. Targets all allies and enemies during [Sea Dragon]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 1 [Block]
          , To XAlly cureBane
          ]
        , Skill.changes   = changeWith "Sea Dragon" targetAll
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sea Dragon"
        , Skill.desc      = "A giant tornado sweeps across the battlefield for 4 turns, dealing 5 piercing damage to all enemies each turn. While active, non-affliction damage from Temari's team always pierces."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 5
        , Skill.channel   = Ongoing 4
        , Skill.effects   =
          [ To Enemies $ pierce 5
          , To Allies  $ apply 1 [Pierce]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gust Blade"
        , Skill.desc      = "Temari sends slicing blades of wind at an enemy, demolishing their destructible defense and her own destructible barrier, then dealing 35 damage to them. Targets all enemies during [Sea Dragon]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                damage 35
          ]
        , Skill.changes   = changeWith "Sea Dragon" targetAll
        }
      ]
    , [ invuln "Block" "Temari" [Physical] ]
    ] []{-
  , Character
    "Kabuto Yakushi"
    "A dangerous rogue ninja and Orochimaru's pupil, Kabuto grows more powerful by the day through untiring study and research. His knowledge and brilliance are all but limitless, and his ambition has been growing to match them. With his perfected form of reanimation, he sacrifices his enemies to resurrect his dead teammates."
    [ [ Skill.new
        { Skill.name      = "Summoning: Clone Serpent"
        , Skill.desc      = "Kabuto draws blood from himself or an ally, sacrificing 15 of the target's health to summon a giant serpent made up of smaller snakes for 5 turns. When Kabuto uses a skill on an enemy, the serpent bites his target with paralyzing venom, dealing 5 affliction damage to them for 3 turns and ending their Action and Control skills in progress. While poisoned, enemies cannot reduce damage, become invulnerable, or be healed or cured. Kabuto stores a blood sample of the ally most recently affected by this skill."
        , Skill.classes   = [Chakra, Summon]
        , Skill.cost      = [Nin]
        , Skill.channel   = Ongoing 5
        , Skill.start     = [ To XAlly $ everyone $ remove "Blood Sample"
                            • tag' "Blood Sample" 0)
                    , To Ally $ sacrifice 0 15
                    ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Blood Offering"
        , Skill.desc      = "Kabuto draws blood from himself or an ally to summon a venomous snake, sacrificing  A reanimated corpse stalks one of Kabuto's enemies, dealing 35 piercing damage to them. The target cannot be healed or cured for 2 turns."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   = [ To Enemy $ pierce 35 • apply 2 [Plague] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Reanimation Sacrifice"
        , Skill.desc      = "Kabuto slices an enemy with a chakra scalpel, dealing 20 affliction damage to them. If he has a blood sample from [Summoning: Clone Serpent], he injects it into the target and prepares to use their body as a sacrifice. In 2 turns, if the target carries the blood sample of a dead ally, the ally is resurrected with as much health as the target and the target is killed."
        , Skill.classes   = [Bane, Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   = [ To Enemy $ afflict 20
                           • ifI "Summoning: Clone Serpent" $ interrrupt
                           • perI "Summoning: Clone Serpent" 5
                             (\i -> apply' "Summoning: Clone Serpent" 3
                                   [Afflict i, Plague, Expose])
                             0
                           • bomb (-2) [] [ To Expire $ )] ]
        }
      ]
    , [ invuln "Dodge" "Kabuto" [Physical] ]
    ] []-}
  , Character
    "Konohamaru Sarutobi"
    "The grandson of the Third Hokage, Konohamaru has spent his youth working hard to pursue his dream of one day following in his grandfather's steps. No longer a bumbling student, Konohamaru has become remarkably skillful as a genin. Agile and fast, he can rush in to save an ally on the brink of defeat."
    [ [ Skill.new
        { Skill.name      = "Rasengan"
        , Skill.desc      = "Focusing on an enemy, Konohamaru takes his time to prepare his Rasengan. Next turn, it will deal 25 damage to the target and an additional 15 if they used a skill."
        , Skill.classes   = [Chakra, Melee, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                trap (-1) (OnAction All) flag
                delay (-1) do
                    bonus <- 15 `bonusIf` targetHas "Rasengan"
                    pierce (25 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Agile Backflip"
        , Skill.desc      = "Konohamaru uses his agility to counter the next non-mental skill used on him. Each time this skill is used, its cost increases by 1 random. Cannot be used while active."
        , Skill.classes   = [Physical, Invisible, Single]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                trapFrom 0 (Counter NonMental) $ return ()
                hide' "tired" 0 []
          ]
        , Skill.changes   = costPer "tired" [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Quick Recovery"
        , Skill.desc      = "In the nick of time, Konohamaru rushes to an ally's rescue. The first time the target's health reaches 0 next turn, they will regain 15 health."
        , Skill.classes   = [Physical, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly $ trap 1 OnRes $ setHealth 15 ]
        }
      ]
    , [ invuln "Hide" "Konohamaru" [Mental] ]
    ] []
  ]
