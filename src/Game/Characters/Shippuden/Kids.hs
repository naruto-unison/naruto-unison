{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Shippuden.Kids (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
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
        , Skill.desc      = "Deals 50 piercing damage. Deals 25 additional damage if [Multi Shadow Clone] countered the target last turn."
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
        , Skill.desc      = "Naruto creates countless clones, hidden in the area around him, who counter the first skill an enemy uses on him in the next turn."
        , Skill.classes   = [Physical, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                trapFrom 1 (Counter All) $ tag 1
                vary' 1 "Giant Rasengan" "Rasenshuriken"
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
                gain [Nin, Tai]
                apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ invuln "Shadow Clone Save" "Naruto" [Chakra] ]
    ]
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
        , Skill.desc     = "Using advanced healing techniques, Sakura restores half of an ally's missing health and cures the target of bane effects. Spends a Seal if available to have no cooldown and cost 1 arbitrary chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly do
                cureBane
                targetHealth <- target health
                heal $ (100 - targetHealth) `quot` 2
          ]
        }
      , Skill.new
        { Skill.name     = "Mystical Palm Healing"
        , Skill.desc     = "Using advanced healing techniques, Sakura restores half of an ally's missing health and cures the target of bane effects. Spends a Seal if available to have no cooldown and cost 1 arbitrary chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand]
        , Skill.varicd    = True
        , Skill.effects   =
          [ To XAlly do
                cureBane
                targetHealth <- target health
                heal $ (100 - targetHealth) `quot` 2
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
    ]
  , Character
    "Sasuke Uchiha"
    "Sasuke's years of training under Orochimaru have made him a master of his elemental aspects. Cold and ruthless, he is regarded as one of the most dangerous ninjas alive."
    [ [ Skill.new
        { Skill.name      = "Chidori Stream"
        , Skill.desc      = "An electric field surrounds Sasuke. Enemies who use non-mental skills on Sasuke next turn will be countered and will take 10 affliction damage. Once used, this skill becomes [Chidori Spear][t]."
        , Skill.classes   = [Bane, Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 30
          , To Self  $ vary "Chidori Stream" "Chidori Spear"
          ]
        }
      , Skill.new
        { Skill.name      = "Chidori Spear"
        , Skill.desc      = "Employing his swordsmanship to wield an energy beam like a weapon, Sasuke deals 15 damage to an enemy and stuns them with electricity for 1 turn. Once used, this skill becomes [Chidori Stream][n][r]."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 1 [Stun All]
          , To Self $ vary "Chidori Stream" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dragon Flame"
        , Skill.desc      = "Draconic fireballs sear an enemy, dealing 10 damage to them for 4 turns. While active, enemies who use skills on Sasuke will take 5 affliction damage."
        , Skill.classes   = [Chakra, Ranged, Bane]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 4
        , Skill.effects   =
          [ To Enemy $ damage 10
          , To Self  $ trapFrom 1 (OnHarmed All) $ afflict 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kirin"
        , Skill.desc      = "A pillar of lightning strikes an enemy, dealing 45 piercing damage."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 45 ]
        }
      ]
    , [ invuln "Summoning: Serpent" "Sasuke" [Summon] ]
    ]
  , Character
    "Kiba Inuzuka"
    "Kiba's years with Akamaru have enhanced their bond and teamwork. Now a chūnin, he has learned the alarming ability to transform Akamaru into a bestial humanoid resembling Kiba. As they progress through several stages of shapeshifting, they gradually transform into unstoppable rampaging beasts."
    [ [ Skill.new
        { Skill.name      = "Man-Beast Clone"
        , Skill.desc      = "Akamaru transforms into a bestial copy of Kiba, providing 15 points of damage reduction to Kiba for 4 turns and causing him to ignore stuns and disabling effects. While active, this skill becomes [Three-Headed Wolf][b][b]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 4
        , Skill.start     =
          [ To Self $ vary "Man-Beast Clone" "Three-Headed Wolf"]
        , Skill.effects   =
          [ To Self $ apply 1 [Focus, Reduce All Flat 15] ]
        }
      , Skill.new
        { Skill.name      = "Three-Headed Wolf"
        , Skill.desc      = "Akamaru and Kiba fuse together, ending [Man-Beast Clone]. For 3 turns, Kiba gains 30 points of damage reduction and ignores stuns and disabling effects. While active, this skill becomes [Giant Rotating Fang][t][b][b]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                cancelChannel "Man-Beast Clone"
                vary' 3 "Man-Beast Clone" "Giant Rotating Fang"
                remove "Man-Beast Clone"
                apply 3 [Reduce All Flat 30]
          ]
        }
      , Skill.new
        { Skill.name      = "Tail Chasing Rotating Fang"
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
        , Skill.desc      = "Kiba launches attack after attack on an enemy, dealing 40 damage to them and permanently lowering their damage by 10. Deals 10 additional damage during [Man-Beast Clone]. Deals 20 additional damage during [Three-Headed Wolf]."
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
    ]
  , Character
    "Shino Aburame"
    "Shino's years of practice with his loyal bugs have deepened his connection with them. Having attained the rank of chūnin, Shino has learned to breed his insects to favor specific traits. His advanced parasites accumulate invisibly in targets before bursting out all at once."
    [ [ Skill.new
        { Skill.name       = "Insect Swarm"
        , Skill.desc       = "A wave of insects attack an enemy, dealing 15 affliction damage to them for 3 turns and making them invulnerable to allies. While active, this skill becomes [Chakra Leech]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.dur       = Action 3
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
                apply 1 [Alone]
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
        , Skill.desc      = "Colonies of insects hide around Shino or one of his allies, countering the first skill an enemy uses on them in the next turn. If an enemy is countered, [Gigantic Beetle Infestation] is applied to them and activated. If no enemies are countered, Shino gains a bloodline chakra."
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
    ]
  , Character
    "Hinata Hyūga"
    "With the Chūnin Exam behind her and Naruto's words deep in her heart, Hinata has grown and become stronger. Now that she has mastered the Hyūga clan tactics, she can give life to powerful chakra lions and hinder the chakra paths of her enemies."
    [ [ Skill.new
        { Skill.name      = "Pressure Point Strike"
        , Skill.desc      = "Hinata blocks an enemy's pressure point, dealing 10 damage to them and increasing the costs of their skills by 1 arbitrary chakra for 1 turn. Deals 10 additional damage during [Eight Trigrams Sixty-Four Palms]."
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
        , Skill.desc      = "Hinata creates two lions out of chakra. The next 2 times an enemy uses a skill on Hinata or her allies, a chakra lion will attack them, dealing 30 damage and depleting 1 random chakra. Creates a third lion during [Eight Trigrams Sixty-Four Palms]. Cannot be used while active. Ends if Hinata dies."
        , Skill.require   = HasI 0 "Chakra Lion"
        , Skill.classes   = [Chakra, Melee, Bypassing, Soulbound, Resource]
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
        , Skill.desc      = "For 4 turns, each time an enemy affected by [Pressure Point Strike] uses a skill on Hinata or her allies, Hinata's next [Pressure Point Strike] will last 1 additional turn on them."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self    $ tag 4
          , To Enemies $ trap 4 OnHarm addStack
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Hinata" [Mental] ]
    ]
  , Character
    "Shikamaru Nara"
    "Once known for his laziness, Shikamaru has worked tirelessly to become a leader. With years of experience, his plans have become even more convoluted and intricate."
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
        , Skill.require   = HasU 1 "Shadow Sewing"
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
        , Skill.desc      = "Shikamaru goes long. For 4 turns, each time Shikamaru uses a skill on an enemy, he will become invulnerable for 1 turn. However, if an enemy uses a skill that deals non-affliction damage to him, he will not become invulnerable during the next turn. While active, this skill becomes [Final Explosion][r][r]."
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
        , Skill.require   = HasI 0 "What a Drag"
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
    ]
  , Character
    "Chōji Akimichi"
    "Chōji's years of mastering his clan's techniques have ended the growing chūnin's dependence on Akimichi pills. Now that he can reshape his body at will without having to sacrifice his health, chakra expenditure is the only remaining limit to his physical power."
    [ [ Skill.new
        { Skill.name      = "Butterfly Bombing"
        , Skill.desc      = "Chōji charges at an enemy for 1 turn, ignoring status effects from enemies except chakra cost changes. At the end of the turn, he deals 30 damage to the target. Increases the costs of Chōji's skills by 2 arbitrary chakra."
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
        , Skill.desc      = "Chōji charges at an enemy for 1 turn, ignoring status effects from enemies except chakra cost changes. At the end of the turn, he deals 30 damage to the target. Increases the costs of Chōji's skills by 2 arbitrary chakra."
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
        , Skill.desc      = "Chōji rolls into a ball bristling with needle-like spikes and deals 15 damage to an enemy for 2 turns. While active, Chōji counters non-mental skills. Increases the cost of Chōji's skills by 1 arbitrary chakra each turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Rand, Rand]
        , Skill.dur       = Action 2
        , Skill.start     =
          [ To Enemy $ damage 15
          , To Self do
                trap 2 (CounterAll NonMental) $ return ()
                hide' "calories" 0 [Exhaust All]
          ]
        }
      , Skill.new
        { Skill.name      = "Spiky Human Boulder"
        , Skill.desc      = "Chōji rolls into a ball bristling with needle-like spikes and deals 15 damage to an enemy for 2 turns. While active, Chōji counters non-mental skills. Increases the cost of Chōji's skills by 1 arbitrary chakra each turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.dur       = Action 2
        , Skill.start     =
          [ To Enemy $ damage 15
          ,  To Self do
                trap 2 (CounterAll NonMental) $ return ()
                hide' "calories" 0 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Butterfly Mode"
        , Skill.desc      = "Performing an advanced Akimichi technique that would be lethal without precise control over his body, Chōji converts calories into jets of chakra energy that grow from his back like butterfly wings. Once used, this skill permanently becomes [Super-Slam][n][r][r]. Each turn after [Butterfly Mode] is activated, the costs of Chōji's skills decrease by 1 arbitrary chakra."
        , Skill.classes   = [Chakra]
        , Skill.dur       = Ongoing 0
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
        , Skill.desc      = "Chōji funnels chakra into his hands until they are as powerful as iron jackhammers and slams them into an enemy, dealing 30 damage and curing Chōji of enemy effects. Increases the cost of Chōji's skills by 2 arbitrary chakra."
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
    ]
  , Character
    "Ino Yamanaka"
    "Now a chūnin, Ino takes control of every fight she faces. Her overpowering will steals the skills and secrets of her enemies and forces her allies to fight on no matter the cost. "
    [ [ Skill.new
        { Skill.name      = "Mind Destruction"
        , Skill.desc      = "Ino infiltrates an enemy's mind and prepares to strike at a moment of weakness. Next turn, the target receives 15 damage. If they use a skill on Ino or her allies next turn, they will be countered and this skill will be replaced by that skill for 1 turn. Ino's copy of their skill has no chakra cost and ends when this skill reverts."
        , Skill.classes   = [Mental, Ranged, Invisible, Unreflectable, Unremovable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                trap (-1) (Countered All) $ copyLast 1
                delay (-1) $ damage 15
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Proxy Surveillance"
        , Skill.desc      = "Ino's will takes over the battlefield. For 3 turns, she detects invisible effects and enemy cooldowns. While active, the enemy team's damage reduction skills and destructible defense skills are reduced by 15. If an enemy uses a skill with negative damage reduction, damage to them is increased by its amount. If they use a skill with negative destructible defense, their target is damaged for its amount. If they use a skill with negative destructible barrier, they are damaged for its amount."
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
    ]
  , Character
    "Rock Lee"
    "Lee's years of training with Gai have taught him not only new abilities, but what it truly means to fight. His strength grows as his allies fall, determined to honor them by finishing their battle."
    [ [ Skill.new
        { Skill.name      = "Leaf Rising Wind"
        , Skill.desc      = "Lee plants his back on the ground and uses his entire body as a spring to kick an enemy with such power that they are launched into the air, dealing 15 damage and lowering the target's damage by 15 for 2 turns. Deals 10 additional damage per dead ally. Effect lasts 1 additional turn per dead ally."
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
    ]
  , let loadout = [0, 0, 0]
    in Character
    "Tenten"
    "Now a chūnin, Tenten's arsenal has expanded to a prodigious stockpile of some of the most powerful weapons in existence, including the legendary fan of the Sage of the Six Paths. Taking any excuse to show off the size and variety of her collection, she has assembled multiple item sets to switch out at a moment's notice."
    [ [ Skill.new
        { Skill.name      = "Kunai Grenade"
        , Skill.desc      = "Tenten throws an explosive filled with a frankly ridiculous number of kunai at an enemy, dealing 20 damage to them and 10 damage to the rest of their team."
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
        , Skill.desc      = "Using the Sage of the Six Path's legendary battle fan, Tenten throws a gust of wind that reflects all skills used on her or her allies next turn."
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
    ]
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
        , Skill.desc      = "For 2 turns, enemies are prevented from reducing damage or becoming invulnerable. If an enemy uses a skill on Neji during the first turn, it is countered and this skill is replaced for 1 turn by [Pressure Point Strike]."
        , Skill.classes   = [Physical, Mental, Invisible]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ apply 2 [Expose]
          , To Self    $ trap 1 (Counter All) $
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
    ]
  , Character
    "Kazekage Gaara"
    "Gaara's years of soul-searching have made him ready to assume the title of Kazekage. Now a powerful force for good, he devotes himself to protecting his friends and the Hidden Sand Village."
    [ [ Skill.new
        { Skill.name      = "Monstrous Sand Arm"
        , Skill.desc      = "Gaara shapes sand into an enormous hand that slams into an enemy, dealing 5 piercing damage and weakening their damage by 10 for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy do
                pierce 5
                apply 1 [Weaken All Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Prison"
        , Skill.desc      = "Crushing ropes of sand constrict into an airtight prison around an enemy, dealing 10 damage to them and stunning their chakra and ranged skills for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 10
                apply 1 [Stun Chakra, Stun Ranged]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Summoning"
        , Skill.desc      = "Gaara transforms the battlefield into a desert, providing 15 permanent destructible defense to his allies. The first use of this skill also causes all of Gaara's damage to be multiplied by 3. The second use of this skill also provides Gaara with 10 points of damage reduction and causes all of his damage to be multiplied by 5."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.charges   = 2
        , Skill.effects   =
          [ To XAllies $ defend 0 15
          , To Self do
                has <- userHas "Sand Summoning"
                if has then do
                    remove "Sand Summoning"
                    apply 0 [Strengthen All Percent 500, Reduce All Flat 10]
                else
                    apply 0 [Strengthen All Percent 300]

          ]
        }
      ]
    , [ invuln "Levitating Sand Shield" "Gaara" [Physical] ]
    ]
  , Character
    "Kankurō"
    "Now a jōnin, Kankurō has crafted a third puppet for his collection and honed his skills as a puppetmaster. Each puppet has its own use, improving his overall versatility."
    [ [ Skill.new
        { Skill.name      = "Kuroari Trap"
        , Skill.desc      = "The Kuroari puppet traps an enemy. If they use a skill on Kankurō or his allies next turn, they will be countered and will receive twice as much damage from [Karasu Knives] for 1 turn."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy $ trap (-1) (Countered All) $ tag 1 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Karasu Knives"
        , Skill.desc      = "The Karasu puppet shoots poisoned knives at an enemy, dealing 20 damage to them. Next turn, the target takes 10 affliction damage."
        , Skill.classes   = [Physical, Ranged, Bane]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 20 `bonusIf` targetHas "Kuroari Trap"
                damage (20 + bonus)
                bomb 1 [] [ To Expire do
                    bonus' <- 10 `bonusIf` targetHas "Kuroari Trap"
                    afflict (10 + bonus') ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sanshōuo Shield"
        , Skill.desc      = "The Sanshōuo puppet blocks enemy attacks for 3 turns, providing 25% damage reduction to Kankuro and his allies and making them immune to affliction damage. While active, this skill becomes [Salamander Puppet]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand, Rand]
        , Skill.dur       = Action 3
        , Skill.cooldown  = 3
        , Skill.start     =
          [ To Self $ vary "Sanshōuo Shield" "Salamander Puppet" ]
        , Skill.effects   =
          [ To Allies $
              apply 1 [Reduce All Percent 25, Invulnerable Affliction]
          ]
        }
      , Skill.new
        { Skill.name      = "Salamander Puppet"
        , Skill.desc      = "The Sanshōuo puppet focuses its defense on Kankurō or one of his allies, providing them with 25% additional damage reduction for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ To Ally $ apply 1 [Reduce All Percent 25] ]
        }
      ]
    , [ invuln "Puppet Distraction" "Kankurō" [Physical] ]
    ]
  , Character
    "Temari"
    "The Hidden Sand Village's official ambassador, Temari is a formidable jōnin who wields an equally formidable battle fan. She defends her team with chakra-infused whirlwinds that deflect attacks, and uses the metal fan itself to block anything that gets through."
    [ [ Skill.new
        { Skill.name      = "First Moon"
        , Skill.desc      = "Snapping open her fan to reveal the first marking on it, Temari gains 25% damage reduction. Once used, this skill becomes [Second Moon][r]."
        , Skill.classes   = [Physical, Ranged, Unremovable]
        , Skill.effects   =
          [ To Self do
                apply 0 [Reduce All Percent 25]
                vary "First Moon" "Second Moon"
          ]
        }
      , Skill.new
        { Skill.name      = "Second Moon"
        , Skill.desc      = "Snapping open her fan to reveal the second marking, Temari gains 25% additional damage reduction. Once used, this skill becomes [Third Moon][n][r]."
        , Skill.classes   = [Physical, Ranged, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                remove "First Moon"
                apply 0 [Reduce All Percent 50]
                vary "First Moon" "Third Moon"
          ]
        }
      , Skill.new
        { Skill.name      = "Third Moon"
        , Skill.desc      = "Fully opening her fan, Temari deals 20 damage to all enemies and weakens their damage by 5 for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                damage 20
                apply 1 [Weaken All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gale Raging Wall"
        , Skill.desc      = "Temari defends an ally behind a wall of wind, providing them with 50% damage reduction and preventing their health from dropping below 1."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To XAlly $ apply 1 [Reduce All Percent 50, Endure] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Cyclone Scythe"
        , Skill.desc      = "Temari strikes an enemy with a gust of wind, dealing 20 damage. Deals 5 additional damage during [First Moon]. Deals 10 additional damage during [Second Moon]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                bonusFirst  <- 5  `bonusIf` userHas "First Moon"
                bonusSecond <- 10 `bonusIf` userHas "Second Moon"
                damage (20 + bonusFirst + bonusSecond)
          ]
        }
      ]
    , [ invuln "Block" "Temari" [Physical] ]
    ]
  , Character
    "Kabuto Yakushi"
    "A dangerous rogue operative from the Hidden Leaf Village, Kabuto is a calculating follower of Orochimaru whose healing expertise goes beyond the limits of medical techniques to outright necromancy."
    [ [ Skill.new
        { Skill.name      = "Chakra Absorbing Snakes"
        , Skill.desc      = "Black snakes wrap around an enemy, dealing 20 damage. For 1 turn, if the target restores health to anyone, the snakes will constrict and stun them for 2 turns."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                trap 1 OnHeal $ apply 2 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      =  "Striking Shadow Snakes"
        , Skill.desc      = "Numerous poisonous snakes attack an enemy, dealing 35 piercing damage. For 2 turns, the target cannot be healed or cured."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 35
                apply 2 [Plague]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Healing Technique"
        , Skill.desc      = "Using advanced medical techniques, Kabuto restores 25 health to himself or an ally and cures the target of bane effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Ally do
                heal 25
                cureBane
          ]
        }
      ]
    , [ invuln "Dodge" "Kabuto" [Physical] ]
    ]
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
        , Skill.desc      = "Konohamaru uses his agility to counter the next non-mental skill used on him. Each time this skill is used, its cost increases by 1 arbitrary chakra."
        , Skill.classes   = [Physical, Invisible, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                trap 0 (Counter NonMental) $ return ()
                hide' "tired" 0 []
          ]
        , Skill.changes   =
            costPer "tired" [Rand]
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
    ]
  ]
