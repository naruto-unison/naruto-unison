{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden.Kids (kidCsS) where

import Game.Functions
import Game.Game
import Game.Structure

kidCsS ∷ [Character]
kidCsS =
  [ Character
    "Naruto Uzumaki"
    "Naruto's years of training under Jiraiya have made him stronger, wiser, and far more effective at controlling his immense chakra. He has learned how to distribute his chakra efficiently across numerous shadow clones, and can harness the flow of energy within himself to transform and repurpose chakra."
    [ [ newSkill
        { label   = "Giant Rasengan"
        , desc    = "Naruto instantly creates several shadow clones to aid him in wielding the Rasengan. Infused with highly compressed chakra, the Rasengan blasts through his enemy's guard and deals 40 damage. If [Multi Shadow Clone] was used last turn, this skill becomes [Rasenshuriken][n][t]."
        , classes = [Chakra, Melee, Bypassing]
        , cost    = χ [Nin, Tai]
        , cd      = 1
        , effects = [(Enemy, damage 40)]
        }
      , newSkill
        { label   = "Rasenshuriken"
        , desc    = "Deals 50 piercing damage. Deals 25 additional damage if the target was countered by [Multi Shadow Clone] last turn."
        , classes = [Chakra, Melee, Bypassing]
        , cost    = χ [Nin, Tai]
        , cd      = 1
        , effects = [(Enemy, withU "Multi Shadow Clone" 25 pierce 50)]
        }
      ]
    , [ newSkill
        { label   = "Multi Shadow Clone"
        , desc    = "Naruto creates countless clones hidden in the area around him, who counter the first harmful skill used on him in the next turn."
        , classes = [Physical, Invisible]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(Self, apply 1 [Parry All 4] • vary 1 0 1)]
        }
      ]
    , [ newSkill
        { label   = "Chakra Boost"
        , desc    = "Naruto cycles his chakra to transform 2 chakra of any type into 1 ninjutsu chakra and 1 taijutsu chakra. The flow of power cures him of enemy effects and provides 10 points of damage reduction for 1 turn."
        , classes = [Chakra]
        , cost    = χ [Rand, Rand]
        , cd      = 3
        , effects = [(Self, cureAll • gain [ Nin, Tai] 
                          • apply 1 [Reduce All 10])]
        }
      ]
    , invuln "Shadow Clone Save" "Naruto" [Chakra]
    , [ newSkill
        { label   = "Multi Shadow Clone"
        , classes = [Physical]
        , effects = [(Enemy, tag 1)]
        }
      ]
    ] []
  , Character
    "Sakura Haruno"
    "Sakura's years of training under Tsunade have provided her with an intricate understanding of healing and the human body. Now a chūnin, she has learned how to store her chakra in concentrated points and then unleash it in empowering waves."
    [ [ newSkill
        { label   = "Cherry Blossom Clash"
        , desc    = "Sakura expertly condenses her chakra into her fist and punches an enemy, dealing 25 damage. Spends a Seal if available to deal 10 additional damage to all enemies."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (XEnemies, ifI "Seal"    § damage 10)
                    , (Enemy,    ifnotI "Seal" § damage 25
                               • ifI "Seal"    § damage 35)
                    , (Self,     removeStack "Seal" 
                               • ifnotI "Seal" § vary 0 1 0 ° vary 0 2 0)
                    ]
        }
      ]
    , [ newSkill
        { label  = "Mystical Palm Healing"
        , desc   = "Using advanced healing techniques, Sakura restores half of an ally's missing health and cures them of baneful effects. Spends a Seal if available to have no cooldown and cost 1 random chakra."
        , classes = [Chakra]
        , cost    = χ [Nin]
        , cd      = 2
        , effects = [(XAlly, cureBane • restore 50)]
        }
      , newSkill
        { label  = "Mystical Palm Healing"
        , desc   = "Using advanced healing techniques, Sakura restores half of an ally's missing health and cures them of baneful effects. Spends a Seal if available to have no cooldown and cost 1 random chakra."
        , classes = [Chakra]
        , cost    = χ [Rand]
        , varicd  = True
        , effects = [ (XAlly, cureBane • restore 50)
                    , (Self,  removeStack "Seal" 
                            • ifnotI "Seal" § vary 0 1 0 ° vary 0 2 0)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Strength of One Hundred Seal"
        , desc    = "Sakura stores up chakra in a point on her forehead, gaining 3 Seals. Sakura's skills spend Seals to become empowered. While active, this skill becomes [Seal Release]."
        , classes = [Chakra]
        , cost    = χ [Rand]
        , effects = [(Self, addStacks "Seal" 3 • vary 0 1 1 • vary 0 2 1)]
        }
      , newSkill
        { label   = "Seal Release"
        , desc    = "Spends a Seal to restore 25 health to Sakura and cure her of enemy effects."
        , classes = [Chakra]
        , effects = [(Self, cureAll • heal 25 • removeStack "Seal" 
                          • ifnotI "Seal" § vary 0 1 0 ° vary 0 2 0)]
        }
      ]
      , invuln "Dodge" "Sakura" [Physical]
    ] []
  , Character
    "Sasuke Uchiha"
    "Sasuke's years of training under Orochimaru have made him a master of his elemental aspects. Now that he has absorbed Orochimaru, he has added the sannin's snake abilities to his own lightning attacks, which pierce through the defenses of his enemies. He is regarded as one of the most dangerous and ruthless ninjas alive."
    [ [ newSkill
        { label   = "Lightning Flash"
        , desc    = "Sasuke infuses a spinning shuriken with Chidori and hurls it at an enemy, dealing 30 piercing damage and leaving a trail of electricity that briefly links Sasuke to his enemies."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, pierce 30) 
                    , (Self,  tag 1)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Shadow Shuriken"
        , desc    = "Sasuke throws two shuriken rigged with electric wires at an enemy, concealing the lower blade in the shadow of the upper one. Deals 20 piercing damage and an additional 5 if [Lightning Flash] was used last turn. If the target uses a ranged skill next turn, they will receive 10 piercing damage."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [ (Enemy, withI "Lightning Flash" 5 damage 20
                            • trap (-1) (OnAction Ranged) § pierce 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Kirin"
        , desc    = "Sasuke calls down a pillar of lightning from the sky upon an enemy, dealing 40 piercing damage. If [Lightning Flash] was used last turn, this skill bypasses invulnerability and deals 40 affliction damage instead."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Nin]
        , cd      = 3
        , effects = [(Enemy, ifI "Lightning Flash"    § pierce 40
                           • ifnotI "Lightning Flash" § afflict 40)]
        , changes = changeWith "Lightning Flash" $ addClass Bypassing
        }
      ]
    , invuln "Snake Shedding" "Sasuke" [Physical]
    ] []
  , Character
    "Kiba Inuzuka"
    "Kiba's years with Akamaru have enhanced their bond and teamwork. Now a chūnin, he has learned the alarming ability to transform Akamaru into a bestial humanoid resembling Kiba. As they progress through several stages of shapeshifting, they gradually transform into unstoppable rampaging beasts."
    [ [ newSkill
        { label   = "Man-Beast Clone"
        , desc    = "Akamaru transforms into a bestial copy of Kiba, providing 15 points of damage reduction to Kiba for 4 turns and causing him to ignore stuns. While active, this skill becomes [Three-Headed Wolf][b][b]."
        , classes = [Physical]
        , cost    = χ [Rand]
        , cd      = 4
        , channel = Action 4
        , start   = [(Self, vary' 0 1)]
        , effects = [(Self, apply 1 [Focus, Reduce All 15])]
        }
      , newSkill
        { label   = "Three-Headed Wolf"
        , desc    = "Akamaru and Kiba fuse together, ending [Man-Beast Clone]. For 3 turns, Kiba gains 30 points of damage reduction and ignores stuns. While active, this skill becomes [Giant Rotating Fang][t][b][b]."
        , classes = [Physical]
        , cost    = χ [Blood, Blood]
        , cd      = 5
        , effects = [ (Self, cancelChannel "Man-Beast Clone" • vary 3 0 2
                           • remove "Man-Beast Clone" 
                           • apply 3 [Focus, Reduce All 30])
                    ]
        }
      , newSkill
        { label   = "Giant Rotating Fang"
        , desc    = "Deals 40 damage to all enemies and stuns them for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Blood, Tai]
        , cd      = 1
        , effects = [(Enemies, damage 40 • apply 1 [Stun All])]
        }
      ]
    , [ newSkill
        { label   = "Rotating Fang"
        , desc    = "Spinning like a buzzsaw, Kiba deals 30 damage to an enemy. Deals 20 damage to the rest of their team during [Three-Headed Wolf]. Deals 20 additional damage to a random enemy during [Man-Beast Clone]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [ (Enemy,    damage 30)
                    , (XEnemies, ifI "Three-Headed Wolf" § damage 20)
                    , (REnemy,   ifChan "Man-Beast Clone" § damage 20)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Fang Over Fang"
        , desc    = "Kiba launches attack after attack on an enemy, dealing 40 damage to them and permanently lowering their non-affliction damage by 10. Deals 10 additional damage during [Man-Beast Clone]. Deals 20 additional damage during [Three-Headed Wolf]."
        , classes = [Physical, Melee, Multi]
        , cost    = χ [Blood, Tai]
        , cd      = 1
        , effects = [(Enemy, apply 0 [Weaken All 10] 
                           • withI "Man-Beast Clone" 10
                            (withI "Three-Headed Wolf" 20 damage) 40)]
        }
      ]
    , invuln "Hide" "Kiba" [Mental]
    ] []
  , Character
    "Shino Aburame"
    "Shino's years of practice with his loyal bugs have deepened his connection with them. Having attained the rank of chūnin, Shino has learned to breed his insects to favor specific traits. His advanced parasites accumulate invisibly in targets before bursting out all at once."
    [ [ newSkill
        { label    = "Insect Swarm"
        , desc     = "Shino sends a wave of insects at an enemy, dealing 15 affliction damage to them for 3 turns and making them immune to effects from allies. While active, this skill becomes [Chakra Leech]."
        , classes = [Ranged, Multi]
        , cost    = χ [Blood, Rand]
        , channel = Action 3
        , cd      = 2
        , start   = [ (Self,  vary' 0 1) 
                    , (Enemy, perU "Chakra Leech" 1 
                              (addStacks' (-3) "Chakra Leech ") 0
                            • remove "Chakra Leech")
                    ]
        , effects = [(Enemy, perU "Chakra Leech " 5 afflict 15 
                            • apply 1 [Seal])]
        }
      , newSkill
        { label    = "Chakra Leech"
        , desc     = "Removes a random chakra from an enemy and adds 5 damage to the next [Insect Swarm] on them."
        , classes  = [Bane, Physical, Ranged]
        , effects  = [(Enemy, drain 1 • apply 0 [])]
        }
      ]
    , [ newSkill
        { label   = "Insect Barricade"
        , desc    = "Colonies of insects hide around Shino or one of his allies, countering the first harmful skill used against them in the next turn. If an enemy is countered, [Gigantic Beetle Infestation] is applied to them and activated. If no enemies are countered, Shino gains a bloodline chakra."
        , classes = [Melee, Invisible, Unreflectable]
        , cost    = χ [Blood]
        , cd      = 2
        , effects = [ (Self, bomb' "Barricaded" (-1) [] 
                             [(Expire, self § gain [Blood])])
                    , (Ally, apply 1 [Parry All 4])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Gigantic Beetle Infestation"
        , desc    = "Shino infects an enemy with a small bug. In 3 turns, the bug will burst out, dealing 25 damage and activating all other copies of this skill on the target."
        , classes = [Bane, Melee, Invisible]
        , cost    = χ [Blood]
        , effects = [(Enemy, bomb 3 []
                             [(Expire, perU "Gigantic Beetle Infestation" 
                                       25 damage 25
                                     • remove "Gigantic Beetle Infestation"
                                     • remove "Chakra Leech")])]
        }
      ]
    , invuln "Insect Cocoon" "Shino" [Physical]
    , [ newSkill
        { label   = "Insect Barricade"
        , classes = [Melee]
        , effects = [ (Enemy, perU "Gigantic Beetle Infestation" 25 damage 25
                            • remove "Gigantic Beetle Infestation"
                            • remove "Chakra Leech")
                    , (Ally,  remove "Insect Barricade")
                    , (Self,  remove "Barricaded")
                    ]
        }
      ]
    ] []
  , Character
    "Hinata Hyūga"
    "With the Chūnin Exam behind her and Naruto's words deep in her heart, Hinata has grown and become stronger. Now that she has mastered the Hyūga clan tactics, she can give life to powerful chakra lions and hinder the chakra paths of her enemies."
    [ [ newSkill
        { label   = "Pressure Point Strike"
        , desc    = "Hinata closes an enemy's pressure point, dealing 10 damage to them and increasing the costs of their skills by 1 random for 1 turn. Deals 10 additional damage during [Eight Trigrams Sixty-Four Palms]."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [ (Enemy, withI "Eight Trigrams Sixty-Four Palms" 
                              10 damage 10 
                            • apply 1 [Exhaust All]
                            • perU "Eight Trigrams Sixty-Four Palms" 1 
                              (applyDur [Exhaust All]) 1
                            • remove "Eight Trigrams Sixty-Four Palms")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Gentle Step Twin Lion Fists"
        , desc    = "Hinata creates two lions out of chakra. The next 2 times an enemy uses a harmful skill, a chakra lion will attack them, dealing 30 damage and removing a random chakra. Creates a third lion during [Eight Trigrams Sixty-Four Palms]. Cannot be used while active. Ends if Hinata dies."
        , classes = [Chakra, Melee, Soulbound, Single]
        , cost    = χ [Blood, Nin]
        , effects = [ (Self,    addStacks "Chakra Lion" 2
                              • ifI "Eight Trigrams Sixty-Four Palms" 
                              § addStacks "Chakra Lion" 1)
                    , (Enemies, trap' 0 OnHarm 
                              § (self § removeStack "Chakra Lion")
                              ° (ifI "Chakra Lion" § drain 1 • damage 30)
                              ° (ifnotI "Chakra Lion"
                              § removeTrap "Gentle Step Twin Lion Fists"))
                    ]
        }
      ]
    , [ newSkill
        { label   = "Eight Trigrams Sixty-Four Palms"
        , desc    = "For 4 turns, each time an enemy affected by [Pressure Point Strike] uses a harmful skill, Hinata's next [Pressure Point Strike] will last an additional turn on them."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , effects = [ (Self,    tag 4)
                    , (Enemies, trap 4 OnHarm § addStack)
                    ]
        }
      ]
    , invuln "Byakugan Foresight" "Hinata" [Mental]
    ] []
  , Character
    "Shikamaru Nara"
    "Once known for his laziniess, Shikamaru has worked tirelessly to become a leader. With years of experience, his plans have become even more convoluted and intricate."
    [ [ newSkill
        { label   = "Shadow Sewing"
        , desc    = "Shikamaru wraps an enemy in delicate tendrils of shadow, dealing 35 damage and stunning their non-mental skills for 1 turn. While active, this skill becomes [Shadow Sewing: Hold][g]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen, Rand]
        , cd      = 1
        , effects = [(Enemy, damage 35 • apply 1 [Stun NonMental]
                           • self § vary 1 0 1)]
        }
      , newSkill
        { label   = "Shadow Sewing: Hold"
        , desc    = "Deals 20 damage to an enemy affected by [Shadow Sewing] and prolongs its stun by 1 turn."
        , require = HasU "Shadow Sewing"
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen]
        , effects = [(Enemies, damage 20
                             • apply' "Shadow Sewing" 1 [Stun NonMental]
                             • self § vary 1 0 1)]
        }
      ]
    , [ newSkill
        { label   = "Long-Range Tactics"
        , desc    = "Shikamaru goes long. For 4 turns, each time Shikamaru uses a new harmful skill, if he did not receive new non-affliction damage last turn, he will become invulnerable for 1 turn. While active, this skill becomes [Final Explosion][r][r]."
        , classes = [Physical]
        , cost    = χ [Tai]
        , cd      = 5
        , effects = [(Self, tag 4 • vary 4 1 1
                          • delay (-1) § trap' (-4) OnHarm
                            (ifnotI "What a Drag" § apply 1 [Immune All])
                          • trap' 4 (OnDamaged NonAffliction) 
                            (tag' "What a Drag" 1))]
        }
      , newSkill
        { label   = "Final Explosion"
        , desc    = "Deals 100 damage to an enemy affected by [Shadow Sewing] or [Expert Analysis]. Cannot be used if Shikamaru received new non-affliction damage last turn."
        , require = HasI (-1) "What a Drag"
        , classes = [Physical]
        , cost    = χ [Tai]
        , effects = [(Enemy, ifU "Shadow Sewing" § damage 10
                           • ifnotU "Shadow Sewing" 
                             § ifU "Expert Analysis" § damage 10)]
        }
      ]
    , [ newSkill
        { label   = "Expert Analysis"
        , desc    = "Shikamaru carefully analyzes an enemy to discern their weaknesses. For 3 turns, any time the target uses a skill, they will be prevented from becoming invulnerable, reducing damage, or benefiting from counters and reflects for 1 turn."
        , classes = [Mental, Ranged]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(Enemy, trap (-3) (OnAction All) 
                             § apply 1 [Expose, Uncounter])]
        }
      ]
    , invuln "Dodge" "Shikamaru" [Physical]
    ] []
  , Character
    "Chōji Akimichi"
    "Chōji's years of mastering his clan's techniques have ended the growing chūnin's dependence on Akimichi pills. Now that he can reshape his body at will without having to sacrifice his health, chakra costs are the only remaining limits on his physical power."
    [ [ newSkill
        { label   = "Butterfly Bombing"
        , desc    = "Chōji charges at an enemy for 1 turn, ignoring harmful non-damage effects other than chakra cost changes. At the end of the turn, he deals 30 damage to the target. Increases the costs of Chōji's skills by 2 random chakra."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand, Rand]
        , effects = [ (Enemy, delay (-1) § damage 30)
                    , (Self,  apply 1 [Enrage]
                            • hide' "calories" 0 [Exhaust All] 
                            • hide' "calories" 0 [Exhaust All])
                    ]
        }
      , newSkill
        { label   = "Butterfly Bombing"
        , desc    = "Chōji charges at an enemy for 1 turn, ignoring harmful non-damage effects other than chakra cost changes. At the end of the turn, he deals 30 damage to the target. Increases the costs of Chōji's skills by 2 random chakra."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy, delay (-1) § damage 30)
                    , (Self,  apply 1 [Enrage]
                            • hide' "calories" 0 [Exhaust All] 
                            • hide' "calories" 0 [Exhaust All])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Spiky Human Boulder"
        , desc    = "Chōji rolls into a ball bristling with needle-like spikes and deals 15 damage to an enemy for 2 turns. While active, Chōji counters physical and chakra skills. Increases the cost of Chōji's skills by 1 random chakra each turn."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Rand, Rand]
        , channel = Action 2
        , start   = [ (Enemy, damage 15) 
                    , (Self,  apply 2 [CounterAll Physical, CounterAll Chakra] 
                            • hide' "calories" 0 [Exhaust All])
                    ]
        }
      , newSkill
        { label   = "Spiky Human Boulder"
        , desc    = "Chōji rolls into a ball bristling with needle-like spikes and deals 15 damage to an enemy for 2 turns. While active, Chōji counters physical and chakra skills. Increases the cost of Chōji's skills by 1 random chakra each turn."
        , classes = [Physical, Melee]
        , cost    = χ [Blood]
        , channel = Action 2
        , start   = [ (Enemy, damage 15) 
                    , (Self,  apply 2 [CounterAll Physical, CounterAll Chakra] 
                            • hide' "calories" 0 [Exhaust All])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Butterfly Mode"
        , desc    = "Performing an advanced Akimichi technique that would be lethal without precise control over his body, Chōji converts calories into jets of chakra energy that grow from his back like butterfly wings. Once used, this skill permanently becomes [Super-Slam][n][r][r]. Each turn after [Butterfly Mode] is activated, the costs of Chōji's skills decrease by 1 random chakra."
        , classes = [Chakra]
        , channel = Ongoing 0
        , start   = [(Self, hide' "calories" 0 [ Exhaust All]
                          • hide' "calories" 0 [Exhaust All]
                          • hide' "calories" 0 [Exhaust All]
                          • vary 0 0 1 • vary 0 1 1 • vary 0 2 1 • vary 0 3 1)]
        , effects = [(Self, removeStack "calories")]
        }
      , newSkill
        { label   = "Super-Slam"
        , desc    = "Chōji funnels chakra into his hands until they are as powerful as iron jackhammers and slams them into an enemy, dealing 30 damage and curing Chōji of enemy effects. Increases the cost of Chōji's skills by 2 random chakra."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin]
        , effects = [ (Enemy, damage 30) 
                    , (Self,  hide' "calories" 0 [Exhaust All]
                            • hide' "calories" 0 [Exhaust All])
                    ]
        }
      ]
    , [ newSkill 
        { label   = "Block"
        , desc    = "Chōji becomes invulnerable for 1 turn."
        , classes = [Physical]
        , cd      = 4
        , cost    = χ [Rand, Rand]
        , effects = [(Self, apply 1 [Immune All])]
        }
      , newSkill 
        { label   = "Block"
        , desc    = "Chōji becomes invulnerable for 1 turn."
        , classes = [Physical]
        , cd      = 4
        , effects = [(Self, apply 1 [Immune All])]
        }
      ]
    ] []
  , Character
    "Ino Yamanaka"
    "Now a chūnin, Ino takes control of every fight she faces. Her overpowering will steals the skills and secrets of her enemies and forces her allies to fight on no matter the cost. "
    [ [ newSkill
        { label   = "Mind Destruction"
        , desc    = "Ino infiltrates an enemy's mind and prepares to strike at a moment of weakness. Next turn, the target receives 15 damage. If they use a harmful skill, its effects will be nullified and this skill will be replaced by that skill for 1 turn. Ino's copy of their skill has no chakra cost and ends when this skill reverts."
        , classes = [Mental, Ranged, Invisible, Unreflectable, Unremovable]
        , cost    = χ [Gen]
        , cd      = 1
        , effects = [(Enemy, trap' (-1) (OnCounter Uncounterable) wait
                           • bomb (-1) [Copy 1 All 0 False] 
                             [(Done, damage 15)])]
        }
      ]
    , [ newSkill 
        { label   = "Proxy Surveillance"
        , desc    = "Ino's will takes over the battlefield. For 3 turns, she detects all invisible effects and enemy cooldowns. While active, the enemy team's damage reduction skills and destructible defense skills are reduced by 15. If an enemy uses a skill with negative damage reduction, damage to them is increased by its amount. If they use a skill with negative destructible defense, their target is damaged for its amount. If they use a skill with negative destructible barrier, they are damaged for its amount."
        , classes = [Mental, Invisible, Uncounterable, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 3
        , effects = [(Enemies, apply 3 [Reveal, Build (-15), Unreduce 15])]
        }
      ]
    , [ newSkill
        { label   = "Mind Transfer Clone"
        , desc    = "Ino takes control of her allies, forcing them to fight on no matter their condition. For 2 turns, her allies ignore harmful non-damage effects other than chakra cost changes."
        , classes = [Mental]
        , cost    = χ [Gen]
        , cd      = 2
        , effects = [(XAllies, apply 2 [Enrage])]
        }
      ]
    , invuln "Hide" "Ino" [Mental]
    ] []
  {- TODO: Rock Lee: per dead ally
  , Character
    "Rock Lee"
    "Lee's years of training with Gai have taught him not only new abilities, but what it truly means to fight. His strength grows as his allies fall, determined to honor them by finishing their battle."
    [ [ newSkill
        { label   = "Leaf Rising Wind"
        , desc    = "Lee plants his back on the ground and uses his entire body as a spring to kick an enemy with such power that they are launched into the air, dealing 15 damage and lowering the target's non-affliction damage by 15 for 2 turns. Deals 10 additional damage per dead ally. Effect lasts 1 additional turn per dead ally."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 0
        , effects = []
        }
      ]
    ] 
    -}
  , Character
    "Tenten"
    "Now a chūnin, Tenten's arsenal has expanded to a prodigious stockpile of some of the most powerful weapons in existence, including the legendary fan of the Sage of the Six Paths. Taking any excuse to show off the size and variety of her collection, she has assembled multiple item sets to switch out at a moment's notice."
    [ [ newSkill
        { label   = "Kunai Grenade"
        , desc    = "Tenten throws an explosive filled with a frankly ridiculous amount of kunai at an enemy, dealing 20 damage to them and 10 damage to the rest of their team."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai]
        , effects = [ (Enemy,    damage 20) 
                    , (XEnemies, damage 10)
                    ]
        }
      , newSkill
        { label   = "Tensasai"
        , desc    = "Using an advanced form of her Rising Twin Dragons technique, Tenten rains blindingly fast projectiles upon the battlefield, dealing 25 piercing damage to all enemies."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai, Rand]
        , cd      = 1
        , effects = [(Enemies, pierce 25)]
        }
      , newSkill
        { label   = "Leaf Fan: Coil of Fire"
        , desc    = "Using the Sage of the Six Path's legendary battle fan, Tenten blasts her enemies with a sea of raging fire, dealing 15 damage and 15 affliction damage to all enemies, as well as 5 affliction damage for the next 3 turns."
        , classes = [Bane, Physical, Ranged]
        , cost    = χ [Nin, Tai]
        , cd      = 3
        , effects = [(Enemies, damage 15 • afflict 15 • apply 3 [Afflict 5])]
        }
      ]
    , [ newSkill
        { label   = "Chain Spin"
        , desc    = "Tenten whirls a long chain whip around her team, making them invulnerable to physical skills for 1 turn."
        , classes = [Physical]
        , cost    = χ [Rand]  
        , effects = [(Allies, apply 1 [Immune Physical])]
        }
      , newSkill
        { label   = "Segmented Iron Dome"
        , desc    = "Tenten somehow produces a massive metal dome from a very small scroll, providing 25 permanent destructible defense to her and her allies."
        , classes = [Physical]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [(Allies, defend 0 25)]
        }
      , newSkill
        { label   = "Leaf Fan: Coil of Wind"
        , desc    = "Using the Sage of the Six Path's legendary battle fan, Tenten throws a gust of wind that reflects all non-mental skills used on her or her allies next turn."
        , classes = [Physical, Invisible, Unreflectable]
        , cost    = χ [Nin, Rand]
        , cd      = 3
        , effects = [(Allies, apply 1 [ReflectAll])]
        }
      ]
    , [ newSkill
        { label   = "Switch Loadout"
        , desc    = "Scrolling through her scrolls to the next item set, Tenten gains 5 permanent destructible defense and replaces her other skills. Tenten has 3 item sets."
        , classes = [Physical]
        , effects = [(Self, defend 0 5 • vary 0 0 1 • vary 0 1 1 • vary 0 2 1)]
        }
      , newSkill
        { label   = "Switch Loadout"
        , desc    = "Scrolling through her scrolls to the next item set, Tenten gains 5 permanent destructible defense and replaces her other skills. Tenten has 3 item sets."
        , classes = [Physical]
        , effects = [(Self, defend 0 5 • vary 0 0 2 • vary 0 1 2 • vary 0 2 2)]
        }
      , newSkill
        { label   = "Switch Loadout"
        , desc    = "Scrolling through her scrolls to the next item set, Tenten gains 5 permanent destructible defense and replaces her other skills. Tenten has 3 item sets."
        , classes = [Physical]
        , effects = [(Self, defend 0 5 • vary 0 0 0 • vary 0 1 0 • vary 0 2 0)]
        }
      ]
    , invuln "Dodge" "Tenten" [Physical]
    ] []
  , Character
    "Neji Hyūga"
    "Having surpassed his peers to reach the rank of jōnin, Neji has spent the intervening years honing his skills. He has learned to supplement his precise pressure-point attacks with devastating chakra waves that demolish the defenses of his opponents."
    [ [ newSkill
        { label   = "Eight Trigrams Air Palm" 
        , desc    = "Neji sends a blast of chakra-filled air at an enemy's vital points, dealing 20 damage and removing a random chakra."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, drain 1 • damage 20)]
        }
      ]
    , [ newSkill
        { label   = "Eight Trigrams Hazan Strike"
        , desc    = "Neji unleashes a giant wave of chakra at an enemy, demolishing their destructible defense and his destructible barrier, then dealing 45 damage."
        , classes = [Chakra, Ranged]
        , cost    = χ [Blood, Tai]
        , effects = [(Enemy, demolish • damage 45)]
        }
      ]
    , [ newSkill
        { label   = "Eight Trigrams Sixty-Four Palms"
        , desc    = "For 2 turns, enemies are prevented from reducing damage or becoming invulnerable. If an enemy uses a harmful skill on Neji during the first turn, it is countered and this skill is replaced for 1 turn by [Pressure Point Strike]."
        , classes = [Physical, Mental, Invisible]
        , cost    = χ [Blood]
        , effects = [ (Enemies, apply 2 [Expose]) 
                    , (Self,    apply 1 [Parry All 4])
                    ]
        }
      , newSkill
        { label   = "Pressure Point Strike"
        , desc    = "Deals 5 damage to an enemy, removes 1 chakra, and causes this skill to remain [Pressure Point Strike] for another turn."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , effects = [ (Enemy, drain 1 • damage 5) 
                    , (Self,  vary 1 2 1)
                    ]
        }
      ]
    , invuln "Byakugan Foresight" "Neji" [Mental]
    , [ newSkill
        { label   = "Eight Trigrams Sixty-Four Palms"
        , classes = [Physical, Mental]
        , effects = [(Self, vary 1 2 1)]
        } 
      ]
    ] []
  , Character
    "Kazekage Gaara"
    "Gaara's years of soul-searching have made him a powerful force for good, ready to assume the title of Kazekage. No longer concerned with destroying others, he devotes himself to protecting his friends and the Hidden Sand Village."
    [ [ newSkill
        { label   = "Partial Sand Coffin"
        , desc    = "Gaara keeps an enemy away from his allies by encasing one of the target's limbs in sand, stunning their non-mental skills for 1 turn and dealing 20 piercing damage when the effect ends. If they were countered by [Concealed Sand Picture] last turn, all of their skills are stunned and the damage is inflicted immediately. If the target has been affected by this skill before, the stun lasts an additional turn."
        , classes = [Physical, Ranged, Unremovable]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [(Enemy, ifU "Concealed Sand Picture"
                             § withU "Clinging Sand" 1 
                               (applyDur [Stun All]) 1 
                             ° pierce 20
                           • ifnotU "Concealed Sand Picture"
                             § withU "Clinging Sand" 1 
                               (\i → bomb i [Stun NonMental] 
                                     [(Expire, pierce 20)]
                               ) 1)]
        }
      ]
    , [ newSkill
        { label   = "Third Eye"
        , desc    = "Gaara surrounds himself and his allies with a hidden layer of sensing sand. Next turn, the first harmful skill used on his team will be countered and provide its target with 15 permanent destructible defense."
        , classes = [Physical, Invisible, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(Allies, apply 1 [Parry All 4])]
        }
      ]
    , [ newSkill
        { label   = "Sand Prison"
        , desc    = "Gaara traps and crushes an enemy inside constricting ropes of sand that tighten into an airtight prison, dealing 30 damage. For 2 turns, the target is immune to effects from allies and cannot reduce damage or become invulnerable. Deals 20 additional damage if the target was countered by [Concealed Sand Picture] last turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Nin]
        , cd      = 4
        , effects = [(Enemy, withU "Concealed Sand Picture" 20 damage 30
                           • apply 2 [Seal, Expose])] 
        }
      ]
    , invuln "Levitating Sand Shield" "Gaara" [Physical]
    , [ newSkill
        { label   = "Concealed Sand Picture"
        , classes = [Physical]
        , effects = [ (Enemy, tag 1)
                    , (Allies, defend 0 15 
                             • delay (-1) § remove "Concealed Sand Picture")
                    ]
        }
      ]
    ] []
    , Character
      "Kankurō"
      "Now a jōnin, Kankurō considers himself one of the greatest puppeteers in history after defeating Sasori. Adding Sasori's body to his collection of puppets, Kankurō uses each puppet for a different purpose."
      [ [ newSkill
          { label   = "Sasori Surrogate"
          , desc    = "Sasori's puppet body attacks an enemy, dealing 15 damage to them for 3 turns. While active, this skill becomes [Hidden Coil Strike][r]."
          , classes = [Physical, Ranged]
          , cost    = χ [Rand, Rand]
          , cd      = 2
          , channel = Action 3
          , start   = [(Self, vary' 0 1)]
          , effects = [(Enemy, damage 15)]
          }
        , newSkill
          { label   = "Hidden Coil Strike"
          , desc    = "Kankurō hooks an enemy with the coil hidden in Sasori's body and pulls the target to him, dealing 10 piercing damage. For 1 turn, the target can only target Kankurō or themselves."
          , classes = [Physical, Ranged, Bypassing, Unreflectable]
          , cost    = χ [Rand]
          , effects = [(Enemy, pierce 10 • apply 1 [ Taunt] 
                             • remove "Kuroari Trap")]
          }
        ]
      , [ newSkill
          { label   = "Kuroari Trap"
          , desc    = "Kankurō's Kuroari puppet stalks an enemy for 5 turns. If Kankurō uses [Hidden Coil Strike] on the target, the trap is activated immediately; otherwise, it is activated at the end of the 5 turns. Activating the trap applies [Kuroari Ambush] to the target, stunning them for 1 turn and making them invulnerable to everyone but Kankurō. Once used, this skill becomes [Iron Maiden][r][r][r]."
          , classes = [Physical, Ranged, Nonstacking, InvisibleTraps, Bypassing, Unreflectable, Unremovable]
          , cost    = χ [Rand]
          , cd      = 5
          , effects = [ (Self,  vary 0 1 1)
                      , (Enemy, bomb 5 [] 
                                [(Done, apply' "Kuroari Ambush" 1
                                        [Stun All, Seal, Duel])]) 
                      ]
          }
        , newSkill
          { label   = "Iron Maiden"
          , desc    = "Kankurō's Karasu puppet snaps shut around an enemy, dealing 20 piercing damage and 40 additional damage if the target is affected by [Kuroari Ambush]. Once used, this skill becomes [Kuroari Trap][r]."
          , classes = [Physical, Ranged, Uncounterable, Unreflectable]
          , cost    = χ [Rand, Rand]
          , effects = [ (Enemy, withU "Kuroari Ambush" 40 pierce 20) 
                      , (Self,  vary 0 1 0)
                      ]
          }
        ]
      , [ newSkill
          { label   = "Salamander Shield"
          , desc    = "Kankurō's Sanshōuo puppet shields him and his allies, providing Kankuro with 40 permanent destructible defense. While Kankurō has destructible defense from this skill, damage against his allies is reflected to him. Cannot be used while active."
          , classes = [Physical, Single, Soulbound, Unremovable, Unreflectable]
          , cost    = χ [Rand, Rand, Rand]
          , cd      = 5
          , effects = [ (Self,    defend 0 40) 
                      , (XAllies, apply 0 [Redirect All]
                                • onBreak § remove "Salamander Shield")
                      ]
          }
        ]
      , invuln "Puppet Distraction" "Kankurō" [Physical]
      ] []
  , Character
    "Temari"
    "The Hidden Sand Village's official ambassador, Temari is a formidable jōnin who uses an equally formidable battle fan. Her gusts of wind cut down the defenses of her enemies and free her allies from afflictions."
    [ [ newSkill
        { label   = "Cutting Whirlwind"
        , desc    = "Temari hurls a burst of wind at a target. If used on an enemy, she deals 15 damage to them and becomes invulnerable to them next turn. If used on an ally, she cures them of baneful effects. Targets all allies and enemies during [Sea Dragon]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [ (Enemy, damage 15 • apply 1 [Block])
                    , (XAlly, cureBane)
                    ]
        , changes = changeWith "Sea Dragon" targetAll
        }
      ]
    , [ newSkill
        { label   = "Sea Dragon"
        , desc    = "Temari calls down a giant tornado for 4 turns, dealing 5 piercing damage to all enemies each turn. While active, non-affliction damage from Temari's team always pierces."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin, Nin]
        , cd      = 5
        , channel = Ongoing 4
        , effects = [ (Enemies, pierce 5) 
                    , (Allies,  apply 1 [Pierce])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Gust Blade"
        , desc    = "Temari sends slicing blades of wind at an enemy, demolishing their destructible defense and her destructible barrier, then dealing 35 damage to them. Targets all enemies during [Sea Dragon]."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Nin]
        , cd      = 2
        , effects = [(Enemy, demolish • damage 35)]
        , changes = changeWith "Sea Dragon" targetAll
        }
      ]
    , invuln "Block" "Temari" [Physical]
    ] []{-
  , Character
    "Kabuto Yakushi"
    "A dangerous rogue ninja and Orochimaru's pupil, Kabuto grows more powerful by the day through untiring study and research. His knowledge and brilliance are all but limitless, and his ambition has been growing to match them. With his perfected form of reanimation, he sacrifices his enemies to resurrect his dead teammates."
    [ [ newSkill 
        { label   = "Summoning: Clone Serpent"
        , desc    = "Kabuto draws blood from himself or an ally, sacrificing 15 of the target's health to summon a giant serpent made up of smaller snakes for 5 turns. When Kabuto uses a skill on an enemy, the serpent bites his target with paralyzing venom, dealing 5 affliction damage to them for 3 turns and ending their Action and Control skills in progress. While poisoned, enemies cannot reduce damage, become invulnerable, or be healed or cured. Kabuto stores a blood sample of the ally most recently affected by this skill."
        , classes = [Chakra, Summon]
        , cost    = χ [Nin]
        , channel = Ongoing 5
        , start   = [ (XAlly, everyone § remove "Blood Sample" 
                            • tag' "Blood Sample" 0) 
                    , (Ally,  sacrifice 0 15)
                    ]
        }
      ] 
    , [ newSkill
        { label   = "Blood Offering"
        , desc    = "Kabuto draws blood from himself or an ally to summon a venomous snake, sacrificing  A reanimated corpse stalks one of Kabuto's enemies, dealing 35 piercing damage to them. The target cannot be healed or cured for 2 turns."
        , classes = [Bane, Physical, Ranged]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [(Enemy, pierce 35 • apply 2 [Plague])]
        }
      ]
    , [ newSkill
        { label   = "Reanimation Sacrifice"
        , desc    = "Kabuto slices an enemy with a chakra scalpel, dealing 20 affliction damage to them. If he has a blood sample from [Summoning: Clone Serpent], he injects it into the target and prepares to use their body as a sacrifice. In 2 turns, if the target carries the blood sample of a dead ally, the ally is resurrected with as much health as the target and the target is killed."
        , classes = [Bane, Chakra]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [(Enemy, afflict 20 
                           • ifI "Summoning: Clone Serpent" § interrrupt
                           • perI "Summoning: Clone Serpent" 5
                             (\i → apply' "Summoning: Clone Serpent" 3 
                                   [Afflict i, Plague, Expose]) 
                             0
                           • bomb (-2) [] [(Expire, )])]
        }
      ]
    , invuln "Dodge" "Kabuto" [Physical]
    ] []-}
  , Character
    "Konohamaru Sarutobi"
    "The grandson of the Third Hokage, Konohamaru has spent his youth working hard to pursue his dream of one day following in his grandfather's steps. No longer a bumbling student, Konohamaru has become remarkably skillful as a genin. Agile and fast, he can rush in to save an ally on the brink of defeat."
    [ [ newSkill
        { label   = "Rasengan"
        , desc    = "Konohamaru focuses on an enemy and takes his time to prepare his Rasengan. Next turn, it will deal 25 damage to the target and an additional 15 if they used a skill."
        , classes = [Chakra, Melee, Invisible]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, trap (-1) (OnAction All) flag
                           • delay (-1) § withU "Rasengan" 15 pierce 25)]
        }
      ] 
    , [ newSkill
        { label   = "Agile Backflip"
        , desc    = "Konohamaru uses his agility to counter the next new non-mental skill used on him. Each time this skill is used, its cost increases by 1 random. Cannot be used while active."
        , classes = [Physical, Invisible, Single]
        , cost    = χ [Rand]
        , effects = [(Self, apply 0 [Counter NonMental] • hide' "tired" 0 [])]
        , changes = costPer "tired" [Rand]
        }
      ]
    , [ newSkill
        { label   = "Quick Recovery"
        , desc    = "Konohamaru rushes to an ally's rescue in the nick of time. The first time that the target's health reaches 0 next turn, they regain 15 health."
        , classes = [Physical, Invisible]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(XAlly, trap 1 OnRes § setHealth 15)]
        }
      ]
    , invuln "Hide" "Konohamaru" [Mental]
    ] []
  ] 
