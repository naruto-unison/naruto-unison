{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Flashbacks (flashbackCs) where

import Preludesque
import Game.Functions
import Game.Game
import Game.Structure

flashbackCs ∷ [Character]
flashbackCs =
  [ Character
    "Konohamaru Sarutobi"
    "The overly bold grandson of the third Hokage, Konohamaru has a knack for getting into trouble and requiring others to bail him out. His usefulness in battle depends entirely on how willing his teammates are to babysit him."
    [ [ newSkill
        { label   = "Refocus"
        , desc    = "Konohamaru tries his best to concentrate on the fight. For 3 turns, effects from his allies on him are twice as powerful. While active, this skill becomes [Unsexy Technique][n]."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [(Self, vary 3 0 1 • tag 3
                          • alliedTeam ∘ self § hide 3 [Boost 2])]
        }
      , newSkill 
        { label   = "Unsexy Technique"
        , desc    = "Konohamaru distracts an enemy with his modified version of the transformation technique he learned from Naruto. For 1 turn, the target is immune to effects from their allies and cannot reduce damage, become invulnerable, counter, or reflect."
        , classes = [Chakra]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [(Enemy, apply 1 [Seal, Expose, Uncounter ])]
        }
      ]
    , [ newSkill
        { label   = "Throw a Fit"
        , desc    = "Konohamaru freaks out and punches wildly at an enemy, dealing 10 damage to them for 3 turns. Deals 5 additional damage per skill affecting Konohamaru from his allies."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , cd      = 3
        , channel = Action 3
        , effects = [(Enemy, perHelpful 5 damage 10)]
        }
      ]
    , [ newSkill
        { label   = "Throw a Shuriken"
        , desc    = "Konohamaru flings a shuriken almost too big for his hand at an enemy, dealing 10 damage and 10 additional damage per skill affecting Konohamaru from his allies."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai]
        , effects = [(Enemy, perHelpful 10 damage 10)]
        }
      ]
    , invuln "Hide?" "Konohamaru" [Mental]
    ] []
  , Character
    "Chōza Akimichi"
    "A jōnin from the Hidden Leaf Village and Chōji's father, Chōza instills confidence in his comrades with his bravery and wisdom. Never one to back down from a fight, he defends his allies and forces the attention of his enemies to himself."
    [ [ newSkill
        { label   = "Chain Bind"
        , desc    = "Chōza slows an enemy, dealing 5 damage and weakening their physical and chakra damage by 10 for 1 turn. Chōza's team gains 5 permanent destructible defense."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , effects = [ (Enemy,  damage 5
                             • apply 1 [Weaken Physical 10, Weaken Chakra 10])
                    , (Allies, defend 0 5)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Human Boulder"
        , desc    = "Chōza transforms into a rolling juggernaut. For 3 turns, he deals 15 damage to an enemy and grants 10 destructible defense to himself and his allies for 1 turn. Each turn, if the target is affected by [Chain Bind], it lasts an additional turn on them."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Rand]
        , cd      = 3
        , channel = Action 3
        , effects = [ (Allies, defend 1 10)
                    , (Enemy,  damage 15
                             • ifU "Chain Bind" 
                               § apply' "Chain Bind" 1 [ Weaken Physical 10
                                                       , Weaken Chakra 10
                                                       ])
                    ]
        }
      ] 
    , [ newSkill
        { label   = "Partial Expansion"
        , desc    = "If used on an enemy, the next harmful non-mental skill they use will be countered. If used on an ally, the next harmful non-mental skill used on them wil be countered. The person countered will receive 10 damage, bypassing invulnerability."
        , classes = [Physical, Melee, Single, Invisible, Unreflectable]
        , cost    = χ [Blood]
        , cd      = 2
        , effects = [ (XAlly, apply 0 [Parry NonMental 4]) 
                    , (Enemy, trap 0 (OnCounter NonMental) § damage 10 )
                    ]
        }
      ]
    , invuln "Block" "Chōza" [Physical]
    , [ newSkill
        { label   = "Partial Expansion"
        , classes = [Physical, Melee, Single]
        , effects = [(Enemy, damage 10)]
        }
      ]
    ] []
  , Character
    "Kushina Uzumaki"
    "Known as the Red-Hot Habanero for her fierce temper and fiery hair, Naruto's mother possesses exceptional chakra necessary to become the nine-tailed fox's jinchūriki. Kushina specializes in unique sealing techniques that bind and incapacitate her enemies."
    [ [ newSkill
        { label   = "Double Tetragram Seal"
        , desc    = "Kushina seals away an enemy's power, dealing 15 piercing damage, stunning them for 1 turn, removing 1 chakra, and permanently weakening their damage by 5."
        , classes = [Chakra, Ranged, Multi]
        , cost    = χ [Gen, Rand]
        , cd      = 1
        , effects = [(Enemy, drain 1 • damage 15
                            • apply 1 [Stun All] • apply 0 [Weaken All 5])]
        }
      ]
    , [ newSkill
        { label   = "Life Link"
        , desc    = "Kushina binds her life-force to that of an enemy. For 4 turns, if either dies, the other will die as well. Effect cannot be avoided, prevented, or removed."
        , classes = [Mental, Ranged, Unremovable, Uncounterable, Unreflectable]
        , cost    = χ [Gen, Rand]
        , cd      = 5
        , effects = [ (Enemy, tag 4 • trap 5 OnDeath § self kill)
                    , (Self,  trap' 5 OnDeath ∘ everyone 
                              § ifU "Life Link" kill')
                    ]
        }
      ]
    , [ newSkill
        { label   = "Adamantine Sealing Chains"
        , desc    = "Kushina seals an enemy within a cage of chain-shaped chakra, removing the effects of helpful skills from them and stunning them for 2 turns. While active, the target is immune to effects from allies and invulnerable."
        , classes = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , cost    = χ [Blood, Gen]
        , cd      = 4
        , effects = [(Enemy, purge • apply 2 [Stun All, Immune All, Seal])]
        }
      ]
    , invuln "Adamantine Covering Chains" "Kushina" [Chakra]
    ] []
  , Character
    "Minato Namikaze"
    "Known as the Yellow Flash for his incredible speed and mastery of space-time techniques, Minato is a jōnin squad leader from the Hidden Leaf Village who will one day become Naruto's father. He fights using unique kunai that allow him to teleport arround the battlefield."
    [ [ newSkill
        { label   = "Flying Raijen"
        , desc    = "Minato teleports to a target, becoming invulnerable for 1 turn. If he teleports to an enemy, he deals 30 damage. If he teleports to an ally, the ally becomes invulnerable for 1 turn."
        , classes = [Physical, Melee, Bypassing]
        , cost    = χ [Gen, Rand]
        , effects = [ (Self,    apply 1 [Immune All])
                    , (XAlly,   apply 1 [Immune All]
                              • ifI "Space-Time Marking" 
                                § tag' "Space-Time Marking" 1)
                    , (Enemy,   damage 30
                              • ifI "Space-Time Marking"
                                § tag' "Space-Time Marking" 1)
                    , (XAllies, ifU "Space-Time Marking" § apply 1 [Immune All])
                    , (Enemies, ifU "Space-Time Marking" § damage 30)

                    ]
        }
      ]
    , [ newSkill
        { label   = "Wide Area Sensing"
        , desc    = "Minato expands his senses to cover the battlefield, preventing enemies from reducing damage or becoming invulnerable for 2 turns. Each turn, Minato gains a random chakra."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin]
        , cd      = 3
        , channel = Control 2
        , effects = [ (Enemies, apply 1 [Expose])
                    , (Self,    gain [Rand])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Space-Time Marking"
        , desc    = "For 3 turns, [Flying Raijen] marks its target for 1 turn. Using [Flying Raijen] causes marked allies to become invulnerable for 1 turn and deals 30 damage to marked enemies, bypassing invulnerability."
        , classes = [Physical, Melee]
        , cost    = χ [Gen, Nin]
        , cd      = 6
        , effects = [(Self, tag 3)]
        }
      ]
    , invuln "Flying Light" "Minato" [Physical]
    ] []
  , Character
    "Yondaime Minato"
    "Now the fourth Hokage, Minato has been shaped by his responsibilities into a thoughtful and strategic leader. With his space-time jutsu, he redirects the attacks of his enemies and effortlessly passes through their defenses."
    [ [ newSkill
        { label   = "Space-Time Marking"
        , desc    = "Minato opportunistically marks targets to use as teleport destinations for avoiding attacks. Next turn, allies and enemies who do not use a skill will be marked by this skill for 4 turns. Minato gains 5 points of damage reduction for each marked target. This skill stacks."
        , classes = [Physical, Ranged, Invisible, Bypassing]
        , cost    = χ [Blood]
        , cd      = 1
        , effects = [ (XAllies, delay 0 ∘ trap' 1 (OnAction All) 
                                § remove "Marking Targets"
                              • bomb' "Marking Targets" 1 [] [(Expire, tag 3)]
                      )
                    , (Enemies, trap' (-1) (OnAction All) 
                                § remove "Marking Targets"
                              • bomb' "Marking Targets" (-1) []
                                [(Expire, tag (-4))])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Teleportation Barrier"
        , desc    = "Minato warps the space around himself or an ally. The first harmful skill used on the target next turn will be reflected."
        , classes = [Chakra, Ranged, Unreflectable]
        , cost    = χ [Gen]
        , cd      = 3
        , effects = [(Ally, apply 1 [Reflect])]
        }
      ]
    , [ newSkill
        { label   = "Rasengan"
        , desc    = "Minato teleports behind an enemy and slams an orb of chakra into them, dealing 20 damage. In quick succession, he teleports between all enemies affected by [Space-Time Marking], dealing 20 damage for every stack of [Space-Time Marking] on them."
        , classes = [Chakra, Melee, Bypassing]
        , cost    = χ [Blood, Rand]
        , effects = [ (Enemy, damage 20)
                    , (Enemies, perU "Space-Time Marking" 20 damage 0)
                    ]
        }
    ]
    , invuln' "Round-Robin Raijen" 
              "Minato and allies affected by [Space-Time Marking] becomes invulnerable for 1 turn." 
            [Chakra] 
            [alliedTeam § apply 1 [Immune All]]
    ] []
  , Character
    "Young Kakashi"
    "A member of Team Minato, Kakashi is the thirteen-year-old son of the legendary White Fang. His early ninjutsu and borrowed sharingan make him the equal of any adult he faces."
    [ [ newSkill
        { label   = "White Light Blade"
        , desc    = "Kakashi deals 20 piercing damage to an enemy with his sword. For 1 turn, the target's non-affliction damage is weakened by 5 and Kakashi's damage is increased by 5."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy, damage 20 • apply 1 [Weaken All 5]
                            • ifI "Sharingan Stun" § apply 1 [Stun All])
                    , (Self,  apply 1 [Strengthen All 5])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Amateur Lightning Blade"
        , desc    = "Using an early form of his signature technique, Kakashi deals 20 piercing damage to one enemy. For 1 turn, the target's non-affliction damage is weakened by 5 and Kakashi's damage is increased by 5."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin]
        , effects = [ (Enemy, damage 20 • apply 1 [Weaken All 5]
                            • ifI "Sharingan Stun" § apply 1 [Stun All])
                    , (Self,  apply 1 [Strengthen All 5])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Sharingan"
        , desc    = "Kakashi anticipates an opponent's moves for 2 turns. If they use a skill that removes or steals chakra, Kakashi gains a random chakra. If they use a skill that stuns, Kakashi's skills will stun next turn. If they use a skill that damages, Kakashi's damage will be increased by 10 during the next turn."
        , classes = [Mental, Ranged, InvisibleTraps]
        , cd      = 1
        , effects = [(Enemy, trap 1 OnChakra ∘ self § gain [Rand]
                           • trap 1 OnStun   ∘ self § gain [Rand]
                           • trap 1 OnDamage ∘ self 
                             § apply 1 [Strengthen All 10])]
        }
      ]
    , invuln "Parry" "Kakashi" [Physical]
    ] []
  , Character
    "Obito Uchiha"
    "A member of Team Minato, Obito is treated as a nobody despite his Uchiha heritage. He dreams of becoming Hokage so that people will finally acknowledge him. Accustomed to helping from the sidelines, if he falls in battle, he will lend his strength to his allies."
    [ [ newSkill
        { label   = "Piercing Stab"
        , desc    = "Spotting an opening in his enemy's defense, Obito stabs them to deal 15 piercing damage. Deals 10 additional damage during [Sharingan]."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , effects = [(Enemy, withI "Sharingan" 10 pierce 15)]
        }
      ]
    , [ newSkill
        { label   = "Grand Fireball"
        , desc    = "Obito breathes searing fire on an enemy, dealing 15 affliction damage for 2 turns. During [Sharingan], this skill deals the full 30 affliction damage instantly and has no cooldown."
        , classes = [Bane, Ranged]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [(Enemy, apply 2 [Afflict 15])]
        }
      , newSkill
        { label   = "Grand Fireball"
        , desc    = "Obito breathes searing fire on an enemy, dealing 15 affliction damage for 2 turns. During [Sharingan], this skill deals the full 30 affliction damage instantly and has no cooldown."
        , classes = [Bane, Ranged]
        , cost    = χ [Nin]
        , varicd  = True
        , effects = [(Enemy, afflict 30)]
        }
      ]
    , [ newSkill
        { label   = "Sharingan"
        , desc    = "Obito targets an ally. For 4 turns, Obito gains 15 points of damage reduction, and if Obito dies, the ally will gain 5 points of damage reduction and deal 5 additional non-affliction damage."
        , classes = [Mental, Unremovable]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [ (XAlly, tag 4)
                    , (Self,  apply 4 [Reduce All 15]
                            • trap' 4 OnDeath ∘ everyone ∘ ifU "Sharingan" 
                              § apply' "Borrowed Sharingan" 0
                                [Reduce All 5, Strengthen NonAffliction 5])
                    ]
        }
      ]
    , invuln "Flee" "Obito" [Physical]
    ] []
  , Character
      "Rin Nohara"
      "A chūnin on Team Minato, Rin is a quiet but strong-willed medical-nin. Her priority is always healing her teammates, though she can also defend herself with traps if necessary."
      [ [ newSkill
        { label   = "Pit Trap"
        , desc    = "Rin traps an enemy for 1 turn, gaining 15 points of damage reduction. At the end of their turn, the target takes 15 piercing damage. If they used a skill that turn, they take 15 additional damage."
        , classes = [Invisible, Bypassing]
        , cost    = χ [Gen]
        , effects = [(Self,  apply 1 [Reduce All 15])
                    , (Enemy, trap (-1) (OnAction All) flag
                            • delay (-1) § withU "Pit Trap" 15 pierce 15)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Mystical Palm Healing"
        , desc    = "Rin restores 25 health to herself or an ally and cures the target of enemy effects."
        , classes = [Chakra]
        , cost    = χ [Nin]
        , effects = [(Ally, cureAll • heal 25)]
        }
      ]
    , [ newSkill
        { label   = "Medical Kit"
        , desc    = "Rin or one of her allies uses her medical kit for 3 turns, restoring 10 health each turn and strengthening their healing skills by 10 points."
        , classes = [Physical, Unremovable]
        , cost    = χ [Rand, Rand]
        , cd      = 3
        , effects = [(Ally, apply 3 [Bless 10, Heal 10])]
        }
      ]
    , invuln "Flee" "Rin" [Physical]
    ] []
  , Character
    "Corrupted Obito"
    "After being rescued from the brink of death by Madara, Obito has hurried back to the Hidden Leaf Village only to witness Kakashi stab Rin through the heart. With his sanity shattered by trauma and his Mangekyō Sharingan awakened, he wields the wood-shaping abilities of his Zetsu armor to rampage through the senseless hell his life has become."
    [ [ newSkill
        { label   = "Cutting Sprigs"
        , desc    = "Obito impales an enemy with a wooden skewer, dealing 20 piercing damage and permanently increasing the damage of this skill on the target by 5. Deals twice as much damage if the target is affected by [Murderous Resolve]."
        , classes = [Physical, Melee, Multi]
        , cost    = χ [Blood]
        , effects = [(Enemy, ifnotU "Murderous Resolve" 
                             § perU "Cutting Sprigs" 5 pierce 20
                           • ifU "Murderous Resolve"
                             § perU "Cutting Sprigs" 10 pierce 40
                           • addStack)]
        }
      ]
    , [ newSkill
        { label   = "Mangekyō Sharingan"
        , desc    = "Obito activates his trauma-awakened Mangekyō eye to counter the next non-mental skill used on him."
        , classes = [Chakra, Invisible, Single]
        , cost    = χ [Gen]
        , cd      = 2
        , effects = [(Self, apply 0 [Parry NonMental 4])]
        }
      ]
    , [ newSkill
        { label   = "Murderous Resolve"
        , desc    = "Obito's mind snaps and fixates obsessively on an enemy who was countered by [Mangekyō Sharingan] last turn. For 4 turns, the target's damage is weakened by 5 and they are prevented from reducing damage or becoming invulnerable."
        , require = HasU "Mangekyō Sharingan"
        , classes = [Mental, Ranged]
        , cost    = χ [Rand, Rand]
        , cd      = 5
        , effects = [(Enemy, apply 4 [Expose, Weaken All 5])]
        }
      ]
    , invuln "Hide" "Obito" [Mental]
    , [ newSkill 
        { label   = "Mangekyō Sharingan"
        , classes = [Chakra]
        , effects = [(Enemy, tag 1)]
        }
      ]
    ] []
  ]
