{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Kids (kidCs) where

import StandardLibrary

import qualified Game.Ninja as N

import Game.Functions
import Game.Game
import Game.Structure 

kidCs :: [Group -> Character]
kidCs =
  [ Character
    "Naruto Uzumaki"
    "A genin from Team 7, Naruto is an orphan with the goal of becoming Hokage. His signature Shadow Clones copy his moves to perform powerful combos and wield the Rasengan."
    [ [ newSkill
        { label   = "Naruto Uzumaki Barrage"
        , desc    = "Using his version of the Lions Barrage, Naruto deals 10 damage to an enemy. Each Shadow Clone deals 5 damage to the target as well. Spends one Shadow Clone if Naruto has any."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy,  perI "Shadow Clone" 5 damage 10) 
                    , (REnemy, ifI "Shadow Clone" § damage 5)
                    , (Self,   removeStack "Shadow Clone")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Rasengan"
        , desc    = "Naruto hits an enemy with an orb of chakra, dealing 30 damage. Spends two Shadow Clones. The target is stunned 1 turn for every 2 Shadow Clones remaining."
        , require = HasI 2 "Shadow Clone"
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [ (Self,  removeStacks "Shadow Clone" 2)
                    , (Enemy, damage 30 
                            • perI' "Shadow Clone" 1 2 (applyDur [Stun All]) 0) 
                    , (REnemy, damage 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Shadow Clones"
        , desc    = "Naruto creates 6 Shadow Clones who hide him and fight in his place. Each shadow clone provides 5 points of damage reduction. Each time an enemy uses a skill on Naruto, a shadow clone deals 5 damage to them and disappears. Each time Naruto loses a shadow clone by using a skill, the shadow clone deals 5 damage to a random enemy. Cannot be used while Naruto has shadow clones remaining."
        , require = HasI (-1) "Shadow Clone"
        , classes = [Chakra, Unremovable, BaseTrap]
        , cost    = χ [Rand]
        , cd      = 3
        , effects = [(Self, trapFrom' 0 (OnHarmed All)
                            § ifI "Shadow Clone" § damage 5
                            ° self (removeStack "Shadow Clone")
                            ° ifnotI "Shadow Clone" (removeTrap "Shadow Clones")
                          • apply' "Shadow Clone" 0 [Reduce All Flat 5]
                          • apply' "Shadow Clone" 0 [Reduce All Flat 5]
                          • apply' "Shadow Clone" 0 [Reduce All Flat 5]
                          • apply' "Shadow Clone" 0 [Reduce All Flat 5]
                          • apply' "Shadow Clone" 0 [Reduce All Flat 5]
                          • apply' "Shadow Clone" 0 [Reduce All Flat 5])]
        }
      ]
    , invuln' "Sexy Technique" 
              "Naruto and his shadow clones become invulnerable for 1 turn." 
              [Chakra] []
    ] []
  , Character
    "Sakura Haruno"
    "A genin from Team 7, Sakura is intelligent but self-conscious. Her recent training from Tsunade has prepared her to heal her allies and knock back her enemies."
    [ [ newSkill
        { label   = "KO Punch"
        , desc    = "Sakura punches an enemy with all her strength, dealing 20 damage and stunning their physical and mental skills for 1 turn. Deals 10 additional damage during [Inner Sakura]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [(Enemy, withI "Inner Sakura" 10 damage 20
                           • apply 1 [Stun Mental, Stun Physical])]
        }
      ]
    , [ newSkill
        { label   = "Mystical Palm Healing"
        , desc    = "Using basic healing techniques, Sakura restores 25 health to herself or an ally."
        , classes = [Chakra]
        , cost    =  χ [Nin]
        , cd      = 1
        , effects = [(Ally, heal 25)]
        }
      ]
    , [ newSkill
        { label   = "Inner Sakura"
        , desc    = "Sakura's inner self surfaces and urges her on. For 4 turns, Sakura gains 10 points of damage reduction and ignores all non-damage effects other than chakra cost changes."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [(Self, apply 4 [Enrage, Reduce All Flat 10])]
        }
      ]
    , invuln "Substitution Technique" "Sakura" [Chakra]
    ] []
  , Character
    "Sasuke Uchiha"
    "A genin from Team 7, Sasuke seeks vengeance against his brother for slaughtering the rest of the Uchiha clan. Using his sharingan, Sasuke targets his opponent's weak spots and anticipates their attacks."
    [ [ newSkill
        { label   = "Lions Barrage"
        , desc    = "Copying a taijutsu combo that Lee used on him, Sasuke deals 30 damage to an enemy. Deals 15 additional damage to an enemy affected by [Sharingan]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, withU "Sharingan" 15 damage 30)]
        }
      ]
    , [ newSkill
        { label   = "Chidori"
        , desc    = "Sasuke attacks an enemy with a bolt of lightning, dealing 30 piercing damage. Deals 25 additional damage to an enemy affected by [Sharingan]."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [(Enemy, withU "Sharingan" 25 pierce 30)]
        }
      ]
    , [ newSkill
        { label   = "Sharingan"
        , desc    = "Sasuke focuses his gaze on an enemy. For 4 turns, he gains 15 points of damage reduction and the enemy cannot reduce damage or become invulnerable. Ends if Sasuke dies."
        , classes = [Mental, Ranged, Soulbound, Unremovable]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [ (Self,  apply 4 [Reduce All Flat 10])
                    , (Enemy, apply 4 [Expose])
                    ]
        }
      ]
    , invuln "Block" "Sasuke" [Physical]
    ] []
  , Character
    "Kiba Inuzuka"
    "A genin from Team 8, Kiba is short-tempered and impulsive. His powerful taijutsu skills are amplified when he fuses with his dog, Akamaru, and transforms into a double-headed monster."
    [ [ newSkill
        { label   = "Wolf Fang"
        , desc    = "Kiba projects a vacuum vortex at an enemy, dealing 30 damage. Deals 5 additional damage to an enemy affected by [Dynamic Marking]. Costs 1 taijutsu chakra during [Two-Headed Wolf]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, withU "Dynamic Marking" 5 damage 30)]
        , changes = changeWith "Two-Headed Wolf" $ setCost [Tai]
        }
      ]
    , [ newSkill
        { label   = "Two-Headed Wolf"
        , desc    = "Kiba and Akamaru transform into giant beasts, gaining 15 points of damage reduction and dealing 15 damage to all enemies for 3 turns."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Tai]
        , cd      = 3 
        , channel = Action 3
        , effects = [ (Enemies, damage 15)
                    , (Self,    apply 1 [Reduce All Flat 15])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Dynamic Marking"
        , desc    = "Akamaru sprays urine on an enemy, preventing them from reducing damage or becoming invulnerable for 3 turns. Cannot be used on an enemy already affected by this skill."
        , classes = [Ranged, Single]
        , effects = [(Enemy, apply 3 [Expose])]
        }
      ]
    , invuln "Smoke Bomb" "Kiba" [Physical]
    ] []
  , Character
    "Shino Aburame"
    "A genin from Team 8, Shino is reserved and tactical. He directs the insects living within his body to leech chakra from his enemies and protect his teammates."
    [ [ newSkill
        { label   = "Chakra Leech"
        , desc    = "Chakra-draining bugs attack an enemy, dealing 20 affliction damage and stealing 1 chakra. Deals 5 additional damage per target's stack of [Parasite]."
        , classes = [Bane, Ranged]
        , cost    = χ [Blood, Rand]
        , cd      = 1
        , effects = [(Enemy, steal 1 • perU "Parasite" 5 afflict 20)]
        }
      ]
    , [ newSkill
        { label   = "Parasite"
        , desc    = "Shino directs one of his bugs to attach itself to an enemy. For 4 turns, the target's non-affliction damage is weakened by 5."
        , classes = [Bane, Physical, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, apply 4 [Weaken All Flat 5])]
        }
      ]
    , [ newSkill
        { label   = "Wall of Insects"
        , desc    = "A massive swarm of insects surrounds Shino's team, providing 20 permanent destructible defense to them."
        , classes = [Physical]
        , cost    = χ [Blood, Rand]
        , cd      = 3
        , effects = [(Allies, defend 0 20)]
        }
      ]
    , invuln "Insect Clone" "Shino" [Chakra]
    ] []
  , Character
    "Hinata Hyūga"
    "A genin from Team 8, Hinata is the shy and withdrawn heiress of the Hyūga clan. With her clan's signature Byakugan, she senses her opponent's energy flow and blocks it at key points to remove their chakra."
    [ [ newSkill
        { label   = "Gentle Fist"
        , desc    = "Using the Hyūga clan's signature taijutsu style, Hinata deals 20 damage to an enemy for 2 turns. During [Byakugan], removes 1 chakra from the opposing team each turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , cd      = 1
        , channel = Action 2
        , effects = [(Enemy, ifI "Byakugan" § drain 1 • damage 20)]
        }
      ]
    , [ newSkill
        { label   = "Eight Trigrams Sixty-Four Palms"
        , desc    = "Hinata deals 15 damage to all enemies and provides 10 destructible defense to her team for 1 turn. Deals 5 additional damage to all enemies during [Byakugan]."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemies, withI "Byakugan" 5 damage 15)
                    , (Allies,  defend 1 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Byakugan"
        , desc    = "Hinata activates her Byakugan, gaining 15 points of damage reduction for 4 turns."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [(Self, apply 4 [Reduce All Flat 15])]
        }
      ]
    , invuln "Block" "Hinata" [Physical]
    ] []
  , Character
    "Shikamaru Nara"
    "A genin from Team 10, Shikamaru is considered the smartest of the Konoha 11. He manipulates shadows to disable and attack his enemies. Whoever he fights, it's only a matter of time before he comes up with the perfect plan to defeat them."
    [ [ newSkill
        { label   = "Meditate"
        , desc    = "Shikamaru sits down and contemplates an enemy. Over the next 5 turns, he composes a strategy against them. Cannot be used on an enemy already affected by this skill."
        , classes = [Single, Mental, Ranged, Uncounterable, Unreflectable]
        , effects = [(Enemy, tag 5)]
        }
      ]
    , [ newSkill
        { label   = "Shadow Strangle"
        , desc    = "Shadow tendrils choke Shikamaru's enemies, dealing 15 damage and preventing them from reducing damage or becoming invulnerable for 1 turn. Enemies affected by [Meditate] are exposed for 2 turns."
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen]
        , cd      = 1
        , effects = [(Enemies, damage 15 
                             • withU "Meditate" 1 (applyDur [Expose]) 1)]
        }
      ]
    , [ newSkill
        { label   = "Shadow Possession"
        , desc    = "Shikamaru captures all enemies in shadows, stunning non-mental skills for 1 turn. Enemies affected by [Meditate] are stunned for 2 turns."
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen, Rand]
        , cd      = 3
        , effects = [(Enemies, withU "Meditate" 1 
                               (applyDur [Stun NonMental]) 1)]
        }
      ]
    , invuln "Hide" "Shikamaru" [Mental]
    ] []
  , let loadout = varyLoadout 0 0 0 False
    in Character
    "Chōji Akimichi"
    "A genin from Team 10, Chōji is a voracious eater and loyal friend. His clan's special pills immensely magnify his innate strength and unlock different abilities, but the toll they take on his body can kill him if he pushes himself too far."
    [ [ newSkill 
        { label   = "Spinach Pill"
        , desc    = "Chōji eats the mildest Akimichi pill, losing 5 health down to a minimum of 1 and gaining the strength he needs to protect his friends. While alive, he provides 5 points of damage reduction to his allies."
        , classes = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , effects = [ (XAllies, apply' "Protected" 0 [Reduce All Flat 5])
                    , (Self,    sacrifice 1 5 • loadout 1)
                    ]
        }
      , newSkill
        { label   = "Obstructing Tackle"
        , desc    = "Chōji charges an enemy, dealing 20 damage to them and weakening their damage by 20 for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [(Enemy, damage 20 • apply 1 [Weaken All Flat 20])]
        }
      , newSkill
        { label    = "Partial Expansion"
        , desc     = "Chōji greatly enlarges one of his arms and swings it through the enemy team, dealing 20 damage."
         , classes = [Physical, Melee, Bypassing]
         , cost    = χ [Tai, Rand]
         , effects = [(Enemies, damage 20)]
        }
      , newSkill
        { label   = "Justice Punch"
        , desc    = "Chōji slams his fist into an enemy and knocks them to the ground, dealing 25 damage and stunning them for 1 turn. While stunned, they cannot reduce damage or become invulnerable."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [(Enemy, damage 25 • apply 1 [Stun All, Expose])]
        }
      ]
    , [ newSkill
        { label   = "Curry Pill"
        , desc    = "Chōji eats the first two Akimichi pills in one go, losing 15 health down to a minimum of 1 and unlocking huge reserves of chakra in addition to immense physical strength. While alive, he provides 10 points of damage reduction to his allies."
        , classes = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , cost    = χ [Rand]
        , effects = [ (XAllies, apply' "Protected" 0 [Reduce All Flat 10])
                    , (Self,    sacrifice 1 15 • loadout 2)
                    ]
        }
      , newSkill
        { label   = "Human Boulder"
        , desc    = "Chōji enlarges his body, tucks in his limbs, and uses chakra to propel himself into an unstoppable roll. For 2 turns, he deals 10 damage and 5 piercing damage to an enemy. While active, Chōji gains 15 points of damage reduction and ignores stuns."
        , classes = [Physical, Melee]
        , cost    = χ [Blood]
        , cd      = 1
        , channel = Action 2
        , effects = [ (Enemy, damage 10 • pierce 5)
                    , (Self,  apply 1 [Ignore Stun, Reduce All Flat 15])
                    ]
        }
      , newSkill
        { label   = "Full Expansion"
        , desc    = "Chōji greatly enlarges himself and body-slams the enemy team, dealing 30 damage and stunning them for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Tai]
        , cd      = 2
        , effects = [(Enemies, damage 30 • apply 1 [Stun All])]
        }
      , newSkill
        { label   = "Chakra Wings"
        , desc    = "Chōji's overflowing chakra erupts around him. For 3 turns, it restores 15 health and bestows a random chakra. While active, Chōji does not take damage from [Chili Pill]."
        , classes = [Chakra]
        , cost    = χ [Nin, Nin]
        , cd      = 5
        , channel = Action 3
        , start   = [(Self, gain [Rand])]
        , effects = [(Self, heal 15 • apply 1 [ImmuneSelf])]
        }
      ]
    , [ newSkill
        { label   = "Chili Pill"
        , desc    = "Chōji swallows all three Akimichi pills, losing 10 health down to a minimum of 1 and gaining so much chakra that butterfly wings of pure energy erupt from his back. While alive, he loses 15 health per turn, provides 15 points of damage reduction to his allies, and ignores stuns."
        , classes = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , cost    = χ [Rand, Rand]
        , effects = [ (XAllies, apply' "Protected" 0 [Reduce All Flat 15])
                    , (Self, sacrifice 1 10 • apply 0 [Ignore Stun, Afflict 15]
                           • loadout 3 • setFace 0)
                    ]
        }
      , newSkill
        { label   = "Curry Pill"
        , desc    = "Chōji eats the second Akimichi pill, losing 5 health down to a minimum of 1 and unlocking huge reserves of chakra. While alive, he provides 10 points of damage reduction to his allies."
        , classes = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , effects = [ (XAllies, apply' "Protected" 0 [Reduce All Flat 10])
                    , (Self, sacrifice 1 5 • loadout 2)
                    ]
        }
      , newSkill
        { label   = "Chili Pill"
        , desc    = "Chōji eats the third Akimichi pill and gains so much chakra that butterfly wings of pure energy erupt from his back. While alive, he loses 15 health per turn, provides 15 points of damage reduction to his allies, and ignores stuns."
        , classes = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , effects = [ (XAllies, apply' "Protected" 0 [Reduce All Flat 15])
                    , (Self, apply 0 [Ignore Stun, Afflict 15]
                           • loadout 3 • vary "Block" "Block" • setFace 0)
                    ]
        }
      , newSkill
        { label   = "Butterfly Bombing"
        , desc    = "Chōji focuses all of his chakra into a fist and drives it into an enemy, dealing 45 damage and killing them if their health reaches 20 or lower."
        , classes = [Chakra]
        , cost    = χ [Nin, Tai]
        , cd      = 5
        , effects = [(Enemy, damage 45 • ifHealthU 0 20 kill)]
        }
      ]
    , invuln "Block" "Choji" [Physical] |>
        newSkill { label   = "Block"
                 , desc    = "Chōji becomes invulnerable for 1 turn. While active, Chōji does not take damage from [Chili Pill]." 
                 , classes = [Physical]
                 , cd      = 4
                 , cost    = χ [Rand]
                 , effects = [(Self, apply 1 [Invulnerable All, ImmuneSelf])]
                 , skPic   = True
                 } 
    ] []
  , Character
    "Ino Yamanaka"
    "A genin from Team 10, Ino is as confident as she is vain. She projects the strength of her will directly into the minds of her opponents, bypassing their physical defenses and rendering them helpless."
    [ [ newSkill
        { label   = "Mind Destruction"
        , desc    = "Ino launches a mental assault on an enemy, stunning their non-mental skills and preventing them from reducing damage or becoming invulnerable for 1 turn. Acting through them, she lashes out and deals 30 piercing damage to a random enemy."
        , classes = [Mental, Ranged, Bypassing]
        , cost    = χ [Gen, Rand]
        , effects = [ (Enemy,  apply 1 [Stun NonMental, Expose])
                    , (REnemy, pierce 30)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Mind Transfer"
        , desc    = "Ino takes over the mind of an enemy, stunning them and preventing them from reducing damage or becoming invulnerable for 4 turns. While active, this skill becomes [Art of the Valentine][r]."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Gen]
        , cd      = 3
        , channel = Control 4
        , start   = [(Self, vary "Mind Transfer" "Art of the Valentine")]
        , effects = [(Enemy, apply 1 [Stun All, Expose])]
        }
      , newSkill
        { label   = "Art of the Valentine"
        , desc    = "Deals 25 damage to an enemy."
        , classes = [Mental, Ranged]
        , cost    = χ [Rand]
        , effects = [(Enemy, damage 25)]
        }
      ]
    , [ newSkill
        { label   = "Chakra Hair Trap"
        , desc    = "Ino endows a strand of hair with chakra to create an ensnaring trap near an enemy. If they use a harmful skill during their next turn, their cooldowns will increase by 1 for 2 turns."
        , classes = [Chakra, Ranged, Invisible]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, trap 1 OnHarm § apply 2 [Snare 1])]
        }
      ]
    , invuln "Block" "Ino" [Physical]
    ] []
  , Character
    "Rock Lee"
    "A member of team Guy, Lee is a genin whose inability to use ninjutsu and genjutsu has led him to devote his life to mastering taijutsu. His strength increases with every strike, building up unstoppable momentum as the fight goes on."
    [ [ newSkill
        { label   = "Ferocious Fist"
        , desc    = "For 3 turns, Lee deals 10 damage to an enemy and gains 10 points of damage reduction. Deals 15 additional damage during [Fifth Gate Opening]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , channel = Action 3
        , effects = [ (Enemy, withI "Fifth Gate Opening" 15 damage 10 • tag 1)
                    , (Self,  apply 1 [Reduce All Flat 1 ])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Primary Lotus"
        , desc    = "Having opened the first gate, Lee uses a high-powered taijutsu to deal 30 damage to an enemy. Deals 10 additional damage to the target of [Ferocious Fist]. Deals 30 additional damage during [Fifth Gate Opening]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, withU "Ferocious Fist" 10
                            (withI "Fifth Gate Opening" 30 damage) 30)]
        }
      ]
    , [ newSkill
        { label   = "Fifth Gate Opening"
        , desc    = "Lee cures himself of enemy effects, loses 50 health down to a minimum of 1, and becomes invulnerable for 2 turns. While active, this skill becomes [Final Lotus][t][t]."
        , classes = [Mental]
        , cost    = χ [Tai]
        , cd      = 4
        , effects = [(Self, cureAll • apply 2 [Invulnerable All] 
                          • sacrifice 1 50
                          • vary' 2 "Fifth Gate Opening" "Final Lotus")]
        }
      , newSkill
        { label   = "Final Lotus"
        , desc    = "Deals 100 damage to an enemy."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Tai]
        , effects = [(Enemy, damage 100)]
        }
      ]
    , invuln "Block" "Lee" [Physical]
    ] []
  , Character
    "Tenten"
    "A member of Team Guy, Tenten is a fiercely independent weapon specialist who intends to become a legendary kunoichi. She keeps an entire arsenal sealed in scrolls to rain down upon the battlefield, wounding and disabling her opponents."
    [ [ newSkill
        { label   = "Unsealing Technique"
        , desc    = "Tenten launches a barrage of weapons at an enemy, dealing 20 damage to them and 10 damage to the rest of their team. Each use of [Unsealing Technique] further empowers her next [Rising Dragon Control]."
        , classes = [Physical, Ranged, Uncounterable]
        , cost    = χ [Tai]
        , effects = [ (Enemy,    damage 20)
                    , (XEnemies, damage 10)
                    , (Self,     addStack • ifI "Rising Twin Dragons" addStack
                               • remove "Rising Twin Dragons")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Rising Dragon Control"
        , desc    = "Tenten's weapons scattered across the battlefield shoot upward, spending all stacks of [Unsealing Technique] to deal 5 damage plus 10 per stack to all enemies. For 1 turn, non-mental damage of all enemies is weakened by 5 plus 10 per stack spent."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand]
        , effects = [ (Enemies, perI "Unsealing Technique" 10 damage 5
                              • ifnotI "Rising Twin Dragons" 
                                § perI "Unsealing Technique" 10
                                 (applyX 1 $ Weaken NonMental Flat) 5
                              • ifI "Rising Twin Dragons" 
                                § perI "Unsealing Technique" 10
                                 (applyX 2 $ Weaken NonMental Flat) 5)
                    , (Self,    remove "Unsealing Technique"
                              • remove "Rising Twin Dragons")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Rising Twin Dragons"
        , desc    = "Summoning scrolls conceal Tenten in a cloud of smoke, rendering her invulnerable to physical and chakra skills for 1 turn. The scrolls aid her the next time she uses one of her other skills. If she uses [Unsealing Technique], it adds 1 additional stack of [Unsealing Technique]. If she uses [Rising Dragon Control], the duration of its effect is increased by 1 turn."
        , classes = [Physical]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Self, tag 0 
                          • apply' "Twin Dragons Defense" 1 
                            [Invulnerable Physical, Invulnerable Chakra])]
        }
      ]
    , invuln "Spiked Boulder Shield" "Tenten" [Physical]
    ] []
  , Character
    "Neji Hyūga"
    "A member of team Guy, Neji is the cold and fatalistic prodigy of the Hyūga clan. He has mastered numerous techniques, from crippling blows to whirling invulnerability."
    [ [ newSkill
        { label   = "Gentle Fist"
        , desc    = "Using the Hyūga clan's signature taijutsu, Neji deals 25 damage to an enemy for 2 turns. During this time, that enemy's non-affliction damage is weakened by 5."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , cd      = 1
        , channel = Action 2
        , effects = [(Enemy, damage 25 • apply 1 [Weaken All Flat 5])]
        }
      ]
    , [ newSkill
        { label   = "Eight Trigrams Palm Rotation"
        , desc    = "Neji becomes invulnerable for 1 turn and deals 15 damage to all enemies."
        , classes = [Chakra, Melee]
        , cost    = χ [Blood]
        , cd      = 1
        , effects = [ (Self,    apply 1 [Invulnerable All])
                    , (Enemies, damage 15)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Eight Trigrams Sixty-Four Palms"
        , desc    = "Neji seals an enemy's chakra nodes shut with a rapid sequence of blows, dealing 40 damage and removing 1 chakra."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Tai]
        , cd      = 1
        , effects = [(Enemy, drain 1 • damage 40)]
        }
      ]
    , invuln "Byakugan Foresight" "Neji" [Mental]
    ] []
  , Character
    "Gaara"
    "The youngest of the sand siblings, Gaara is the unstable jinchūriki of Shukaku, the one-tailed beast. With its aid, Gaara manipulates the sand in his gourd to crush his enemies. The shell of sand around his body makes him invulnerable to all but the strongest of blows."
    [ [ newSkill
        { label   = "Sand Coffin"
        , desc    = "Sand surrounds an enemy, stunning their non-mental skills for 2 turns. While active, the enemy cannot reduce damage or become invulnerable and this skill becomes [Sand Burial][n][n]."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin, Rand]
        , cd      = 2
        , channel = Control 2
        , start   = [(Self, vary "Sand Coffin" "Sand Burial")]
        , effects = [(Enemy, apply 1 [Expose, Stun NonMental])]
        }
      , newSkill
        { label   = "Sand Burial"
        , desc    = "Kills the target of [Sand Coffin]."
        , require = HasU "Sand Coffin"
        , classes = [Physical, Ranged]
        , cost    = χ [Nin, Nin]
        , effects = [(Enemies, killThen 
                             . self § cancelChannel "Sand Coffin" ° heal 10)]
        }
      ]
    , [ newSkill
        { label   = "Sand Clone"
        , desc    = "Gaara creates a clone of sand that mimics him and provides defense. Until Gaara receives new non-affliction damage from an enemy, he ignores harmful non-damage effects other than chakra cost changes. Cannot be used while active."
        , classes = [Physical, Single, Unremovable]
        , cd      = 2
        , effects = [(Self, apply 0 [Enrage])]
        }
      ]
    , [ newSkill
        { label   = "Sand Armor"
        , desc    = "By covering himself with sand, Gaara gains 40 non-stacking permanent destructible defense."
        , classes = [Nonstacking, Chakra]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [(Self, defend 0 40)]
        }
      ]
    , invuln "Sand Shield" "Gaara" [Physical]
    ] 
    [(OnDamaged NonAffliction, N.removeOwn "Sand Clone")]
  , Character
    "Kankurō"
    "One of the three sand siblings, Kankurō is a master puppeteer. His strategically-placed poison bombs pierce through enemy defenses. In order to harm him, his enemies will have to go through his army of puppets first."
    [ [ newSkill
        { label   = "Iron Maiden"
        , desc    = "Two of Kankurō's puppets trap and stab an enemy, dealing 30 piercing damage. Deals 5 additional damage if used within 4 turns of [Puppet Technique]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand, Rand]
        , effects = [(Enemy, withI "Puppet Technique" 5 pierce 30)]
        }
      ]
    , [ newSkill
        { label   = "Poison Bomb"
        , desc    = "One of Kankurō's puppets creates a cloud of smoke that deals 10 affliction damage to all enemies. Deals 5 additional damage if used within 4 turns of [Puppet Technique]."
        , classes = [Bane, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemies, withI "Puppet Technique" 5 afflict 10)]
        }
      ]
    , [ newSkill
        { label   = "Puppet Technique"
        , desc    = "Kankurō fashions a chakra-controlled puppet which serves as a decoy, providing 15 permanent destructible defense."
        , classes = [Physical]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Self, defend 0 15 • tag 4)]
        }
      ]
    , invuln "Substitution Technique" "Kankurō" [Chakra]
    ] []
  , Character
    "Temari"
    "The older sister of Gaara and Kankurō, Temari is a cruel and blunt expert in war-fan combat. She uses her massive iron fan to generate powerful gusts of wind that repel attacks on her team."
    [ [ newSkill
        { label    = "Cyclone Scythe"
        , desc     = "Temari creates a razor sharp wind with her fan that hits an enemy, dealing 20 damage. The following turn, she is invulnerable to all harmful non-mental skills."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin]
        , effects = [ (Enemy, damage 20)
                    , (Self,  apply 1 [Invulnerable NonMental])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Blade Dance"
        , desc    = "The wind weasel Kamatari uses a massive version of Cyclone Scythe to deal 35 damage to all enemies."
        , classes = [Physical, Ranged, Summon]
        , cost    = χ [Nin, Rand, Rand]
        , cd      = 2
        , effects = [(Enemies, damage 35)]
        }
      ]
    , [ newSkill
        { label   = "Sandstorm"
        , desc    = "Temari kicks up dust with her fan, granting her team invulnerability for 1 turn and weakening all enemies' non-affliction damage by 15 for 2 turns."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin, Nin]
        , cd      = 5
        , effects = [ (Allies,  apply 1 [Invulnerable All])
                    , (Enemies, apply 2 [Weaken All Flat 15])
                    ]
        }
      ]
    , invuln "Block" "Temari" [Physical]
    ] []
  ]
