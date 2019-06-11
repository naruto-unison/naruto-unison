{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Characters.Original.Kids (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Naruto Uzumaki"
    "A genin from Team 7, Naruto is an orphan with the goal of becoming Hokage. His signature Shadow Clones copy his moves to perform powerful combos and wield the Rasengan."
    [ [ Skill.new
        { Skill.name      = "Naruto Uzumaki Barrage"
        , Skill.desc      = "Using his version of the Lions Barrage, Naruto deals 10 damage to an enemy. Each Shadow Clone deals 5 damage to the target as well. Spends one Shadow Clone if Naruto has any."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy do
                stacks <- userStacks "Shadow Clone"
                damage (10 + 5 * stacks)
          , p REnemy $ whenM (userHas "Shadow Clone") $ damage 5
          , p Self   $ removeStack "Shadow Clone"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rasengan"
        , Skill.desc      = "Naruto hits an enemy with an orb of chakra, dealing 30 damage. Spends two Shadow Clones. The target is stunned 1 turn for every 2 Shadow Clones remaining."
        , Skill.require   = HasI 2 "Shadow Clone"
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self   $ removeStacks "Shadow Clone" 2
          , p Enemy  do
                damage 30
                stacks <- userStacks "Shadow Clone"
                apply (stacks `quot` 2) [Stun All]
          , p REnemy $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Clones"
        , Skill.desc      = "Naruto creates 6 Shadow Clones who hide him and fight in his place. Each shadow clone provides 5 points of damage reduction. Each time an enemy uses a skill on Naruto, a shadow clone deals 5 damage to them and disappears. Each time Naruto loses a shadow clone by using a skill, the shadow clone deals 5 damage to a random enemy. Cannot be used while Naruto has shadow clones remaining."
        , Skill.require   = HasI (-1) "Shadow Clone"
        , Skill.classes   = [Chakra, Unremovable, TrapAttack]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self do
            trapFrom' 0 (OnHarmed All) do
                whenM (userHas "Shadow Clone") $ damage 5
                self $ removeStack "Shadow Clone"
                unlessM (userHas "Shadow Clone") $ removeTrap "Shadow Clones"
            replicateM_ 6 $
                apply' "Shadow Clone" 0 [Reduce All Flat 5]
          ]
        }
      ]
    , [ (invuln "Sexy Technique" "Naruto" [Chakra])
          { Skill.desc = "Naruto and his shadow clones become invulnerable for 1 turn." }
      ]
    ] []
  , Character
    "Sakura Haruno"
    "A genin from Team 7, Sakura is intelligent but self-conscious. Her recent training from Tsunade has prepared her to heal her allies and knock back her enemies."
    [ [ Skill.new
        { Skill.name      = "KO Punch"
        , Skill.desc      = "Sakura punches an enemy with all her strength, dealing 20 damage and stunning their physical and mental skills for 1 turn. Deals 10 additional damage during [Inner Sakura]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 10 `bonusIf` userHas "Inner Sakura"
                damage (20 + bonus)
                apply 1 [Stun Mental, Stun Physical]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mystical Palm Healing"
        , Skill.desc      = "Using basic healing techniques, Sakura restores 25 health to herself or an ally."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Ally $ heal 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inner Sakura"
        , Skill.desc      = "Sakura's inner self surfaces and urges her on. For 4 turns, Sakura gains 10 points of damage reduction and ignores all non-damage effects other than chakra cost changes."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self $ apply 4 [Enrage, Reduce All Flat 10] ]
        }
      ]
    , [ invuln "Substitution Technique" "Sakura" [Chakra] ]
    ] []
  , Character
    "Sasuke Uchiha"
    "A genin from Team 7, Sasuke seeks vengeance against his brother for slaughtering the rest of the Uchiha clan. Using his sharingan, Sasuke targets his opponent's weak spots and anticipates their attacks."
    [ [ Skill.new
        { Skill.name      = "Lions Barrage"
        , Skill.desc      = "Copying a taijutsu combo that Lee used on him, Sasuke deals 30 damage to an enemy. Deals 15 additional damage to an enemy affected by [Sharingan]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 15 `bonusIf` targetHas "Sharingan"
                damage (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Sasuke attacks an enemy with a bolt of lightning, dealing 30 piercing damage. Deals 25 additional damage to an enemy affected by [Sharingan]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                bonus <- 25 `bonusIf` targetHas "Sharingan"
                pierce (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Sasuke focuses his gaze on an enemy. For 4 turns, he gains 15 points of damage reduction and the enemy cannot reduce damage or become invulnerable. Ends if Sasuke dies."
        , Skill.classes   = [Mental, Ranged, Soulbound, Unremovable]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self  $ apply 4 [Reduce All Flat 10]
          , p Enemy $ apply 4 [Expose]
          ]
        }
      ]
    , [ invuln "Block" "Sasuke" [Physical] ]
    ] []
  , Character
    "Kiba Inuzuka"
    "A genin from Team 8, Kiba is short-tempered and impulsive. His powerful taijutsu skills are amplified when he fuses with his dog, Akamaru, and transforms into a double-headed monster."
    [ [ Skill.new
        { Skill.name      = "Wolf Fang"
        , Skill.desc      = "Kiba projects a vacuum vortex at an enemy, dealing 30 damage. Deals 5 additional damage to an enemy affected by [Dynamic Marking]. Costs 1 taijutsu chakra during [Two-Headed Wolf]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 5 `bonusIf` targetHas "Dynamic Marking"
                damage (30 + bonus)
          ]
        , Skill.changes   = changeWith "Two-Headed Wolf" $ setCost [Tai]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Two-Headed Wolf"
        , Skill.desc      = "Kiba and Akamaru transform into giant beasts, gaining 15 points of damage reduction and dealing 15 damage to all enemies for 3 turns."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Blood, Tai]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Enemies $ damage 15
          , p Self    $ apply 1 [Reduce All Flat 15]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dynamic Marking"
        , Skill.desc      = "Akamaru sprays urine on an enemy, preventing them from reducing damage or becoming invulnerable for 3 turns. Cannot be used on an enemy already affected by this skill."
        , Skill.classes   = [Ranged, Single]
        , Skill.effects   =
          [ p Enemy $ apply 3 [Expose] ]
        }
      ]
    , [ invuln "Smoke Bomb" "Kiba" [Physical] ]
    ] []
  , Character
    "Shino Aburame"
    "A genin from Team 8, Shino is reserved and tactical. He directs the insects living within his body to leech chakra from his enemies and protect his teammates."
    [ [ Skill.new
        { Skill.name      = "Chakra Leech"
        , Skill.desc      = "Chakra-draining bugs attack an enemy, dealing 20 affliction damage and absorbing 1 random chakra. Deals 5 additional damage per target's stack of [Parasite]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = k [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                absorb 1
                stacks <- targetStacks "Parasite"
                afflict (20 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Parasite"
        , Skill.desc      = "Shino directs one of his bugs to attach itself to an enemy. For 4 turns, the target's non-affliction damage is weakened by 5."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ apply 4 [Weaken All Flat 5] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wall of Insects"
        , Skill.desc      = "A massive swarm of insects surrounds Shino's team, providing 20 permanent destructible defense to them."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Allies $ defend 0 20 ]
        }
      ]
    , [ invuln "Insect Clone" "Shino" [Chakra] ]
    ] []
  , Character
    "Hinata Hyūga"
    "A genin from Team 8, Hinata is the shy and withdrawn heiress of the Hyūga clan. With her clan's signature Byakugan, she senses her opponent's energy flow and blocks it at key points to remove their chakra."
    [ [ Skill.new
        { Skill.name      = "Gentle Fist"
        , Skill.desc      = "Using the Hyūga clan's signature taijutsu style, Hinata deals 20 damage to an enemy for 2 turns. During [Byakugan], depletes 1 random chakra of the opposing team each turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.channel   = Action 2
        , Skill.effects   =
            [ p Enemy do
                  whenM (userHas "Byakugan") $ deplete 1
                  damage 20
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Sixty-Four Palms"
        , Skill.desc      = "Hinata deals 15 damage to all enemies and provides 10 destructible defense to her team for 1 turn. Deals 5 additional damage to all enemies during [Byakugan]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemies do
                bonus <- 5 `bonusIf` userHas "Byakugan"
                damage (15 + bonus)
          , p Allies  $ defend 1 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Byakugan"
        , Skill.desc      = "Hinata activates her Byakugan, gaining 15 points of damage reduction for 4 turns."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self $ apply 4 [Reduce All Flat 15] ]
        }
      ]
    , [ invuln "Block" "Hinata" [Physical] ]
    ] []
  , Character
    "Shikamaru Nara"
    "A genin from Team 10, Shikamaru is considered the smartest of the Konoha 11. He manipulates shadows to disable and attack his enemies. Whomever he fights, it's only a matter of time before he comes up with the perfect plan to defeat them."
    [ [ Skill.new
        { Skill.name      = "Meditate"
        , Skill.desc      = "Shikamaru sits down and contemplates an enemy. Over the next 5 turns, he composes a strategy against them. Cannot be used on an enemy already affected by this skill."
        , Skill.classes   = [Single, Mental, Ranged, Uncounterable, Unreflectable]
        , Skill.effects   =
          [ p Enemy $ tag 5 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Strangle"
        , Skill.desc      = "Shadow tendrils choke Shikamaru's enemies, dealing 15 damage and preventing them from reducing damage or becoming invulnerable for 1 turn. Enemies affected by [Meditate] are exposed for 2 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
            [ p Enemies do
                  damage 15
                  bonus <- 1 `bonusIf` targetHas "Meditate"
                  apply (1 + bonus) [Expose]
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shadow Possession"
        , Skill.desc      = "Shikamaru captures all enemies in shadows, stunning non-mental skills for 1 turn. Enemies affected by [Meditate] are stunned for 2 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Gen, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemies do
                bonus <- 1 `bonusIf` targetHas "Meditate"
                apply (1 + bonus) [Stun NonMental]
          ]
        }
      ]
    , [ invuln "Hide" "Shikamaru" [Mental] ]
    ] []
  , let loadout = (0, 0, 0, False)
    in Character
    "Chōji Akimichi"
    "A genin from Team 10, Chōji is a voracious eater and loyal friend. His clan's special pills immensely magnify his innate strength and unlock different abilities, but the toll they take on his body can kill him if he pushes himself too far."
    [ [ Skill.new
        { Skill.name      = "Spinach Pill"
        , Skill.desc      = "Chōji eats the mildest Akimichi pill, losing 5 health down to a minimum of 1 and gaining the strength he needs to protect his friends. While alive, he provides 5 points of damage reduction to his allies."
        , Skill.classes   = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , Skill.effects   =
          [ p XAllies $ apply' "Protected" 0 [Reduce All Flat 5]
          , p Self    do
                sacrifice 1 5
                varyLoadout loadout 1
          ]
        }
      , Skill.new
        { Skill.name      = "Obstructing Tackle"
        , Skill.desc      = "Chōji charges an enemy, dealing 20 damage to them and weakening their damage by 20 for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 20
                apply 1 [Weaken All Flat 20]
          ]
        }
      , Skill.new
        { Skill.name       = "Partial Expansion"
        , Skill.desc       = "Chōji greatly enlarges one of his arms and swings it through the enemy team, dealing 20 damage."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemies $ damage 20 ]
        }
      , Skill.new
        { Skill.name      = "Justice Punch"
        , Skill.desc      = "Chōji slams his fist into an enemy and knocks them to the ground, dealing 25 damage and stunning them for 1 turn. While stunned, they cannot reduce damage or become invulnerable."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 25
                apply 1 [Stun All, Expose]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Curry Pill"
        , Skill.desc      = "Chōji eats the first two Akimichi pills in one go, losing 15 health down to a minimum of 1 and unlocking huge reserves of chakra in addition to immense physical strength. While alive, he provides 10 points of damage reduction to his allies."
        , Skill.classes   = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
            [ p XAllies $ apply' "Protected" 0 [Reduce All Flat 10]
            , p Self    do
                  sacrifice 1 15
                  varyLoadout loadout 2
            ]
        }
      , Skill.new
        { Skill.name      = "Human Boulder"
        , Skill.desc      = "Chōji enlarges his body, tucks in his limbs, and uses chakra to propel himself into an unstoppable roll. For 2 turns, he deals 10 damage and 5 piercing damage to an enemy. While active, Chōji gains 15 points of damage reduction and ignores stuns."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ p Enemy do
                damage 10
                pierce 5
          , p Self  $ apply 1 [Ignore Stun, Reduce All Flat 15]
          ]
        }
      , Skill.new
        { Skill.name      = "Full Expansion"
        , Skill.desc      = "Chōji greatly enlarges himself and body-slams the enemy team, dealing 30 damage and stunning them for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemies do
                damage 30
                apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "Chakra Wings"
        , Skill.desc      = "Chōji's overflowing chakra erupts around him. For 3 turns, it restores 15 health and bestows 1 random chakra. While active, Chōji does not take damage from [Chili Pill]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 5
        , Skill.channel   = Action 3
        , Skill.start     =
          [ p Self $ gain [Rand] ]
        , Skill.effects   =
          [ p Self do
                heal 15
                apply 1 [ImmuneSelf]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chili Pill"
        , Skill.desc      = "Chōji swallows all three Akimichi pills, losing 10 health down to a minimum of 1 and gaining so much chakra that butterfly wings of pure energy erupt from his back. While alive, he loses 15 health per turn, provides 15 points of damage reduction to his allies, and ignores stuns."
        , Skill.classes   = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , Skill.cost      = k [Rand, Rand]
        , Skill.effects   =
          [ p XAllies $ apply' "Protected" 0 [Reduce All Flat 15]
          ,  p Self   do
                sacrifice 1 10
                apply 0 [Ignore Stun, Afflict 15]
                varyLoadout loadout 3
                setFace 0
          ]
        }
      , Skill.new
        { Skill.name      = "Curry Pill"
        , Skill.desc      = "Chōji eats the second Akimichi pill, losing 5 health down to a minimum of 1 and unlocking huge reserves of chakra. While alive, he provides 10 points of damage reduction to his allies."
        , Skill.classes   = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , Skill.effects   =
          [ p XAllies $ apply' "Protected" 0 [Reduce All Flat 10]
          , p Self do
                sacrifice 1 5
                varyLoadout loadout 2
          ]
        }
      , Skill.new
        { Skill.name      = "Chili Pill"
        , Skill.desc      = "Chōji eats the third Akimichi pill and gains so much chakra that butterfly wings of pure energy erupt from his back. While alive, he loses 15 health per turn, provides 15 points of damage reduction to his allies, and ignores stuns."
        , Skill.classes   = [Chakra, Soulbound, Nonstacking, Unreflectable, Unremovable]
        , Skill.effects   =
            [ p XAllies $ apply' "Protected" 0 [Reduce All Flat 15]
            , p Self    do
                  apply 0 [Ignore Stun, Afflict 15]
                  varyLoadout loadout 3
                  vary "Block" "Block"
                  setFace 0
            ]
        }
      , Skill.new
        { Skill.name      = "Butterfly Bombing"
        , Skill.desc      = "Chōji focuses all of his chakra into a fist and drives it into an enemy, dealing 45 damage and killing them if their health reaches 20 or lower."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Nin, Tai]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ p Enemy do
                damage 45
                hp <- target health
                when (hp <= 20) kill
          ]
        }
      ]
    , [ invuln "Block" "Choji" [Physical]
      , Skill.new { Skill.name     = "Block"
                  , Skill.desc      = "Chōji becomes invulnerable for 1 turn. While active, Chōji does not take damage from [Chili Pill]."
                  , Skill.classes   = [Physical]
                  , Skill.cooldown  = 4
                  , Skill.cost      = k [Rand]
                  , Skill.effects   =
                    [ p Self $ apply 1 [Invulnerable All, ImmuneSelf] ]
                  , Skill.pic       = True
                  }
      ]
    ] []
  , Character
    "Ino Yamanaka"
    "A genin from Team 10, Ino is as confident as she is vain. She projects the strength of her will directly into the minds of her opponents, bypassing their physical defenses and rendering them helpless."
    [ [ Skill.new
        { Skill.name      = "Mind Destruction"
        , Skill.desc      = "Ino launches a mental assault on an enemy, stunning their non-mental skills and preventing them from reducing damage or becoming invulnerable for 1 turn. Acting through them, she lashes out and deals 30 piercing damage to a random enemy."
        , Skill.classes   = [Mental, Ranged, Bypassing]
        , Skill.cost      = k [Gen, Rand]
        , Skill.effects   =
          [ p Enemy  $ apply 1 [Stun NonMental, Expose]
          , p REnemy $ pierce 30
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mind Transfer"
        , Skill.desc      = "Ino takes over the mind of an enemy, stunning them and preventing them from reducing damage or becoming invulnerable for 4 turns. While active, this skill becomes [Art of the Valentine][r]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Gen, Gen]
        , Skill.cooldown  = 3
        , Skill.channel   = Control 4
        , Skill.start     =
          [ p Self $ vary "Mind Transfer" "Art of the Valentine"]
        , Skill.effects   =
          [ p Enemy $ apply 1 [Stun All, Expose] ]
        }
      , Skill.new
        { Skill.name      = "Art of the Valentine"
        , Skill.desc      = "Deals 25 damage to an enemy."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemy $ damage 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Hair Trap"
        , Skill.desc      = "Ino endows a strand of hair with chakra to create an ensnaring trap near an enemy. If they use a harmful skill during their next turn, their cooldowns will increase by 1 for 2 turns."
        , Skill.classes   = [Chakra, Ranged, Invisible]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ trap 1 OnHarm $ apply 2 [Snare 1] ]
        }
      ]
    , [ invuln "Block" "Ino" [Physical] ]
    ] []
  , Character
    "Rock Lee"
    "A member of team Guy, Lee is a genin whose inability to use ninjutsu and genjutsu has led him to devote his life to mastering taijutsu. His strength increases with every strike, building up unstoppable momentum as the fight goes on."
    [ [ Skill.new
        { Skill.name      = "Ferocious Fist"
        , Skill.desc      = "For 3 turns, Lee deals 10 damage to an enemy and gains 10 points of damage reduction. Deals 15 additional damage during [Fifth Gate Opening]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Enemy do
                bonus <- 15 `bonusIf` userHas "Fifth Gate Opening"
                damage (10 + bonus)
                tag 1
          , p Self $ apply 1 [Reduce All Flat 1 ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Primary Lotus"
        , Skill.desc      = "Having opened the first gate, Lee uses a high-powered taijutsu to deal 30 damage to an enemy. Deals 10 additional damage to the target of [Ferocious Fist]. Deals 30 additional damage during [Fifth Gate Opening]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                targetBonus <- 10 `bonusIf` targetHas "Ferocious Fist"
                userBonus   <- 30 `bonusIf` userHas "Fifth Gate Opening"
                damage (30 + targetBonus + userBonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fifth Gate Opening"
        , Skill.desc      = "Lee cures himself of enemy effects, loses 50 health down to a minimum of 1, and becomes invulnerable for 2 turns. While active, this skill becomes [Final Lotus][t][t]."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                cureAll
                apply 2 [Invulnerable All]
                sacrifice 1 50
                vary' 2 "Fifth Gate Opening" "Final Lotus"
          ]
        }
      , Skill.new
        { Skill.name      = "Final Lotus"
        , Skill.desc      = "Deals 100 damage to an enemy."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Tai]
        , Skill.effects   =
          [ p Enemy $ damage 100 ]
        }
      ]
    , [ invuln "Block" "Lee" [Physical] ]
    ] []
  , Character
    "Tenten"
    "A member of Team Guy, Tenten is a fiercely independent weapon specialist who intends to become a legendary kunoichi. She keeps an entire arsenal sealed in scrolls to rain down upon the battlefield, wounding and disabling her opponents."
    [ [ Skill.new
        { Skill.name      = "Unsealing Technique"
        , Skill.desc      = "Tenten launches a barrage of weapons at an enemy, dealing 20 damage to them and 10 damage to the rest of their team. Each use of [Unsealing Technique] further empowers her next [Rising Dragon Control]."
        , Skill.classes   = [Physical, Ranged, Uncounterable]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy    $ damage 20
          , p XEnemies $ damage 10
          , p Self do
                addStack
                whenM (userHas "Rising Twin Dragons") addStack
                remove "Rising Twin Dragons"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rising Dragon Control"
        , Skill.desc      = "Tenten's weapons scattered across the battlefield shoot upward, spending all stacks of [Unsealing Technique] to deal 5 damage plus 10 per stack to all enemies. For 1 turn, non-mental damage of all enemies is weakened by 5 plus 10 per stack spent."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemies do
                stacks <- userStacks "Unsealing Technique"
                damage (5 + 10 * stacks)
                bonus <- 1 `bonusIf` userHas "Rising Twin Dragons"
                apply (1 + bonus) [Weaken NonMental Flat (5 + 10 * stacks)]
          ,  p Self do
                remove "Unsealing Technique"
                remove "Rising Twin Dragons"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rising Twin Dragons"
        , Skill.desc      = "Summoning scrolls conceal Tenten in a cloud of smoke, rendering her invulnerable to physical and chakra skills for 1 turn. The scrolls aid her the next time she uses one of her other skills. If she uses [Unsealing Technique], it adds 1 additional stack of [Unsealing Technique]. If she uses [Rising Dragon Control], the duration of its effect is increased by 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self do
                tag 0
                apply' "Twin Dragons Defense" 1
                    [Invulnerable Physical, Invulnerable Chakra]
          ]
        }
      ]
    , [ invuln "Spiked Boulder Shield" "Tenten" [Physical] ]
    ] []
  , Character
    "Neji Hyūga"
    "A member of team Guy, Neji is the cold and fatalistic prodigy of the Hyūga clan. He has mastered numerous techniques, from crippling blows to whirling invulnerability."
    [ [ Skill.new
        { Skill.name      = "Gentle Fist"
        , Skill.desc      = "Using the Hyūga clan's signature taijutsu, Neji deals 25 damage to an enemy for 2 turns. During this time, that enemy's non-affliction damage is weakened by 5."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.channel   = Action 2
        , Skill.effects   =
            [ p Enemy do
                damage 25
                apply 1 [Weaken All Flat 5]
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Palm Rotation"
        , Skill.desc      = "Neji becomes invulnerable for 1 turn and deals 15 damage to all enemies."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
            [ p Self    $ apply 1 [Invulnerable All]
            , p Enemies $ damage 15
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eight Trigrams Sixty-Four Palms"
        , Skill.desc      = "Neji seals an enemy's chakra nodes shut with a rapid sequence of blows, dealing 40 damage and depleting 1 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Blood, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                deplete 1
                damage 40
          ]
        }
      ]
    , [ invuln "Byakugan Foresight" "Neji" [Mental] ]
    ] []
  , Character
    "Gaara"
    "The youngest of the sand siblings, Gaara is the unstable jinchūriki of Shukaku, the one-tailed beast. With its aid, Gaara manipulates the sand in his gourd to crush his enemies. The shell of sand around his body makes him invulnerable to all but the strongest of blows."
    [ [ Skill.new
        { Skill.name      = "Sand Coffin"
        , Skill.desc      = "Sand surrounds an enemy, stunning their non-mental skills for 2 turns. While active, the enemy cannot reduce damage or become invulnerable and this skill becomes [Sand Burial][n][n]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.channel   = Control 2
        , Skill.start     =
          [ p Self $ vary "Sand Coffin" "Sand Burial"]
        , Skill.effects   =
          [ p Enemy $ apply 1 [Expose, Stun NonMental] ]
        }
      , Skill.new
        { Skill.name      = "Sand Burial"
        , Skill.desc      = "Kills the target of [Sand Coffin]."
        , Skill.require   = HasU "Sand Coffin"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin, Nin]
        , Skill.effects   =
          [ p Enemies do
              kill
              unlessM (target alive) . self $ cancelChannel "Sand Coffin"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Clone"
        , Skill.desc      = "Gaara creates a clone of sand that mimics him and provides defense. Until Gaara receives new non-affliction damage from an enemy, he ignores harmful non-damage effects other than chakra cost changes. Cannot be used while active."
        , Skill.classes   = [Physical, Single, Unremovable]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Self do
                apply 0 [Enrage]
                trap 0 (OnDamaged NonAffliction) do
                    remove "Sand Clone"
                    removeTrap "Sand Clone"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Armor"
        , Skill.desc      = "By covering himself with sand, Gaara gains 40 non-stacking permanent destructible defense."
        , Skill.classes   = [Nonstacking, Chakra]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self $ defend 0 40 ]
        }
      ]
    , [ invuln "Sand Shield" "Gaara" [Physical] ]
    ] []
  , Character
    "Kankurō"
    "One of the three sand siblings, Kankurō is a master puppeteer. His strategically-placed poison bombs pierce through enemy defenses. In order to harm him, his enemies will have to go through his army of puppets first."
    [ [ Skill.new
        { Skill.name      = "Iron Maiden"
        , Skill.desc      = "Two of Kankurō's puppets trap and stab an enemy, dealing 30 piercing damage. Deals 5 additional damage if used within 4 turns of [Puppet Technique]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand, Rand]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 5 `bonusIf` userHas "Puppet Technique"
                pierce (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Poison Bomb"
        , Skill.desc      = "One of Kankurō's puppets creates a cloud of smoke that deals 10 affliction damage to all enemies. Deals 5 additional damage if used within 4 turns of [Puppet Technique]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies do
                bonus <- 5 `bonusIf` userHas "Puppet Technique"
                afflict (10 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Puppet Technique"
        , Skill.desc      = "Kankurō fashions a chakra-controlled puppet which serves as a decoy, providing 15 permanent destructible defense."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self do
                defend 0 15
                tag 4
          ]
        }
      ]
    , [ invuln "Substitution Technique" "Kankurō" [Chakra] ]
    ] []
  , Character
    "Temari"
    "The older sister of Gaara and Kankurō, Temari is a cruel and blunt expert in war-fan combat. She uses her massive iron fan to generate powerful gusts of wind that repel attacks on her team."
    [ [ Skill.new
        { Skill.name       = "Cyclone Scythe"
        , Skill.desc       = "Temari creates a razor sharp wind with her fan that hits an enemy, dealing 20 damage. The following turn, she is invulnerable to all harmful non-mental skills."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Enemy $ damage 20
          , p Self  $ apply 1 [Invulnerable NonMental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Blade Dance"
        , Skill.desc      = "The wind weasel Kamatari uses a massive version of Cyclone Scythe to deal 35 damage to all enemies."
        , Skill.classes   = [Physical, Ranged, Summon]
        , Skill.cost      = k [Nin, Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemies $ damage 35 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sandstorm"
        , Skill.desc      = "Temari kicks up dust with her fan, granting her team invulnerability for 1 turn and weakening all enemies' non-affliction damage by 15 for 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ p Allies  $ apply 1 [Invulnerable All]
          , p Enemies $ apply 2 [Weaken All Flat 15]
          ]
        }
      ]
    , [ invuln "Block" "Temari" [Physical] ]
    ] []
  ]
