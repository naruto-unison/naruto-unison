{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Shippuden.Jinchuriki (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Yugito Nii"
    "A jōnin from the Hidden Cloud Village, Yugito is the expert jinchūriki of Matatabi, the two-tailed beast. Having trained as a tailed-beast host since infancy, Yugito can effortlessly transform into Matatabi at will and has access to its full power."
    [ [ Skill.new
        { Skill.name      = "Two-Tailed Transformation"
        , Skill.desc      = "Matatabi's chakra envelops Yugito, transforming her into a huge two-tailed cat of blue flame. Yugito gains 50% damage reduction and can use her other skills."
        , Skill.require   = HasI 0 "Two-Tailed Transformation"
        , Skill.classes   = [Chakra]
        , Skill.effects   =
          [ To Self do
                apply 0 [Reduce All Percent 50]
                setFace
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flaming Cat Roar"
        , Skill.desc      = "A fireball engulfs an enemy, dealing 30 damage to them and weakening their damage by 10 for 1 turn. Every time this skill is used, its damage increases by 5."
        , Skill.require   = HasI 1 "Two-Tailed Transformation"
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Flaming Cat Roar"
                damage (30 + 5 * stacks)
                apply 1 [Weaken All Flat 10]
          , To Self addStack
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Cat Claws"
        , Skill.desc      = "Yugito rakes the enemy team with her claws, dealing 15 damage to an enemy and 5 damage to all other enemies. Every time this skill is used, its damage increases by 5."
        , Skill.require   = HasI 1 "Two-Tailed Transformation"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Cat Claws"
                damage (15 + 5 * stacks)
          , To XEnemies do
                stacks <- userStacks "Cat Claws"
                damage (5 + 5 * stacks)
          , To Self addStack
          ]
        }
      ]
    , [ invuln "Block" "Yugito" [Physical] ]
    ]
  , Character
    "Utakata"
    "An exile from the Hidden Mist Village, Utakata is the jinchūriki of Saiken, the six-tailed beast. He abandoned his home during its Blood Mist era, and has had to fend off tracker ninjas ever since."
    [ [ Skill.new
        { Skill.name      = "Soap Bubble"
        , Skill.desc      = "Utakata blows bubbles from his pipe that burst on an enemy, demolishing their destructible defense and his own destructible barrier, then dealing 25 piercing damage to the target."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                pierce 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Drowning Bubble"
        , Skill.desc      = "Soap bubbles surround an enemy's head and prevent them from breathing, dealing 10 affliction damage each turn. Cannot be used on an enemy already affected by this skill. Ends if Utakata dies."
        , Skill.require   = HasU 0 "Drowning Bubble"
        , Skill.classes   = [Chakra, Ranged, Bane, Soulbound]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy $ apply 0 [Afflict 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Six-Tailed Transformation"
        , Skill.desc      = "Saiken's chakra heals Utakata, restoring 25 health and providing him with 1 turn of 50% damage reduction to all types of damage, including piercing and affliction."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                heal 25
                apply 1 [Reduce Affliction Percent 50]
          ]
        }
      ]
    , [ invuln "Bubble Dome" "Utakata" [Chakra] ]
    ]
  , Character
    "Killer B"
    "You know his name, you know his fame, don't be lame!\nMakin' beats and rhymes, and makin' 'em' live, is what a jinchūriki needs to survive!\nWin after win is the way that it's done, and when he's done, you'll wish you never met, son!"
    [ [ Skill.new
        { Skill.name      = "Acrobat"
        , Skill.desc      = "For 4 turns, Killer B prepares for attacks. If you mess with the Bee, he'll sting ya right back!\n15 piercing damage back at any aggressor—it's the school of hard knocks, and B's the professor!"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ trapFrom 4 (OnHarmed All) $ pierce 15 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lariat"
        , Skill.desc      = "As the eight-tailed beast's chakra surrounds Killer B, he deals 20 piercing damage to one enemy.\nHe spends an additional arbitrary chakra during [Acrobat]'s funky flow to deal 20 extra damage with a punishing blow."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 20 `bonusIf` userHas "Acrobat"
                pierce (20 + bonus)
          ]
        , Skill.changes   =
            changeWith "Acrobat" \x -> x { Skill.cost = [Tai, Rand] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Octopus Hold"
        , Skill.desc      = "Clones form around B from ink that Gyūki spills. For 1 turn, they counter harmful non-mental skills.\nCountered foes take 20 piercing damage each, while Killer B strikes a cool pose, safely out of reach."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        }
      ]
    , [ (invuln "Octopus Leg Clone" "Killer B" [Chakra])
        { Skill.desc = "Deciding that he has cause for concern, Killer B becomes invulnerable for 1 turn.\nHe and Gyūki stage tactical retreats, and spend the time working on some ice-cold beats."
        }
      ]
    ]
  , Character
    "Eight-Tailed B"
    "Killer B works in perfect harmony with Gyūki, maintaining control of its immense quantities of tailed-beast chakra. With its bulk and might, B in jinchūriki mode is practically unassailable."
    [ [ Skill.new
        { Skill.name      = "Chakra Bones"
        , Skill.desc      = "Skeletal armor manifests from Gyūki's chakra, permanently increasing B's damage by 5 and providing him with 10% damage reduction to all types of damage, including piercing and affliction."
        , Skill.classes   = [Chakra]
        , Skill.cooldown  = 1
        , Skill.charges   = 5
        , Skill.effects   =
          [ To Self $
                apply 0 [Strengthen All Flat 5, Reduce Affliction Percent 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lariat"
        , Skill.desc      = "Rushing an enemy, B deals 20 damage to them and increases their cooldowns by 1 for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Snare 1]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Bomb"
        , Skill.desc      = "B launches a sphere of condensed chakra at an opponent, dealing 25 damage."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ damage 25 ]
        }
      ]
    , [ invuln "Chakra Barrier" "B" [Chakra] ]
    ]
  , let loadout = [1, 0, 0, 0]
    in Character
    "Nine-Tailed Naruto"
    "Rage has triggered the beast within Naruto to emerge. As his hatred grows, so does the nine-tailed beast's power. If left unchecked, Kurama may break free of his seal, and Naruto himself will cease to exist."
    [ [ Skill.new
        { Skill.name      = "Four-Tailed Transformation"
        , Skill.desc      = "Naruto's rage takes over. He loses 5 health down to a minimum of 1 and gains 10 points of damage reduction and 10 permanent destructible defense. He permanently ignores all healing. His other skills become usable, and will increase in strength as his transformation progresses through further stages. Once used, this skill becomes [Six-Tailed Transformation][b][r]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.effects   =
          [ To Self do
                sacrifice 1 5
                defend 0 10
                varyLoadout loadout 0
                setFace
                apply 0 [Reduce All Flat 10, Plague]
          ]
        }
      , Skill.new
        { Skill.name      = "Six-Tailed Transformation"
        , Skill.desc      = "Naruto's fury drives him to the brink of madness. He loses 10 health down to a minimum of 1 and gains 20 points of damage reduction and 20 permanent destructible defense. He permanently ignores status effects from enemies except chakra cost changes and is invulnerable to allies. The power of his other skills continues to grow. Once used, this skill becomes [Nine-Tailed Transformation][b][b]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Self do
                remove "Four-Tailed Transformation"
                sacrifice 1 10
                defend 0 20
                varyLoadout loadout 1
                setFace
                apply 0 [Reduce All Flat 20, Plague, Alone, Enrage]
          ]
        }
      , Skill.new
        { Skill.name      = "Nine-Tailed Transformation"
        , Skill.desc      = "As Naruto's mind is overwhelmed by wrath, the seal breaks and Kurama takes over, unlocking the full extent of his abilities. He loses 15 health down to a minimum of 1 and gains 30 points of damage reduction and 30 permanent destructible defense. Once used, this skill becomes [Raging Flames][b][r]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Self do
                remove "Six-Tailed Transformation"
                sacrifice 1 15
                defend 0 30
                varyLoadout loadout 2
                setFace
                apply 0 [Reduce All Flat 30, Plague, Alone, Enrage]
          ]
        }
      , Skill.new
        { Skill.name      = "Raging Flames"
        , Skill.desc      = "Finally emerging from years of imprisonment, Kurama is as cranky as he is powerful. He rains fire upon the enemy team, dealing 20 affliction damage and weakening their damage by 10 for 1 turn."
        , Skill.classes   = [Bane, Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                afflict 20
                apply 1 [Weaken All Flat 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Bomb"
        , Skill.desc      = "Naruto launches a sphere of condensed chakra at an opponent, dealing 30 piercing damage."
        , Skill.require   = HasI 1 "Four-Tailed Transformation"
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 30 ]
        }
      , Skill.new
        { Skill.name      = "Mini Tailed Beast Bomb Barrage"
        , Skill.desc      = "Naruto fires a volley of burning chakra orbs at an enemy, dealing 10 affliction damage to them for 3 turns. If used on an enemy affected by [Clasp], this skill deals all 30 damage instantly."
        , Skill.classes   = [Bane, Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                has <- targetHas "Clasp"
                if has then
                    afflict 30
                else
                    apply 3 [Afflict 10]
          ]
        }
      , Skill.new
        { Skill.name      = "Massive Tailed Beast Bomb"
        , Skill.desc      = "Kurama fires a gigantic sphere of condensed chakra at an enemy, dealing 60 piercing damage. Deals 40 additional damage if [Chakra Gathering] was used last turn."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 40 `bonusIf` userHas "Chakra Gathering"
                pierce (60 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Burning Chakra Hand"
        , Skill.desc      = "Naruto extends a limb made of chakra to reach out and grab an enemy, dealing 20 damage and weakening their damage by 5 for 1 turn."
        , Skill.require   = HasI 1 "Four-Tailed Transformation"
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy do
                afflict 20
                apply 1 [Weaken All Flat 5]
          ]
        }
      , Skill.new
        { Skill.name      = "Clasp"
        , Skill.desc      = "Naruto breaks through an enemy's defenses and takes hold of their head, dealing 10 damage and stunning their non-mental skills for 1 turn."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 10
                apply 1 [Stun NonMental]
          ]
        }
      , Skill.new
        { Skill.name      = "Chakra Gathering"
        , Skill.desc      = "Kurama draws in chakra to improve his next [Tailed Beast Bomb]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand, Rand, Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self $ tag 1 ]
        }
      ]
     , [ invuln "Chakra Skin" "Naruto" [Chakra]
       , invuln "Hide" "Naruto" [Mental]
       , invuln "Block" "Kurama" [Physical]
       ]
    ]
  ]
