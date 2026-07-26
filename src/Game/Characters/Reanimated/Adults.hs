{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.Adults (characters, reanimations) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Category -> Text -> Character]
characters =
  [ Character
    "Pakura" 75
    "Reanimated by Kabuto, Pakura was a jōnin diplomat known as the Hero of the Hidden Sand until she was betrayed and ambushed by the Hidden Mist Village. Her unique scorch style combines fire and wind elements to create heat orbs that mummify her enemies."
    [SandVillage, Kabuto, Jonin, Fire, Wind]
    [ [ Skill.new
        { Skill.name      = "Scorch Style"
        , Skill.desc      = "Miniature suns orbit Pakura and attack an enemy, dealing 20 affliction damage. For 2 turns, enemies who use skills on Pakura will take 10 affliction damage."
        , Skill.classes   = [Chakra, Ranged, Bane]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ afflict 20
          , To Self $ trapFrom 2 skillName (OnHarmed All) $
                afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Searing Combat"
        , Skill.desc      = "Pakura ignites an enemy, dealing 20 affliction damage to them for 2 turns. Cannot be used on an enemy already affected by this skill."
        , Skill.require   = [TargetHas AtMost 0 skillName]
        , Skill.classes   = [Physical, Melee, Bane]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy $ apply 2 skillName [Afflict 20] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Steam Kill"
        , Skill.desc      = "One of Pakura's orbs contacts an enemy and evaporates the water inside their body, mummifying them and dealing 40 affliction damage. Requires [Scorch Style]."
        , Skill.require   = [UserTrap True "Scorch Style"]
        , Skill.classes   = [Chakra, Melee, Bane]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Enemy $ afflict 40 ]
        }
      ]
    , [ invuln "Block" "Pakura" [Physical] ]
    ]
  , Character
    "Gari" 75
    "Reanimated by Kabuto, Gari was a jōnin member of the Hidden Stone Village's Demolitions Unit. Augmented with explosive force, his taijutsu attacks overwhelm enemies who meet them head-on."
    [StoneVillage, Kabuto, Jonin, Earth, Lightning]
    [ [ Skill.new
        { Skill.name      = "Exploding Palm"
        , Skill.desc      = "Gari strikes an enemy and sets off an explosion at the moment of contact. The next time they use a skill on Gari or his allies, they will take 20 piercing damage. This skill stacks."
        , Skill.classes   = [Bypassing, Physical, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                addStack skillName
                trap Permanent skillName OnHarm do
                    removeTrap skillName
                    asAction do
                        stacks <- target amount skillName
                        pierce (20 * stacks)
                    remove skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ground Pound"
        , Skill.desc      = "An explosive shockwave destabilizes the ground around Gari. If an enemy uses a skill on Gari next turn, they will take 25 damage, and Gari will gain 2 turns of 25% damage reduction and regain 15 health each turn."
        , Skill.classes   = [Chakra, Melee, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ trapFrom 1 skillName (OnHarmed All) do
                damage 25
                targeting Self $ apply 2 skillName
                    [ Reduce [All] Percent 25
                    , Heal 15
                    ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Landmine Fist"
        , Skill.desc      = "Making direct contact with an enemy, Gari generates an explosion inside them that deals 35 piercing damage."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 35 ]
        }
      ]
    , [ invuln "Dodge" "Gari" [Physical] ]
    ]
  , Character
    "Ginkaku" 75
    "The word \"Silver\" tattooed on his shoulder marks Ginkaku as one half of the Silver and Gold Brothers, the most notorious criminals in the history of the Hidden Cloud Village. Ginkaku and Kinkaku use tailed-beast chakra to wield the revered arsenal of their ancestor, the Sage of the Six Paths. When they fight together, they make a terrifying pair."
    [CloudVillage, Kabuto]
    [ [ Skill.new
        { Skill.name      = "Seven Stars Blade"
        , Skill.desc      = "Using a legendary sword that records a person's most frequently used word, Ginkaku slashes at an enemy's soul, dealing 25 piercing damage and extracting a Spirit Word from them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                pierce 25
                addStack "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amber Purification Jar"
        , Skill.desc      = "Ginkaku captures an enemy inside the Sage of the Sixth Path's sealing jar, stunning their physical and melee skills for 1 turn and extracting a Spirit Word from them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                apply 1 skillName
                    [ Stun Physical
                    , Stun Melee
                    ]
                addStack "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Benihisago"
        , Skill.desc      = "Ginkaku draws an enemy's soul into a crimson gourd, dealing 10 affliction damage and 5 additional damage for each of the target's Spirit Words from himself or Kinkaku. This also extracts a Spirit Word from the target and increases the damage of Kinkaku's [Scroll of Fire] on the target by 5."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- target amountFromAny "Spirit Word"
                afflict (10 + 5 * stacks)
                addStack "Scroll of Fire"
                addStack "Spirit Word"
          ]
        }
      ]
    , [ invuln "Parry" "Ginkaku" [Physical] ]
    ]
  , Character
    "Kinkaku" 75
    "The word \"Gold\" tattooed on his shoulder marks Kinkaku as one half of the Silver and Gold Brothers, the most notorious criminals in the history of the Hidden Cloud Village. Ginkaku and Kinkaku use tailed-beast chakra to wield the revered arsenal of their ancestor, the Sage of the Six Paths. When they fight together, they make a terrifying pair."
    [CloudVillage, Kabuto]
    [ [ Skill.new
        { Skill.name      = "Leaf Fan"
        , Skill.desc      = "Using a legendary fan that can generate any of the five elements, Kinkaku deals 25 affliction damage to an enemy, extracts a Spirit Word from them, and gains 50% damage reduction for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                afflict 25
                addStack "Spirit Word"
          , To Self $ apply 1 skillName [Reduce [All] Percent 50]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gold Rope"
        , Skill.desc      = "Kinkaku binds an enemy with the Sage of the Six Path's soul-stealing rope. The next time they use a skill on Kinkaku or his allies, they will take 35 damage and a Spirit Word will be extracted from them. This skill stacks."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                addStack skillName
                trap Permanent skillName OnHarm do
                    removeTrap skillName
                    stacks <- target amount skillName
                    asAction do
                        damage (35 * stacks)
                        addStacks "Spirit Word" stacks
                    remove skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Scroll of Fire"
        , Skill.desc      = "A coil of flame erupts from the Bashōsen Leaf Fan, dealing 20 damage to all enemies and extracting a Spirit Word from each of them."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                stacks <- target amountFromAny skillName
                damage (20 + 5 * stacks)
                addStack "Spirit Word"
          ]
        }
      ]
    , [ invuln "Parry" "Kinkaku" [Physical] ]
    ]
  , Character
    "Toroi" 75
    "Reanimated by Kabuto, Toroi was a jōnin from the Hidden Cloud Village with the rare ability to manipulate magnetic fields. His weapons are magnetized, and as their magnetic field accumulates on targets, they become harder and harder to avoid."
    [CloudVillage, Kabuto, Jonin, Wind, Earth]
    [ [ Skill.new
        { Skill.name      = "Demon Wind Shuriken"
        , Skill.desc      = "Toroi deals 20 damage to an enemy with a giant shuriken and defends himself with several others. For 2 turns, enemies who use skills on Toroi will become permanently unable to be healed or cured. Deals 5 additional damage for every time the enemy was affected by [Magnetic Current] or [Conserving Bee Twin Blades]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ trapFrom 2 skillName (OnHarmed All) $
                apply Permanent skillName [Plague]
          , To Enemy do
                bonusA <- 5 `bonusPer` target amount "Conserving Bee Twin Blades"
                bonusB <- 5 `bonusPer` target amount "Magnetic Current"
                damage (20 + bonusA + bonusB)
                addStack skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Magnetic Field"
        , Skill.desc      = "Toroi generates a field of magnetism around himself that provides 50% damage reduction for 3 turns. While active, this skill becomes [Conserving Bee Twin Blades]."
        , Skill.classes   = [Physical]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ apply 3 skillName
                [ Reduce [All] Percent 50
                , Alternate "Magnetic Field"
                            "Conserving Bee Twin Blades"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Conserving Bee Twin Blades"
        , Skill.desc      = "Toroi hurls a magnetized shuriken at an enemy, dealing 10 piercing damage. Deals 5 additional piercing damage for each time the target was affected by [Demon Wind Shuriken] or [Magnetic Current]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.effects   =
          [ To Enemy do
                bonusA <- 5 `bonusPer` target amount "Demon Wind Shuriken"
                bonusB <- 5 `bonusPer` target amount "Magnetic Current"
                pierce (10 + bonusA + bonusB)
                addStack skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Magnetic Current"
        , Skill.desc      = "Toroi energizes the field with magnetism, dealing 10 piercing damage to all enemies. Deals 5 additional piercing damage for each time the target was affected by [Demon Wind Shuriken] or [Conserving Bee Twin Blades]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                bonusA <- 5 `bonusPer` target amount "Demon Wind Shuriken"
                bonusB <- 5 `bonusPer` target amount "Conserving Bee Twin Blades"
                pierce (10 + bonusA + bonusB)
                addStack skillName
          ]
        }
      ]
    , [ invuln "Shuriken Threads" "Toroi" [Physical] ]
    ]
  , Character
    "Fukai" 75
    "Reanimated by Kabuto, Fukai was the previous jinchūriki of Gyūki, the eight-tailed beast. Also known as Blue B, Fukai was unable to control Gyūki, which led to his downfall and the deaths of his comrades."
    [CloudVillage, Kabuto, Jinchuriki, Sensor, Lightning]
    [ [ Skill.new
        { Skill.name      = "Chakra Arms"
        , Skill.desc      = "Manifesting limbs of tailed-beast chakra, Fukai deals 10 affliction damage to all enemies."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                afflict 10
                addStack skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Bomb Barrage"
        , Skill.desc      = "With a continuous attack of tailed-beast bombs, Fukai deals 30 damage to an enemy. All damage he receives—including piercing and affliction—is permanently reduced by 10%. Deals 5 additional damage for each time the target was affected by [Chakra Arms]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ apply Permanent skillName [Reduce [Affliction] Percent 10]
          , To Enemy do
                bonus <- 5 `bonusPer` target amount "Chakra Arms"
                damage (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lariat"
        , Skill.desc      = "Rushing an enemy, Fukai deals 15 piercing damage to them and stuns their chakra and ranged skills for 1 turn. Deals 5 additional damage for each time the target was affected by [Chakra Arms]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusPer` target amount "Chakra Arms"
                pierce (15 + bonus)
                apply 1 skillName
                    [ Stun Chakra
                    , Stun Ranged
                    ]
          ]
        }
      ]
    , [ invuln "Dodge" "Fukai" [Physical] ]
    ]
  , Character
    "Chiyo" 75
    "Reanimated by Kabuto, Chiyo was the leader of the Hidden Sand Village's Puppet Brigade. Her expertise with chakra threads allows her to control numerous puppets at once."
    [SandVillage, Kabuto]
    [ [ Skill.new
        { Skill.name      = "Ten Puppets Collection"
        , Skill.desc      = "Commanding a brigade of puppets, Chiyo stuns an enemy's physical and ranged skills for 1 turn and deals 20 damage to a random enemy for 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.start     =
          [ To Enemy $ apply 1 skillName
                [ Stun Physical
                , Stun Ranged
                ]
          ]
        , Skill.effects   =
          [ To REnemy $ damage 20 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Possum"
        , Skill.desc      = "Chiyo feigns unconsciousness to lure an enemy into a false sense of security. If the target uses a skill on Chiyo or her allies, they will be countered and take 20 damage, and their physical skills will be stunned for 1 turn."
        , Skill.classes   = [Physical, Melee, Invisible]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap Permanent skillName (Countered All) $ asAction do
                damage 20
                apply 1 skillName [Stun Physical]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Army of Illusions"
        , Skill.desc      = "Chiyo takes control of multiple bodies and attacks an enemy, dealing 20 damage and becoming invulnerable to melee skills for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self $ apply 1 skillName [Invulnerable Melee]
          ]
        }
      ]
    , [ invuln "Puppet Distraction" "Chiyo" [Physical] ]
    ]
  , Character
    "Chūkichi" 75
    "Reanimated by Kabuto, Chūkichi was a jōnin renowned as the Hidden Mist Village's most talented sensor. Due to his refusal to serve Akatsuki, Chūkichi had his personality completely erased by Kabuto, transforming him into a mere killing machine."
    [MistVillage, Kabuto, Jonin, Sensor, Water]
    [ [ Skill.new
        { Skill.name      = "Hidden Frost"
        , Skill.desc      = "A cloud of frost conceals Chūkichi, rendering him effectively invisible. For 2 turns, he is invulnerable to ranged skills."
        , Skill.classes   = [Chakra]
        , Skill.cooldown  = 3
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Self $ apply 2 skillName [Invulnerable Ranged]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Psychic Jamming"
        , Skill.desc      = "Chūkichi telepathically disrupts the minds of all enemies, increasing the costs of their skills by 1 arbitrary chakra for 1 turn. During [Hidden Frost], this skill costs 1 [r]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies $ apply 1 skillName [Exhaust [All]] ]
        , Skill.changes   = changeWith "Hidden Frost" $ setCost [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Silent Killing"
        , Skill.desc      = "Chūkichi ambushes an enemy, dealing 30 piercing damage and preventing them from reducing damage or becoming invulnerable for 1 turn. During [Hidden Frost], this skill deals 10 additional damage and costs [t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` user has "Hidden Frost"
                pierce (30 + bonus)
                apply 1 skillName [Expose]
          ]
        , Skill.changes   = changeWith "Hidden Frost" $ setCost [Tai]
        }
      ]
    , [ invuln "Hide" "Chūkichi" [Mental] ]
    ]
  ]

reanimations :: [Skill]
reanimations =
    [ Skill.new
        { Skill.name    = "Pakura: Searing Combat"
        , Skill.desc    = "Pakura ignites an enemy, dealing 20 affliction damage to them for 2 turns."
        , Skill.classes = [Physical, Melee, Bane]
        , Skill.effects =
          [ To Enemy $ apply 2 "Searing Combat" [Afflict 20] ]
        }
    , Skill.new
        { Skill.name   = "Gari: Landmine Fist"
        , Skill.desc   = "Making direct contact with an enemy, Gari generates an explosion inside them that deals 35 piercing damage."
        , Skill.classes = [Chakra, Melee]
        , Skill.effects =
          [ To Enemy $ pierce 35 ]
        }
    , Skill.new
        { Skill.name    = "Ginkaku: Amber Purification Jar"
        , Skill.desc    = "Ginkaku captures an enemy inside the Sage of the Sixth Path's sealing jar, stunning their physical and melee skills for 1 turn."
        , Skill.classes = [Physical, Melee]
        , Skill.effects =
          [ To Enemy $ apply 1 "Amber Purification Jar"
                [ Stun Physical
                , Stun Melee
                ]
          ]
        }
    , Skill.new
        { Skill.name    = "Kinkaku: Leaf Fan"
        , Skill.desc    = "Using a legendary fan that can generate any of the five elements, Kinkaku deals 25 affliction damage to an enemy and provides 50% damage reduction to his reanimator for 1 turn."
        , Skill.classes = [Physical, Ranged]
        , Skill.effects =
          [ To Enemy $ afflict 25
          , To Self $ apply 1 "Leaf Fan" [Reduce [All] Percent 50]
          ]
        }
    , Skill.new
        { Skill.name    = "Toroi: Magnetic Current"
        , Skill.desc    = "Toroi energizes the field with magnetism, dealing 10 piercing damage to all enemies."
        , Skill.classes = [Physical, Ranged]
        , Skill.effects =
          [ To Enemies $ pierce 10 ]
        }
    , Skill.new
        { Skill.name    = "Fukai: Lariat"
        , Skill.desc    = "Rushing an enemy, Fukai deals 15 piercing damage to them and stuns their chakra and ranged skills for 1 turn."
        , Skill.classes = [Mental, Ranged]
        , Skill.effects =
          [ To Enemy do
                pierce 15
                apply 1 "Lariat"
                    [ Stun Chakra
                    , Stun Ranged
                    ]
          ]
        }
    , Skill.new
        { Skill.name    = "Chiyo: Army of Illusions"
        , Skill.desc    = "Chiyo takes control of multiple bodies and attacks an enemy, dealing 20 damage and making her reanimator invulnerable to melee skills for 1 turn."
        , Skill.classes = [Physical, Ranged]
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self $ apply 1 "Army of Illusions" [Invulnerable Melee]
          ]
        }
    , Skill.new
        { Skill.name    = "Chūkichi: Psychic Jamming"
        , Skill.desc    = "Chūkichi telepathically disrupts the minds of all enemies, increasing the costs of their skills by 1 arbitrary chakra for 1 turn."
        , Skill.classes = [Mental, Ranged]
        , Skill.effects =
          [ To Enemies $ apply 1 "Psychic Jamming" [Exhaust [All]] ]
        }
    ]
