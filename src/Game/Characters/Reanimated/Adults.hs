{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated.Adults (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Category -> Character]
characters =
  [ Character
    "Pakura"
    "Reanimated by Kabuto, Pakura was a jōnin diplomat known as the Hero of the Hidden Sand until she was betrayed and ambushed by the Hidden Mist Village. Her unique scorch style combines fire and wind elements to create heat orbs that mummify her enemies."
    [ [ Skill.new
        { Skill.name      = "Scorch Style"
        , Skill.desc      = "Miniature suns orbit Pakura and attack an enemy, dealing 20 affliction damage. For 2 turns, enemies who use skills on Pakura will take 10 affliction damage."
        , Skill.classes   = [Chakra, Ranged, Bane]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ afflict 20
          , To Self do
                trapFrom 2 (OnHarmed All) $ afflict 10
                tag 2
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Searing Combat"
        , Skill.desc      = "Pakura ignites an enemy, dealing 20 affliction damage to them for 2 turns. Cannot be used on an enemy already affected by this skill."
        , Skill.require   = HasU 0 "Searing Combat"
        , Skill.classes   = [Physical, Melee, Bane]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy $ apply 2 [Afflict 20] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Steam Kill"
        , Skill.desc      = "One of Pakura's orbs contacts an enemy and evaporates the water inside their body, mummifying them and dealing 40 affliction damage. Requires [Scorch Style]."
        , Skill.require   = HasI 1 "Scorch Style"
        , Skill.classes   = [Chakra, Melee, Bane]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Enemy $ afflict 40 ]
        }
      ]
    , [ invuln "Block" "Pakura" [Physical] ]
    ]
    75
  , Character
    "Gari"
    "Reanimated by Kabuto, Gari was a jōnin member of the Hidden Stone Village's Demolitions Unit. Augmented with explosive force, his taijutsu attacks overwhelm enemies who meet them head-on."
    [ [ Skill.new
        { Skill.name      = "Exploding Palm"
        , Skill.desc      = "Gari strikes an enemy and sets off an explosion at the moment of contact. The next time they use a skill on Gari or his allies, they will take 20 piercing damage. This skill stacks."
        , Skill.classes   = [Bypassing, Physical, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                addStack
                trap 0 OnHarm do
                    stacks <- targetStacks "Exploding Palm"
                    pierce (20 * stacks)
                    removeTrap "Exploding Palm"
                    remove "Exploding Palm"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ground Pound"
        , Skill.desc      = "An explosive shockwave destabilizes the ground around Gari. If an enemy uses a skill on Gari next turn, they will take 25 damage, and Gari will gain 2 turns of 25% damage reduction and restore 15 health each turn."
        , Skill.classes   = [Chakra, Melee, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ trapFrom 1 (OnHarmed All) do
                damage 25
                self $ apply 2 [Reduce All Percent 25, Heal 15]
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
    75
  , Character
    "Ginkaku"
    "The word \"Silver\" tattooed on his shoulder marks Ginkaku as one half of the Silver and Gold Brothers, the most notorious criminals in the history of the Hidden Cloud Village. Ginkaku and Kinkaku use tailed-beast chakra to wield the revered arsenal of their ancestor, the Sage of the Six Paths. When they fight together, they make a terrifying pair."
    [ [ Skill.new
        { Skill.name      = "Seven Stars Blade"
        , Skill.desc      = "Using a legendary sword that records a person's most frequently used word, Ginkaku slashes at an enemy's soul, dealing 25 piercing damage and extracting a Spirit Word from them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                pierce 25
                addStack' "Spirit Word"
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
                apply 1 [Stun Physical, Stun Melee]
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Benihisago"
        , Skill.desc      = "Ginkaku draws an enemy's soul into a crimson gourd, dealing 10 affliction damage and 5 additional damage for each of the target's Spirit Words. This also extracts a Spirit Word from the target and increases the damage of Kinkaku's [Scroll of Fire] on the target by 5."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- target $ numAnyStacks "Spirit Word"
                afflict (10 + 5 * stacks)
                addStack' "Scroll of Fire"
          ]
        }
      ]
    , [ invuln "Parry" "Ginkaku" [Physical] ]
    ]
    75
  , Character
    "Kinkaku"
    "The word \"Gold\" tattooed on his shoulder marks Kinkaku as one half of the Silver and Gold Brothers, the most notorious criminals in the history of the Hidden Cloud Village. Ginkaku and Kinkaku use tailed-beast chakra to wield the revered arsenal of their ancestor, the Sage of the Six Paths. When they fight together, they make a terrifying pair."
    [ [ Skill.new
        { Skill.name      = "Leaf Fan"
        , Skill.desc      = "Using a legendary fan that can generate any of the five elements, Kinkaku deals 25 affliction damage to an enemy, extracts a Spirit Word from them, and gains 50% damage reduction for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                afflict 25
                addStack' "Spirit Word"
          , To Self $ apply 1 [Reduce All Percent 50]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gold Rope"
        , Skill.desc      = "Kinkaku binds an enemy with the Sage of the Six Path's soul-stealing rope. The next time they use a skill on Kinkaku or his allies, they will take 35 damage and a Spirit Word will be extracted from them."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 0 OnHarm do
                damage 35
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Scroll of Fire"
        , Skill.desc      = "A coil of flame erupts from the Bashōsen Leaf Fan, dealing 20 damage to all enemies and extracting a Spirit Word from each of them."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                stacks <- target $ numAnyStacks "Scroll of Fire"
                damage (20 + 5 * stacks)
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ invuln "Parry" "Kinkaku" [Physical] ]
    ]
    75
  ]
