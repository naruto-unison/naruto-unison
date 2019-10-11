{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated.Adults (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Category -> Character]
characters =
  [ Character
    "Pakura"
    "Reanimated by Kabuto, Pakura was a successful diplomat known as the Hero of the Hidden Sand until she was betrayed and ambushed by the Hidden Mist Village. Her unique scorch style combines fire and wind elements to create heat orbs that mummify her enemies."
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
    "Reanimated by Kabuto, Gari was a member of the Hidden Stone Village's Demolitions Unit. Augmented with explosive force, his taijutsu attacks overwhelm enemies who meet them head-on."
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
  ]
