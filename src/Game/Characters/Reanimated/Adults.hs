{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated.Adults (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Category -> Character]
characters =
  [ Character
    "Pakura"
    "Known as the Hero of the Hidden Sand, Pakura was a successful diplomat until she was betrayed and ambushed by the Hidden Mist Village. Her unique scorch style combines fire and wind elements to create heat orbs that mummify her enemies."
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
  ]
