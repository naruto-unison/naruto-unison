{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Mission.Missions.Shippuden.Adults (missions) where

import Mission.Missions.Import

missions :: [Mission]
missions =
  [ Mission
    "Kakashi Hatake (S)"
    [ win 5 ["Naruto Uzumaki (S)", "Sakura Haruno (S)", "Sasuke Uchiha (S)"]

    , Reach Match 5
      "In a single match, reflect 5 skills with [Sharingan]." $
      HookTrigger "Kakashi Hatake" OnReflect $
      hasOwn "Sharingan"

    , Reach Match 3
      "In a single match, stun all 3 enemies with [Summoning: Ninja Hounds]." $
      HookStore "Kakashi Hatake" "Summoning: Ninja Hounds"
      stunUnique

    , Reach Career 10
      "Kill 10 enemies with the instant-kill effect of [Lightning Blade]." $
      HookTrigger "Kakashi Hatake" OnExecute $
      used "Lightning Blade"
    ]

  , Mission
    "Asuma Sarutobi (S)"
    [ win 5 ["Ino Yamanaka (S)", "Shikamaru Nara (S)", "Chōji Akimichi (S)"]

    , Reach Moment 5
      "Use [Flying Swallow] with at least 6 stacks of [Sharpen Blades]." .
      HookAction "Asuma Sarutobi" "Flying Swallow" $
      useDuringStacks "Sharpen Blades"

    , Reach Career 10
      "Kill 10 enemies with [Flying Kick]." $
      HookAction "Asuma Sarutobi" "Flying Kick"
      kill

    , Reach Match 6
      "Maintain [Self-Sacrifice] on an ally for 6 turns." .
      HookTurn "Asuma Sarutobi" $
      maintainOnAlly "Self-Sacrifice"
    ]

  , Mission
    "Might Guy (S)"
    [ win 5 ["Rock Lee (S)", "Tenten (S)", "Neji Hyūga (S)"]

    , Reach Career 10
      "Kill 10 enemies with [Leaf Hurricane] during [Sixth Gate Opening]." .
      HookAction "Might Guy" "Leaf Hurricane" $
      killDuring "Sixth Gate Opening"

    , Reach Moment 1
      "Use [Sixth Gate Opening] with less than 40 health." .
      HookAction "Might Guy" "Sixth Gate Opening" $
      check \user _ _ -> user.health < 40

    , Reach Match 3
      "In a single match, counter all 3 enemies with [Counter Punch]." $
      HookTrap "Might Guy" "Counter Punch"
      trapUniqueEnemy
    ]
  ]
