{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Mission.Missions.Shippuden.Adults (missions) where

import Mission.Missions.Base

missions :: [Mission]
missions =
  [ Mission
    "Kakashi Hatake (S)"
    [ win 5 ["Naruto Uzumaki", "Sakura Haruno", "Sasuke Uchiha"]

    , Reach 5 Match
      "In a single match, reflect 5 skills with [Sharingan]." $
      HookTrigger "Kakashi Hatake" OnReflect $
      hasOwn "Sharingan"

    , Reach 3 Match
      "In a single match, stun all 3 enemies with [Summoning: Ninja Hounds]." $
      HookStore "Kakashi Hatake" "Summoning: Ninja Hounds"
      stunUnique

    , Reach 10 Career
      "Kill 10 enemies with the instant-kill effect of [Lightning Blade]." $
      HookAction "Kakashi Hatake" "Lightning Blade"
      execute
    ]

  , Mission
    "Asuma Sarutobi (S)"
    [ win 5 ["Ino Yamanaka", "Shikamaru Nara", "Chōji Akimichi"]

    , Reach 6 Moment
      "Use [Flying Swallow] with at least 6 stacks of [Sharpen Blades]." .
      HookAction "Asuma Sarutobi" "Flying Swallow" $
      useDuringStacks "Sharpen Blades"

    , Reach 10 Career
      "Kill 10 enemies with [Flying Kick]." $
      HookAction "Asuma Sarutobi" "Flying Kick"
      kill

    , Reach 6 Match
      "Maintain [Self-Sacrifice] on an ally for 6 turns." .
      HookTurn "Asuma Sarutobi" $
      maintainOnAlly "Self-Sacrifice"
    ]

  , Mission
    "Might Guy (S)"
    [ win 5 ["Rock Lee (S)", "Tenten (S)", "Neji Hyūga (S)"]

    , Reach 10 Career
      "Kill 10 enemies with [Leaf Hurricane] during [Sixth Gate Opening]." .
      HookAction "Might Guy" "Leaf Hurricane" $
      killDuring "Sixth Gate Opening"

    , Reach 1 Moment
      "Use [Sixth Gate Opening] with less than 40 health." .
      HookAction "Might Guy" "Sixth Gate Opening" $
      check \user _ _ -> health user < 40

    , Reach 3 Match
      "In a single match, counter all 3 enemies with [Counter Punch]." $
      HookTrap "Might Guy" "Counter Punch"
      trapUniqueEnemy
    ]
  ]
