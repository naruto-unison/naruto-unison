{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Mission.Missions.Shippuden.Versions (missions) where

import Mission.Missions.Base

missions :: [Mission]
missions =
  [ Mission
    "Nine-Tailed Naruto (S)"
    [ winConsecutive 5 ["Naruto Uzumaki (S)"]

    , Reach 3 Match
      "In a single match, stun all 3 enemies with [Tailed Beast Rasengan]." $
      HookStore "One-Tailed Naruto" "Tailed Beast Rasengan"
      stunUnique

    , Reach 3 Match
      "In a single match, kill all 3 enemies with [Tailed Beast Rasengan] during [Tailed Beast Chakra Arms]." .
      HookStore "One-Tailed Naruto" "Tailed Beast Rasengan" $
      killUniqueDuring "Tailed Beast Chakra Arms"

    , Reach 3 Match
      "In a single match, kill all 3 enemies with [Tailed Beast Rasengan] during [Inner Chakra Mode]." .
      HookStore "One-Tailed Naruto" "Tailed Beast Rasengan" $
      killUniqueDuring "Inner Chakra Mode"
    ]

  , Mission
    "Mangekyō Sasuke (S)"
    [ winConsecutive 5 ["Sasuke Uchiha (S)"]

    , Reach 3 Match
      "In a single match, affect all 3 enemies with [Chidori]." $
      HookStore "Curse Mark Sasuke" "Chidori"
      affectUniqueEnemy

    , Reach 3 Match
      "In a single match, kill all 3 enemies with [Dark Void]." $
      HookStore "Curse Mark Sasuke" "Dark Void"
      killUnique

    , Reach 4 Match
      "In a single match, use [Curse Mark] 4 times." $
      HookAction "Curse Mark Sasuke" "Curse Mark"
      use
    ]

    {-
  , Mission
    "Commander Gaara"
    [ winConsecutive 5 ["Kazekage Gaara (S)"]

    , Reach 3 Turn
      "In a single turn, damage all 3 enemies with [Giant Sand Burial]." $
      HookStore "Rehabilitated Gaara" "Giant Sand Burial"
      damageUnique
    ]
    -}
  ]
