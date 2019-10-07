{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Mission.Missions.Shippuden.Kids (missions) where

import Mission.Missions.Base

missions :: [Mission]
missions =
  [ Mission
    "Naruto Uzumaki (S)"
    [ Reach 5 Career
      "Win 5 matches with Naruto Uzumaki and Jiraiya together." $
      Win ["Naruto Uzumaki", "Jiraiya"]

    , Reach 10 Career
      "Kill 10 enemies with [Naruto Uzumaki Barrage] during [Shadow Clones]." .
      Hook "Naruto Uzumaki" "Naruto Uzumaki Barrage" $
      killDuring "Shadow Clones"

    , Reach 3 Match
      "In a single match, stun all 3 enemies with [Rasengan]." .
      Hook "Naruto Uzumaki" "Rasengan" $
      checkUnique \user target ->
          not (allied user target)
          && hasFrom user "Rasengan" target

    , Reach 8 Match
      "Maintain [Shadow Clones] for 8 consecutive turns." .
      HookTurn "Naruto Uzumaki" $
      maintain "Shadow Clones"
    ]
  , Mission
    "Sakura Haruno (S)"
    [ Reach 5 Career
      "Win 5 matches with Sakura Haruno and Tsunade together." $
      Win ["Sakura Haruno", "Tsunade"]

    , Reach 1 Career
      "Use [KO Punch] to damage an enemy affected by [KO Punch]." .
      Hook "Sakura Haruno" "KO Punch" $
      check \user target target' ->
          not (allied user target)
          && health target > health target'
          && hasFrom user "KO Punch" target

    , Reach 10 Career
      "Heal 10 allies under 30 health with [Mystical Palm Healing]." .
      Hook "Sakura Haruno" "Mystical Palm Healing" $
      check \user target target' ->
          allied user target
          && health target < 30
          && health target' >= 30

    , Reach 8 Match
      "Maintain [Inner Sakura] for 8 turns." .
      HookTurn "Sakura Haruno" $
      maintain "Inner Sakura"
    ]
  , Mission
    "Sasuke Uchiha (S)"
    [ Reach 5 Career
      "Win 5 matches with Sasuke Uchiha and Orochimaru together." $
      Win ["Sasuke Uchiha", "Orochimaru"]

    , Reach 10 Career
      "Kill 10 enemies affected by [Sharingan] with [Chidori]." .
      Hook "Sasuke Uchiha" "Chidori" $
      killAffected "Sharingan"

    , Reach 3 Match
      "In a single match, apply [Sharingan] to all 3 enemies." .
      Hook "Sasuke Uchiha" "Sharingan" $
      checkUnique \user target ->
          not (allied user target)
          && hasFrom user "Sharingan" target

    , Reach 1 Match
      "Use all 4 skills in 4 consecutive turns." $
      UseAllSkills "Sasuke Uchiha"
    ]
  , Mission
    "Kiba Inuzuka (S)"
    [ Reach 5 Career
      "Win 5 matches with Kiba Inuzuka and Kurenai Yuhi together." $
      Win ["Kiba Inuzuka", "Kurenai Yuhi"]

    , Reach 10 Career
      "Kill 10 enemies affected by [Dynamic Marking] with [Wolf Fang]." .
      Hook "Kiba Inuzuka" "Wolf Fang" $
      killAffected "Dynamic Marking"

    , Reach 6 Match
      "Maintain [Two-Headed Wolf] for 6 consecutive turns." .
      HookTurn "Kiba Inuzuka" $
      maintain "Two-Headed Wolf"

    , Reach 3 Turn
      "Cause all 3 enemies to be affected by [Dynamic Marking] simultaneously." .
      HookTurn "Kiba Inuzuka" $
      checkEnemyStatus "Dynamic Marking"
    ]

  , Mission
    "Shino Aburame (S)"
    [ Reach 5 Career
      "Win 5 matches with Shino Aburame and Kurenai Yuhi together." $
      Win ["Shino Aburame", "Kurenai Yuhi"]

    , Reach 1 Career
      "Use [Chakra Leech] to damage an enemy with at least 2 stacks of [Parasite]." .
      Hook "Shino Aburame" "Chakra Leech" $
      damageWith 2 "Parasite"

    , Reach 3 Turn
      "Cause all 3 enemies to be affected by [Parasite] simultaneously." .
      HookTurn "Shino Aburame" $
      checkEnemyStatus "Parasite"

    , Reach 400 Career
      "Provide 400 destructible defense with [Wall of Insects]." .
      Hook "Shino Aburame" "Wall of Insects" $
      defend "Wall of Insects"
    ]

  , Mission
    "Hinata Hyūga (S)"
    [ Reach 5 Career
      "Win 5 matches with Hinata Hyūga and Kurenai Yuhi together." $
      Win ["Hinata Hyūga", "Kurenai Yuhi"]

    , Reach 6 Match
      "Maintain [Gentle Fist] for 6 consecutive turns." .
      HookTurn "Hinata Hyūga" $
      maintain "Gentle Fist"

    , Reach 100 Career
      "Provide 100 destructible defense with [Eight Trigrams Sixty-Four Palms]." .
      Hook "Hinata Hyūga" "Eight Trigrams Sixty-Four Palms" $
      defend "Eight Trigrams Sixty-Four Palms"
    ]
  ]
