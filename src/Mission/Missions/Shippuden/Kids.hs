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

    , Reach 1 Match
      "In a single match, stun all 3 enemies with [Rasengan]." .
      Hook "Naruto Uzumaki" "Rasengan" $
      affectAll \user target ->
          not (allied user target)
          && not (stunned target)
          && stunned target
          && hasFrom user "Rasengan" target

    , Reach 1 Match
      "Maintain [Shadow Clones] for 8 consecutive turns." .
      HookTurn "Naruto Uzumaki" $
      maintain 8 "Shadow Clones"
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

    , Reach 1 Career
      "Maintain [Inner Sakura] for 8 turns." .
      HookTurn "Sakura Haruno" $
      maintain 8 "Inner Sakura"
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

    , Reach 1 Match
      "In a single match, apply [Sharingan] to all 3 enemies." .
      Hook "Sasuke Uchiha" "Sharingan" $
      affectAll \user target ->
          not (allied user target)
          && hasFrom user "Sharingan" target

    , Reach 1 Career
      "Use all 4 skills in 4 consecutive turns." $
      UseAllSkills "Sasuke Uchiha"
    ]
  ]
