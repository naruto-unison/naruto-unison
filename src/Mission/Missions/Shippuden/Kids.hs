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
      HookAction "Naruto Uzumaki" "Naruto Uzumaki Barrage" $
      killDuring "Shadow Clones"

    , Reach 3 Match
      "In a single match, stun all 3 enemies with [Rasengan]." .
      HookStore "Naruto Uzumaki" "Rasengan" $
      checkUnique \user target ->
          not (allied user target)
          && hasFrom user "Rasengan" target
          && stunned target

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
      HookAction "Sakura Haruno" "KO Punch" $
      check \user target target' ->
          not (allied user target)
          && health target > health target'
          && hasFrom user "KO Punch" target

    , Reach 10 Career
      "Heal 10 allies under 30 health with [Mystical Palm Healing]." .
      HookAction "Sakura Haruno" "Mystical Palm Healing" $
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
      HookAction "Sasuke Uchiha" "Chidori" $
      killAffected "Sharingan"

    , Reach 3 Match
      "In a single match, apply [Sharingan] to all 3 enemies." .
      HookStore "Sasuke Uchiha" "Sharingan" $
      checkUnique \user target ->
          alive target
          && not (allied user target)
          && hasFrom user "Sharingan" target

    , Reach 1 Career
      "Use all 4 skills in 4 consecutive turns." $
      Consecutive "Sasuke Uchiha"
      ["Lions Barrage", "Chidori", "Sharingan", "Block"]
    ]
  , Mission
    "Kiba Inuzuka (S)"
    [ Reach 5 Career
      "Win 5 matches with Kiba Inuzuka and Kurenai Yuhi together." $
      Win ["Kiba Inuzuka", "Kurenai Yuhi"]

    , Reach 10 Career
      "Kill 10 enemies affected by [Dynamic Marking] with [Wolf Fang]." .
      HookAction "Kiba Inuzuka" "Wolf Fang" $
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

    , Reach 2 Moment
      "Use [Chakra Leech] to damage an enemy with at least 2 stacks of [Parasite]." .
      HookAction "Shino Aburame" "Chakra Leech" $
      damageWithStacks "Parasite"

    , Reach 3 Turn
      "Cause all 3 enemies to be affected by [Parasite] simultaneously." .
      HookTurn "Shino Aburame" $
      checkEnemyStatus "Parasite"

    , Reach 400 Career
      "Provide 400 destructible defense with [Wall of Insects]." .
      HookAction "Shino Aburame" "Wall of Insects" $
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
      HookAction "Hinata Hyūga" "Eight Trigrams Sixty-Four Palms" $
      defend "Eight Trigrams Sixty-Four Palms"

    , Reach 8 Match
      "In a single match, deplete 8 chakra with [Gentle Fist]." $
      HookChakra "Hinata Hyūga" "Gentle Fist"
      deplete
    ]

  , Mission
    "Shikamaru Nara (S)"
    [ Reach 5 Career
      "Win 5 matches with Shikamaru Nara and Asuma Sarutobi together." $
      Win ["Shikamaru Nara", "Asuma Sarutobi"]

    , Reach 3 Turn
      "Cause all 3 enemies to be affected by [Meditate] simultaneously." .
      HookTurn "Shikamaru Nara" $
      checkEnemyStatus "Meditate"

    , Reach 20 Career
      "With any team member, kill 20 enemies affected by [Shadow Strangle]." .
      HookTurn "Shikamaru Nara" $
      killWith "Shadow Strangle"

    , Reach 1 Career
      "Interrupt an Action or Control skill with [Shadow Possession]." $
      HookAction "Shikamaru Nara" "Shadow Possession"
      interrupt
    ]

  , Mission
    "Chōji Akimichi (S)"
    [ Reach 5 Career
      "Win 5 matches with Chōji Akimichi and Asuma Sarutobi together." $
      Win ["Chōji Akimichi", "Asuma Sarutobi"]

    , Reach 1 Career
      "Use [Spinach Pill], [Curry Pill], and [Chili Pill] in 3 consecutive turns." $
      Consecutive "Chōji Akimichi" ["Spinach Pill", "Curry Pill", "Chili Pill"]

    , Reach 90 Match
      "In a single match, restore 90 health with [Chakra Wings]." $
      HookAction "Chōji Akimichi" "Chakra Wings"
      heal

    , Reach 10 Career
      "Kill 10 enemies with the instant-kill effect of [Butterfly Bombing]." $
      HookAction "Chōji Akimichi" "Butterfly Bombing"
      execute
    ]

  , Mission
    "Ino Yamanaka (S)"
    [ Reach 5 Career
      "Win 5 matches with Ino Yamanaka and Asuma Sarutobi together." $
      Win ["Ino Yamanaka", "Asuma Sarutobi"]

    , Reach 10 Career
      "Kill 10 enemies with [Mind Destruction]." $
      HookAction "Ino Yamanaka" "Mind Destruction"
      kill

    , Reach 4 Match
      "Use [Mind Transfer] for 4 turns without being interrupted." .
      HookTurn "Ino Yamanaka" $
      maintain "Mind Transfer"

    , Reach 3 Match
      "In a single match, cause all 3 enemies to activate [Chakra Hair Trap]." $
      HookTrap "Ino Yamanaka" "Chakra Hair Trap"
      trapUnique
    ]

  , Mission
    "Rock Lee (S)"
    [ Reach 10 Career
      "Win 10 matches with Rock Lee and Might Guy together." $
      Win ["Rock Lee", "Might Guy"]

    , Reach 3 Turn
      "In a single turn, damage all 3 enemies with [Ferocious Fist]." $
      HookStore "Rock Lee" "Final Lotus"
      damageUnique

    , Reach 10 Career
      "Kill 10 enemies with [Primary Lotus] during [Fifth Gate Opening]." .
      HookAction "Rock Lee" "Primary Lotus" $
      killDuring "Fifth Gate Opening"

    , Reach 3 Match
      "In a single match, kill all 3 enemies with [Final Lotus]." $
      HookStore "Rock Lee" "Final Lotus"
      killUnique
    ]

  , Mission
    "Tenten (S)"
    [ Reach 10 Career
      "Win 10 matches with Tenten and Might Guy together." $
      Win ["Tenten", "Might Guy"]

    , Reach 5 Moment
      "Use [Rising Dragon Control] with at least 5 stacks of [Unsealing Technique]." .
      HookAction "Tenten" "Rising Dragon Control" $
      useDuringStacks "Unsealing Technique"

    , Reach 10 Career
      "Empower [Unsealing Technique] with [Rising Twin Dragons] 10 times." .
      HookAction "Tenten" "Unsealing Technique" $
      useDuring "Rising Twin Dragons"

    , Reach 10 Career
      "Empower [Rising Dragon Control] with [Rising Twin Dragons] 10 times." .
      HookAction "Tenten" "Rising Dragon Control" $
      useDuring "Rising Twin Dragons"
    ]

  , Mission
    "Neji Hyūga (S)"
    [ Reach 10 Career
      "Win 10 matches with Neji Hyūga and Might Guy together." $
      Win ["Neji Hyūga", "Might Guy"]

    , Reach 6 Match
      "Maintain [Gentle Fist] for 6 consecutive turns." .
      HookTurn "Neji Hyūga" $
      maintain "Gentle Fist"

    , Reach 10 Career
      "Kill 10 enemies with [Eight Trigrams Sixty-Four Palms]." $
      HookAction "Neji Hyūga" "Eight Trigrams Sixty-Four Palms"
      kill

    , Reach 4 Match
      "Use all 4 skills in 4 consecutive turns." $
      Consecutive "Neji Hyūga"
      [ "Gentle Fist"
      , "Eight Trigrams Palm Rotation"
      , "Eight Trigrams Sixty-Four Palms"
      , "Byakugan Foresight"
      ]
    ]
  , Mission
    "Kazekage Gaara (S)"
    [ Reach 10 Career
      "Win 10 matches with Gaara and Baki together." $
      Win ["Gaara", "Baki"]

    , Reach 10 Career
      "Kill 10 enemies with [Sand Burial]." $
      HookAction "Gaara" "Sand Burial"
      kill

    , Reach 8 Match
      "Maintain [Sand Clone] for 8 turns." .
      HookTurn "Gaara" $
      maintain "Sand Clone"

    , Reach 160 Match
      "In a single match, apply 160 destructible defense with [Sand Armor]." .
      HookAction "Gaara" "Sand Armor" $
      defend "Sand Armor"
    ]

  , Mission
    "Kankurō (S)"
    [ Reach 10 Career
      "Win 10 matches with Kankurō and Baki together." $
      Win ["Kankurō", "Baki"]

    , Reach 10 Career
      "Kill 10 enemies with [Iron Maiden]." $
      HookAction "Kankurō" "Iron Maiden"
      kill

    , Reach 2 Moment
      "Use [Iron Maiden] with at least 2 stacks of [Puppet Technique] to damage an enemy." .
      HookAction "Kankurō" "Iron Maiden" $
      damageDuringStacks "Puppet Technique"

    , Reach 2 Moment
      "Use [Poison Bomb] with at least 2 stacks of [Puppet Technique] to damage an enemy." .
      HookAction "Kankurō" "Poison Bomb" $
      damageDuringStacks "Puppet Technique"
    ]

  , Mission
    "Temari (S)"
    [ Reach 10 Career
      "Win 10 matches with Temari and Baki together." $
      Win ["Temari", "Baki"]

    , Reach 10 Career
      "Kill 10 enemies with [Cyclone Scythe]." $
      HookAction "Temari" "Cyclone Scythe"
      kill

    , Reach 2 Turn
      "In a single turn, kill 2 enemies with [Summoning: Blade Dance]." $
      HookAction "Temari" "Summoning: Blade Dance"
      kill

    , Reach 4 Match
      "Use all 4 skills in 4 consecutive turns." $
      Consecutive "Temari"
      ["Cyclone Scythe", "Summoning: Blade Dance", "Sandstorm", "Block"]
    ]

  , Mission
    "Konohamaru Sarutobi (S)"
    [ Reach 10 Career
      "Win 10 matches with Konohamaru Sarutobi and Naruto Uzumaki together." $
      Win ["Konohamaru Sarutobi", "Naruto Uzumaki"]

    , Reach 20 Career
      "With any team member, kill 20 enemies affected by [Unsexy Technique]." .
      HookTurn "Konohamaru Sarutobi" $
      killWith "Unsexy Technique"

    , Reach 6 Match
      "Maintain [Throw a Fit] for 6 turns." .
      HookTurn "Konohamaru Sarutobi" $
      maintain "Throw a Fit"

    , Reach 40 Turn
      "In a single turn, deal at least 40 damage with [Throw a Shuriken]." $
      HookAction "Konohamaru Sarutobi" "Throw a Shuriken"
      damage
    ]

  , Mission
    "Kabuto Yakushi (S)"
    [ Reach 10 Career
      "Win 10 matches with Yoroi Akadō and Misumi Tsurugi together." $
      Win ["Yoroi Akadō", "Misumi Tsurugi"]

    , Reach 20 Career
      "With any team member, kill 20 enemies affected by [Chakra Scalpel]." .
      HookTurn "Kabuto Yakushi" $
      killWith "Chakra Scalpel"

    , Reach 3 Career
      "Cure 3 status effects with [Pre-Healing Technique]." $
      HookAction "Kabuto Yakushi" "Pre-Healing Technique"
      cure

    , Reach 3 Match
      "In a single match, stun all 3 enemies with [Temple of Nirvana]." .
      HookStore "Kabuto Yakushi" "Temple of Nirvana" $
      checkUnique \user target ->
          not (allied user target)
          && hasFrom user "Temple of Nirvana" target
          && stunned target
    ]
  ]
