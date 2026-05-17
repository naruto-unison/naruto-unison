{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Mission.Missions.Shippuden.Kids (missions) where

import Mission.Missions.Import

missions :: [Mission]
missions =
  [ Mission
    "Naruto Uzumaki (S)"
    [ win 5 ["Naruto Uzumaki", "Jiraiya"]

    , Reach Career 10
      "Kill 10 enemies with [Naruto Uzumaki Barrage] during [Shadow Clones]." .
      HookAction "Naruto Uzumaki" "Naruto Uzumaki Barrage" $
      killDuring "Shadow Clones"

    , Reach Match 3
      "In a single match, stun all 3 enemies with [Rasengan]." $
      HookStore "Naruto Uzumaki" "Rasengan"
      stunUnique

    , Reach Match 8
      "Maintain [Shadow Clones] for 8 consecutive turns." .
      HookTurn "Naruto Uzumaki" $
      maintain "Shadow Clones"
    ]
  , Mission
    "Sakura Haruno (S)"
    [ win 5 ["Sakura Haruno", "Tsunade"]

    , Reach Career 1
      "Use [KO Punch] to damage an enemy affected by [KO Punch]." .
      HookAction "Sakura Haruno" "KO Punch" $
      damageWithStacks "KO Punch"

    , Reach Career 10
      "Heal 10 allies under 30 health with [Healing Technique]." .
      HookAction "Sakura Haruno" "Healing Technique" $
      check \user target target' ->
          allied user target
          && health target < 30
          && health target' >= 30

    , Reach Match 8
      "Maintain [Inner Sakura] for 8 consecutive turns." .
      HookTurn "Sakura Haruno" $
      maintain "Inner Sakura"
    ]
  , Mission
    "Sasuke Uchiha (S)"
    [ win 5 ["Sasuke Uchiha", "Orochimaru"]

    , Reach Career 10
      "Kill 10 enemies affected by [Sharingan] with [Chidori]." .
      HookAction "Sasuke Uchiha" "Chidori" $
      killAffected "Sharingan"

    , Reach Match 3
      "In a single match, apply [Sharingan] to all 3 enemies." $
      HookStore "Sasuke Uchiha" "Sharingan"
      affectUniqueEnemy

    , Reach Career 1
      "Use all 4 skills in 4 consecutive turns." $
      Consecutive "Sasuke Uchiha"
      ["Lions Barrage", "Chidori", "Sharingan", "Block"]
    ]
  , Mission
    "Kiba Inuzuka (S)"
    [ win 5 ["Kiba Inuzuka", "Kurenai Yuhi"]

    , Reach Career 10
      "Kill 10 enemies affected by [Dynamic Marking] with [Wolf Fang]." .
      HookAction "Kiba Inuzuka" "Wolf Fang" $
      killAffected "Dynamic Marking"

    , Reach Match 6
      "Maintain [Two-Headed Wolf] for 6 consecutive turns." .
      HookTurn "Kiba Inuzuka" $
      maintain "Two-Headed Wolf"

    , Reach Turn 3
      "Cause all 3 enemies to be affected by [Dynamic Marking] simultaneously." .
      HookTurn "Kiba Inuzuka" $
      checkEnemyStatus "Dynamic Marking"
    ]

  , Mission
    "Shino Aburame (S)"
    [ win 5 ["Shino Aburame", "Kurenai Yuhi"]

    , Reach Moment 2
      "Use [Chakra Leech] to damage an enemy with at least 2 stacks of [Parasite]." .
      HookAction "Shino Aburame" "Chakra Leech" $
      damageWithStacks "Parasite"

    , Reach Turn 3
      "Cause all 3 enemies to be affected by [Parasite] simultaneously." .
      HookTurn "Shino Aburame" $
      checkEnemyStatus "Parasite"

    , Reach Career 400
      "Provide 400 destructible defense with [Wall of Insects]." $
      HookAction "Shino Aburame" "Wall of Insects"
      defend
    ]

  , Mission
    "Hinata Hyūga (S)"
    [ win 5 ["Hinata Hyūga", "Kurenai Yuhi"]

    , Reach Match 6
      "Maintain [Gentle Fist] for 6 consecutive turns." .
      HookTurn "Hinata Hyūga" $
      maintain "Gentle Fist"

    , Reach Career 100
      "Provide 100 destructible defense with [Eight Trigrams Sixty-Four Palms]." $
      HookAction "Hinata Hyūga" "Eight Trigrams Sixty-Four Palms"
      defend

    , Reach Match 8
      "In a single match, deplete 8 chakra with [Gentle Fist]." $
      HookChakra "Hinata Hyūga" "Gentle Fist"
      deplete
    ]

  , Mission
    "Shikamaru Nara (S)"
    [ win 5 ["Shikamaru Nara", "Asuma Sarutobi"]

    , Reach Turn 3
      "Cause all 3 enemies to be affected by [Meditate] simultaneously." .
      HookTurn "Shikamaru Nara" $
      checkEnemyStatus "Meditate"

    , Reach Career 20
      "With any team member, kill 20 enemies affected by [Shadow Strangle]." .
      HookTurn "Shikamaru Nara" $
      killWith "Shadow Strangle"

    , Reach Career 1
      "Interrupt an Action or Control skill with [Shadow Possession]." $
      HookAction "Shikamaru Nara" "Shadow Possession"
      interrupt
    ]

  , Mission
    "Chōji Akimichi (S)"
    [ win 5 ["Chōji Akimichi", "Asuma Sarutobi"]

    , Reach Career 1
      "Use [Spinach Pill], [Curry Pill], and [Chili Pill] in 3 consecutive turns." $
      Consecutive "Chōji Akimichi" ["Spinach Pill", "Curry Pill", "Chili Pill"]

    , Reach Match 90
      "In a single match, restore 90 health with [Chakra Wings]." $
      HookAction "Chōji Akimichi" "Chakra Wings"
      heal

    , Reach Career 10
      "Kill 10 enemies with the instant-kill effect of [Butterfly Bombing]." $
      HookAction "Chōji Akimichi" "Butterfly Bombing"
      execute
    ]

  , Mission
    "Ino Yamanaka (S)"
    [ win 5 ["Ino Yamanaka", "Asuma Sarutobi"]

    , Reach Career 10
      "Kill 10 enemies with [Mind Destruction]." $
      HookAction "Ino Yamanaka" "Mind Destruction"
      kill

    , Reach Match 4
      "Maintain [Mind Transfer] for 4 consecutive turns without being interrupted." .
      HookTurn "Ino Yamanaka" $
      maintain "Mind Transfer"

    , Reach Match 3
      "In a single match, cause all 3 enemies to activate [Chakra Hair Trap]." $
      HookTrap "Ino Yamanaka" "Chakra Hair Trap"
      trapUniqueEnemy
    ]

  , Mission
    "Rock Lee (S)"
    [ win 5 ["Rock Lee", "Might Guy"]

    , Reach Turn 3
      "In a single turn, damage all 3 enemies with [Ferocious Fist]." $
      HookStore "Rock Lee" "Hidden Lotus"
      damageUnique

    , Reach Career 10
      "Kill 10 enemies with [Primary Lotus] during [Fifth Gate Opening]." .
      HookAction "Rock Lee" "Primary Lotus" $
      killDuring "Fifth Gate Opening"

    , Reach Match 3
      "In a single match, kill all 3 enemies with [Hidden Lotus]." $
      HookStore "Rock Lee" "Hidden Lotus"
      killUnique
    ]

  , Mission
    "Tenten (S)"
    [ win 5 ["Tenten", "Might Guy"]

    , Reach Moment 5
      "Use [Rising Dragon Control] with at least 5 stacks of [Unsealing Technique]." .
      HookAction "Tenten" "Rising Dragon Control" $
      useDuringStacks "Unsealing Technique"

    , Reach Career 10
      "Empower [Unsealing Technique] with [Rising Twin Dragons] 10 times." .
      HookAction "Tenten" "Unsealing Technique" $
      useDuring "Rising Twin Dragons"

    , Reach Career 10
      "Empower [Rising Dragon Control] with [Rising Twin Dragons] 10 times." .
      HookAction "Tenten" "Rising Dragon Control" $
      useDuring "Rising Twin Dragons"
    ]

  , Mission
    "Neji Hyūga (S)"
    [ win 5 ["Neji Hyūga", "Might Guy"]

    , Reach Match 6
      "Maintain [Gentle Fist] for 6 consecutive turns." .
      HookTurn "Neji Hyūga" $
      maintain "Gentle Fist"

    , Reach Career 10
      "Kill 10 enemies with [Eight Trigrams Sixty-Four Palms]." $
      HookAction "Neji Hyūga" "Eight Trigrams Sixty-Four Palms"
      kill

    , Reach Match 4
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
    [ win 5 ["Gaara", "Baki"]

    , Reach Career 10
      "Kill 10 enemies with [Sand Burial]." $
      HookAction "Gaara" "Sand Burial"
      kill

    , Reach Match 8
      "Maintain [Sand Clone] for 8 consecutive turns." .
      HookTurn "Gaara" $
      maintain "Sand Clone"

    , Reach Match 160
      "In a single match, apply 160 destructible defense with [Sand Armor]." $
      HookAction "Gaara" "Sand Armor"
      defend
    ]

  , Mission
    "Kankurō (S)"
    [ win 5 ["Kankurō", "Baki"]

    , Reach Career 10
      "Kill 10 enemies with [Iron Maiden]." $
      HookAction "Kankurō" "Iron Maiden"
      kill

    , Reach Moment 2
      "Use [Iron Maiden] with at least 2 stacks of [Puppet Technique] to damage an enemy." .
      HookAction "Kankurō" "Iron Maiden" $
      damageDuringStacks "Puppet Technique"

    , Reach Moment 2
      "Use [Poison Bomb] with at least 2 stacks of [Puppet Technique] to damage an enemy." .
      HookAction "Kankurō" "Poison Bomb" $
      damageDuringStacks "Puppet Technique"
    ]

  , Mission
    "Temari (S)"
    [ win 5 ["Temari", "Baki"]

    , Reach Career 10
      "Kill 10 enemies with [Cyclone Scythe]." $
      HookAction "Temari" "Cyclone Scythe"
      kill

    , Reach Turn 2
      "In a single turn, kill 2 enemies with [Summoning: Blade Dance]." $
      HookAction "Temari" "Summoning: Blade Dance"
      kill

    , Reach Match 4
      "Use all 4 skills in 4 consecutive turns." $
      Consecutive "Temari"
      ["Cyclone Scythe", "Summoning: Blade Dance", "Sandstorm", "Block"]
    ]

  , Mission
    "Konohamaru Sarutobi (S)"
    [ win 5 ["Konohamaru Sarutobi", "Naruto Uzumaki"]

    , Reach Career 20
      "With any team member, kill 20 enemies affected by [Unsexy Technique]." .
      HookTurn "Konohamaru Sarutobi" $
      killWith "Unsexy Technique"

    , Reach Match 6
      "Maintain [Throw a Fit] for 6 consecutive turns." .
      HookTurn "Konohamaru Sarutobi" $
      maintain "Throw a Fit"

    , Reach Turn 40
      "In a single turn, deal at least 40 damage with [Throw a Shuriken]." $
      HookAction "Konohamaru Sarutobi" "Throw a Shuriken"
      damage
    ]

  , Mission
    "Kabuto Yakushi (S)"
    [ win 5 ["Yoroi Akadō", "Misumi Tsurugi"]

    , Reach Career 20
      "With any team member, kill 20 enemies affected by [Chakra Scalpel]." .
      HookTurn "Kabuto Yakushi" $
      killWith "Chakra Scalpel"

    , Reach Career 3
      "Cure 3 status effects with [Pre-Healing Technique]." $
      HookAction "Kabuto Yakushi" "Pre-Healing Technique"
      cure

    , Reach Match 3
      "In a single match, stun all 3 enemies with [Temple of Nirvana]." $
      HookStore "Kabuto Yakushi" "Temple of Nirvana"
      stunUnique
    ]
  ]
