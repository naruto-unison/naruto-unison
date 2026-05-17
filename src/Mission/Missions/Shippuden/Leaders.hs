{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Mission.Missions.Shippuden.Leaders (missions) where

import Mission.Missions.Import

missions :: [Mission]
missions =
  [ Mission
    "Orochimaru (S)"
    [ win 5 ["Suigetsu Hōzuki (S)", "Karin (S)", "Jūgo (S)"]

    , Reach Career 200
      "Destroy 200 total destructible defense with [Kusanagi]." $
      HookAction "Orochimaru" "Kusanagi"
      demolish

    , Reach Match 5
      "In a single match, use [Curse Mark] on all 5 other characters." $
      HookStore "Orochimaru" "Curse Mark"
      useUnique

    , Reach Career 1
      "Kill an enemy with [Paralyzing Bite]." $
      HookAction "Orochimaru" "Paralyzing Bite"
      kill
    ]

  , Mission
    "Jiraiya (S)"
    [ win 5 ["Minato Namikaze", "Kushina Uzumaki"]

    , Reach Career 20
      "With any team member, kill 20 enemies affected by [Summoning: Toad Mouth Trap]." .
      HookTurn "Jiraiya" $
      killWith "Summoning: Toad Mouth Trap"

    , Reach Match 3
      "In a single match, cause all 3 team members to become invulnerable with [Summoning: Toad Mouth Trap]." $
      HookTrap "Jiraiya" "Summoning: Toad Mouth Trap"
      trapUniqueAlly

    , Reach Moment 1
      "Use [Toad Oil Bomb] to damage an enemy affected by [Toad Oil Bomb]." .
      HookAction "Jiraiya" "Toad Oil Bomb" $
      damageWithStacks "Toad Oil Bomb"
    ]

  , Mission
    "Tsunade (S)"
    [ win 5 ["Hiruzen Sarutobi", "Yondaime Minato"]

    , Reach Match 3
      "In a single match, stun all 3 enemies with [Heavenly Kick of Pain]." $
      HookStore "Tsunade" "Heavenly Kick of Pain"
      stunUnique

    , Reach Moment 80
      "Restore 80 health with a single use of [Mitotic Regeneration]." $
      HookAction "Tsunade" "Mitotic Regeneration"
      heal

    , Reach Career 200
      "Provide 200 destructible defense with [Slug Division]." $
      HookAction "Tsunade" "Slug Division"
      defend
    ]
  ]
