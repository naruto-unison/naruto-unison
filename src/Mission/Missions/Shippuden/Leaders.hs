{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Mission.Missions.Shippuden.Leaders (missions) where

import Mission.Missions.Import

missions :: [Mission]
missions =
  [ Mission
    "Orochimaru (S)"
    [ win 5 ["Suigetsu Hōzuki (S)", "Karin (S)", "Jūgo (S)"]

    , Reach 200 Career
      "Destroy 200 total destructible defense with [Kusanagi]." $
      HookAction "Orochimaru" "Kusanagi"
      demolish

    , Reach 5 Match
      "In a single match, use [Curse Mark] on all 5 other characters." $
      HookStore "Orochimaru" "Curse Mark"
      useUnique

    , Reach 1 Career
      "Kill an enemy with [Paralyzing Bite]." $
      HookAction "Orochimaru" "Paralyzing Bite"
      kill
    ]

  , Mission
    "Jiraiya (S)"
    [ win 5 ["Minato Namikaze", "Kushina Uzumaki"]

    , Reach 20 Career
      "With any team member, kill 20 enemies affected by [Summoning: Toad Mouth Trap]." .
      HookTurn "Jiraiya" $
      killWith "Summoning: Toad Mouth Trap"

    , Reach 3 Match
      "In a single match, cause all 3 team members to become invulnerable with [Summoning: Toad Mouth Trap]." $
      HookTrap "Jiraiya" "Summoning: Toad Mouth Trap"
      trapUniqueAlly

    , Reach 1 Moment
      "Use [Toad Oil Bomb] to damage an enemy affected by [Toad Oil Bomb]." .
      HookAction "Jiraiya" "Toad Oil Bomb" $
      damageWithStacks "Toad Oil Bomb"
    ]

  , Mission
    "Tsunade (S)"
    [ win 5 ["Hiruzen Sarutobi", "Yondaime Minato"]

    , Reach 3 Match
      "In a single match, stun all 3 enemies with [Heavenly Kick of Pain]." $
      HookStore "Tsunade" "Heavenly Kick of Pain"
      stunUnique

    , Reach 80 Moment
      "Restore 80 health with a single use of [Mitotic Regeneration]." $
      HookAction "Tsunade" "Mitotic Regeneration"
      heal

    , Reach 200 Career
      "Provide 200 destructible defense with [Slug Division]." $
      HookAction "Tsunade" "Slug Division"
      defend
    ]
  ]
