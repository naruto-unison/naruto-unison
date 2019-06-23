{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Original.Teachers (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Iruka Umino"
     "A chūnin from the Hidden Leaf Village, Iruka is a kind instructor who has mentored Naruto throughout his youth. He fights to keep his allies safe and protect them from harm, growing stronger out of desperation the closer to death he is."
    [ [ Skill.new
        { Skill.name      = "Shuriken Throw"
        , Skill.desc      = "Iruka throws a shuriken at an enemy, dealing 20 damage plus 10 per 25 health Iruka has lost."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy do
                hp <- user health
                damage $ 20 + 10 * ((100 - hp) `quot` 25)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ally Shield"
        , Skill.desc      = "Using himself as a shield, Iruka makes an ally invulnerable for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p XAlly $ apply 1 [Invulnerable All] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Capture and Arrest"
        , Skill.desc      = "Iruka traps an enemy in an area filled with his paper bombs. If the target uses a skill on Iruka or his allies during their next turn, they will receive 40 damage, and physical and chakra skills will deal 25 additional damage to them for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy $ trap 1 OnHarm do
                damage 40
                apply 1 [Bleed Physical Flat 25, Bleed Chakra Flat 25 ]
          ]
        }
      ]
    , [ invuln "Parry" "Iruka" [Physical] ]
    ] []
  , Character
    "Mizuki"
    "A chūnin from the Hidden Leaf Village, Mizuki is an unpleasant instructor who betrays his allies without hesitation in order to succeed. Unless forced into direct combat, he slips into the shadows and ambushes his enemies at their weakest."
    [ [ Skill.new
        { Skill.name      = "Kunai Assault"
        , Skill.desc      = "Mizuki throws a series of kunai at an enemy, dealing 15 damage for 2 turns. Deals all 30 damage instantly during [Successful Ambush]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ p Enemy $ damage 15 ]
        }
      , Skill.new
        { Skill.name      = "Kunai Assault"
        , Skill.desc      = "Mizuki throws a series of kunai at an enemy, dealing 15 damage for 2 turns. Deals all 30 damage instantly during [Successful Ambush]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ damage 30 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Execution Shuriken"
        , Skill.desc      = "Mizuki throws one of his two giant shurikens at an enemy, dealing 10 damage plus 10 per 20 health the target has lost. Deals 30 additional damage during [Successful Ambush]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Tai, Rand]
        , Skill.charges   = 2
        , Skill.effects   =
          [ p Enemy do
                hp     <- target health
                bonus  <- 30 `bonusIf` userHas "Successful Ambush"
                damage $ 20 + bonus + 10 * ((100 - hp) `quot` 20)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Genjutsu Ambush Tactics"
        , Skill.desc      = "Mizuki lurks in the shadows. If no enemy uses a skill that deals damage to him, he becomes invulnerable for 1 turn and his other skills are empowered."
        , Skill.classes   = [Mental, InvisibleTraps]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self do
                trap (-1) (OnDamaged All) $ remove "Ambush Preparation"
                bombWith' [Hidden] "Ambush Preparation" (-1) []
                  [ p Expire $ self do
                        vary' 1 "Kunai Assault" "Kunai Assault"
                        apply (-1) [Invulnerable All]
                  ]
          ]
        }
      ]
    , [ invuln "Dodge" "Mizuki" [Physical] ]
    ] []
  , Character
    "Kakashi Hatake"
    "Team 7's jōnin squad leader, Kakashi puts the life of his teammates above all else. Known as the Copy Ninja, Kakashi analyzes and duplicates abilities used against him."
    [ [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Kakashi anticipates any attacks against him, copying them faster than their user. All harmful skills that target Kakashi next turn will be reflected."
        , Skill.classes   = [Chakra, Melee, Invisible]
        , Skill.cost      = k [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self $ apply 1 [ReflectAll] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Ninja Hounds"
        , Skill.desc      = "A pack of giant dogs bite at an enemy, stunning the target's non-mental skills for 1 turn."
        , Skill.classes   = [Physical, Ranged, Summon]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy $ apply 1 [Stun NonMental] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lightning Blade"
        , Skill.desc      = "Kakashi devastates an enemy with a blast of lightning chakra, dealing 50 piercing damage. Instantly kills targets affected by [Summoning: Ninja Hounds]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                pierce 50
                whenM (targetHas "Summoning: Ninja Hounds") kill
          ]
        }
      ]
    , [ invuln "Hide" "Kakashi" [Mental] ]
    ] []
  , Character
    "Anko Mitarashi"
    "A former student of Orochimaru who bears his Curse Mark, Anko is now a jōnin teacher in the Hidden Leaf Village. She uses various poisons and forbidden techniques learned from Orochimaru to dismantle her enemies."
    [ [ Skill.new
        { Skill.name      = "Dual Pin"
        , Skill.desc      = "Anko pins herself to an enemy by stabbing a kunai through her hand, dealing 5 damage and preventing the target from reducing damage or becoming invulnerable for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.channel   = Control 1
        , Skill.effects   =
          [ p Enemy do
                bonus <- 5 `bonusIf` targetHas "Dragon Flame"
                damage (5 + bonus)
                apply 1 [Expose]
          , p Self do
                vary "Dragon Flame" "Twin Snake Sacrifice"
                tag' "Twin Snake Sacrifice" 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dragon Flame"
        , Skill.desc      = "Fire scorches the battlefield, dealing 10 affliction damage to all enemies for 2 turns. All of Anko's damage is increased by 5 against targets affected by [Dragon Flame]. During [Dual Pin], this skill becomes [Twin Snake Sacrifice][n][n]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemies $ apply 2 [Afflict 10] ]
        }
      , Skill.new
        { Skill.name      = "Twin Snake Sacrifice"
        , Skill.desc      = "Kills Anko and the target of [Dual Pin]."
        , Skill.require   = HasU "Dual Pin"
        , Skill.classes   = [Melee]
        , Skill.cost      = k [Nin, Nin]
        , Skill.effects   =
          [ p Enemies $ kill
          , p Self    $ killHard
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Striking Shadow Snakes"
        , Skill.desc      = "Numerous snakes attack an enemy, dealing 20 damage instantly and 5 affliction damage each turn for 3 turns."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 5 `bonusIf` targetHas "Dragon Flame"
                damage (20 + bonus)
                apply 3 [Afflict 5]
          ]
        }
      ]
    , [ invuln "Dodge" "Anko" [Physical] ]
    ] []
  , Character
    "Hayate Gekkō"
    "A jōnin exam proctor from the Hidden Leaf Village, Hayate is calm and composed despite his poor health and chronic cough. He slips in and out of the shadows, gradually recovering his strength and honing his expert swordsmanship."
    [ [ Skill.new
        { Skill.name      = "Secret Sword"
        , Skill.desc      = "Hayate leaps and attacks an enemy from above, dealing 15 damage. Permanently increases Hayate's damage by 5."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy $ damage 15
          , p Self  $ apply 0 [Strengthen All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crescent Moon Dance"
        , Skill.desc      = "Performing a genjutsu-aided triple sword strike, Hayate deals 30 damage to an enemy. Catching his opponents off guard, he also counters the first harmful skill used on him in the next turn."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = k [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ damage 30
          , p Self  $ apply 1 [Counter All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Transparency Technique"
        , Skill.desc      = "Hayate melds into shadows, increasing his damage by 10 for 3 turns. Each turn, he gains 1 turn of damage reduction: 25 points on the first turn, 15 points on the second, and 5 points on the third."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self do
                apply 3 [Strengthen All Flat 10]
                apply 1 [Reduce All Flat 25]
                delay (-1) $ apply 1 [Reduce All Flat 15]
                delay (-2) $ apply 1 [Reduce All Flat 5]
          ]
        }
      ]
    , [ invuln "Dodge" "Hayate" [Physical] ]
    ] []
  , Character
    "Kurenai Yuhi"
    "Team 8's jōnin squad leader, Kurenai is caring and brave. A master of genjutsu, Kurenai traps her enemies in inescapable illusions."
    [ [ Skill.new
        { Skill.name      = "Demonic Illusion: Entrap"
        , Skill.desc      = "Kurenai hinders an enemy with her genjutsu, dealing 10 damage. For 2 turns, the target's non-affliction damage is weakened by 10, the chakra costs of their skills is increased by 1 random chakra, and they cannot reduce damage or become invulnerable. Adds 5 destructible defense to Kurenai's next [Illusory Tree Meld]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                damage 10
                apply 2 [Weaken All Flat 10, Exhaust All, Expose]
          , p Self $ addStacks "Illusion" 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Illusory Tree Meld"
        , Skill.desc      = "Kurenai melds into an illusory tree, gaining 10 permanent destructible defense."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Self do
                stacks <- userStacks "Illusion"
                defend 0 (10 + 5 * stacks)
                remove "Illusion"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Demonic Illusion: Sylvan Fetters"
        , Skill.desc      = "Kurenai traps an enemy in a powerful genjutsu, stunning them for 2 turns. Adds 5 destructible defense to Kurenai's next [Illusory Tree Meld] each turn. While active, this skill becomes [Sylvan Fetters Attack][r]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Gen, Gen]
        , Skill.cooldown  = 4
        , Skill.channel   = Control 2
        , Skill.start     =
          [ p Self $
                vary "Demonic Illusion: Sylvan Fetters" "Sylvan Fetters Attack"
          ]
        , Skill.effects   =
          [ p Enemy $ apply 1 [Stun All]
          , p Self  $ addStacks "Illusion" 1
          ]
        }
      , Skill.new
        { Skill.name      = "Sylvan Fetters Attack"
        , Skill.desc      = "Deals 30 piercing damage to the target of [Demonic Illusion: Sylvan Fetters]."
        , Skill.require   = HasU "Demonic Illusion: Sylvan Fetters"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemy $ pierce 30 ]
        }
      ]
    , [ invuln "Vanish" "Kurenai" [Mental] ]
    ] []
  , Character
    "Asuma Sarutobi"
    "Team 10's squad leader, Asuma is a chronic smoker and the third Hokage's son. He focuses on protecting his team, providing them with defense and taking blows in their stead."
    [ [ Skill.new
        { Skill.name      = "Flying Swallow"
        , Skill.desc      = "Asuma carries out a series of chakra-enhanced knife attacks. For 2 turns, he deals 15 damage to all enemies and provides 15 points of damage reduction to his team. While active, this skill becomes [Finishing Blow][n][r] and [Sharpen Blades] becomes [Flying Kick][t][r]."
        , Skill.classes   = [Physical, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = k [Nin, Tai]
        , Skill.cooldown  = 2
        , Skill.channel   = Action 2
        , Skill.start     =
          [ p Self do
                vary "Flying Swallow" "Finishing Blow"
                vary "Sharpen Blades" "Flying Kick"
          ]
        , Skill.effects   =
          [ p Enemies $ damage 15
          , p Allies  $ apply 1 [Reduce All Flat 15]
          , p Self    $ remove "Sharpen Blades"
          ]
        , Skill.changes   = extendWith "Sharpen Blades" 1
        }
      , Skill.new
        { Skill.name      = "Finishing Blow"
        , Skill.desc      = "Deals 35 damage to an enemy and stuns them for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 35
                apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharpen Blades"
        , Skill.desc      = "Asuma sharpens his blades, increasing the duration of his next [Flying Swallow] by 1 additional turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Self addStack ]
        }
      , Skill.new
        { Skill.name      = "Flying Kick"
        , Skill.desc      = "Deals 35 damage to an enemy."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy $ damage 35 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Self-Sacrifice"
        , Skill.desc      = "Asuma continually protects one ally. All harmful skills used on the target will be reflected to Asuma. This skill can be used again with no chakra cost to cancel its effect."
        , Skill.classes   = [Physical, Melee, Soulbound, Unreflectable]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p XAlly do
                userSlot <- user slot
                apply 0 [Redirect All userSlot]
          , p Self  $ vary "Self-Sacrifice" "Self-Sacrifice"
          ]
        }
      , Skill.new
        { Skill.name      = "Self-Sacrifice"
        , Skill.desc      = "Ends the effect of [Self-Sacrifice]."
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.varicd    = True
        , Skill.effects   =
          [ p Self do
                vary "Self-Sacrifice" baseVariant
                everyone $ remove "Self-Sacrifice"
          ]
        }
      ]
    , [ invuln "Parry" "Asuma" [Physical] ]
    ] []
  , Character
    "Might Guy"
    "Team Guy's jōnin squad leader, Guy is passionate and strong-willed. He treats his teammates like family, especially Lee, who looks up to him as a father figure. His taijutsu prowess is unmatched. Although opening his inner Gates takes a heavy toll on his body, it empowers his blows with inescapable destruction."
    [ [ Skill.new
        { Skill.name      = "Leaf Hurricane"
        , Skill.desc      = "Guy delivers a powerful spinning kick to an enemy, dealing 30 damage. Deals 30 additional damage and bypasses invulnerability during [Sixth Gate Opening]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 30 `bonusIf` userHas "Sixth Gate Opening"
                damage (30 + bonus)
          ]
        , Skill.changes   = changeWith "Sixth Gate Opening" $ addClass Bypassing
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sixth Gate Opening"
        , Skill.desc      = "Guy loses 40 health down to a minimum of 1 and becomes invulnerable for 2 turns."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                apply 2 [Invulnerable All]
                sacrifice 1 40
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Counter Punch"
        , Skill.desc      = "Guy singles out an enemy. If the target uses a harmful skill next turn, they will be countered and receive 30 damage."
        , Skill.classes   = [Physical, Melee, Invisible]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy $ trap 1 (OnCounter All) $ damage 30 ]
        }
      ]
    , [ invuln "Dodge" "Guy" [Physical] ]
    ] []
  , Character
    "Baki"
    "A jōnin from the Hidden Sand Village, Baki is a ruthlessly efficient squad leader. He cloaks his attacks in illusions to prevent his enemies from defending against them."
    [ [ Skill.new
        { Skill.name      = "Sudden Strike"
        , Skill.desc      = "Baki performs a blindingly fast attack concealed by decoy illusory trails, dealing 20 piercing damage to an enemy."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Gen]
        , Skill.effects   =
          [ p Enemy $ pierce 20 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wind Sword"
        , Skill.desc      = "A maelstrom of stabbing wind deals 40 piercing damage to an enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Gen, Nin]
        , Skill.effects   =
          [ p Enemy $ pierce 40 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flak Jacket"
        , Skill.desc      = "Using a top-of-the-line Hidden Sand Village flak jacket, Baki provides 50 destructible defense to himself or an ally. While the target has destructible defense from this skill, they ignore harmful non-damage effects other than chakra cost changes."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ p Ally do
                defend 4 50
                apply 4 [Enrage]
                onBreak'
          ]
        }
      ]
    , [ invuln "Teleport" "Baki" [Chakra] ]
    ] []
  , Character
    "Shizune"
    "A jōnin from the Hidden Leaf Village, Shizune is a talented medical-nin apprenticed to Tsunade. She is agile and fast, fully capable of holding her own in a fight whenever she isn't healing one of her allies."
    [ [ Skill.new
        { Skill.name      = "Poison Needle Volley"
        , Skill.desc      = "Shizune shoots hidden needles at an enemy, dealing 15 damage."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemy $ damage 15 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Poison Fog"
        , Skill.desc      = "Shizune spews forth a toxic cloud to poison an enemy, causing them to receive 10 affliction damage for 3 turns. Cannot be used on an enemy already affected by [Poison Fog]."
        , Skill.classes   = [Bane, Ranged, Single]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ apply 3 [Afflict 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Regenerative Healing Technique"
        , Skill.desc      = "Using a notoriously difficult healing technique, Shizune restores 35 health to herself or an ally and cures them of enemy effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p XAlly do
                cureAll
                heal 35
          ]
        }
      ]
    , [ invuln "Dodge" "Shizune" [Physical] ]
    ] []
  ]
