{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Original.Teachers (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Iruka Umino"
     "A chūnin from the Hidden Leaf Village, Iruka is a kind instructor who has mentored Naruto throughout his youth. He fights to keep his allies safe and protect them from harm, growing stronger out of desperation the closer to death he is."
    [LeafVillage, Chunin, Fire, Water, Yin]
    [ [ Skill.new
        { Skill.name      = "Shuriken Throw"
        , Skill.desc      = "Iruka throws a shuriken at an enemy, dealing 20 damage plus 10 per 25 health Iruka has lost."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                userHealth <- user health
                damage $ 20 + 10 * ((100 - userHealth) `quot` 25)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ally Shield"
        , Skill.desc      = "Using himself as a shield, Iruka makes an ally invulnerable for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly $ apply 1 [Invulnerable All] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Capture and Arrest"
        , Skill.desc      = "Iruka traps an enemy in an area filled with his paper bombs. If the target uses a skill on Iruka or his allies during their next turn, they will take 40 damage, and physical, chakra, and summon skills will deal 25 additional damage to them for 1 turn."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Invisible]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 1 OnHarm do
                damage 40
                apply 1 [Bleed [Physical, Chakra, Summon] Flat 25]
          ]
        }
      ]
    , [ invuln "Parry" "Iruka" [Physical] ]
    ]
  , Character
    "Mizuki"
    "A chūnin from the Hidden Leaf Village, Mizuki is an unpleasant instructor who betrays his allies without hesitation in order to succeed. Unless forced into direct combat, he slips into the shadows and ambushes his enemies at their weakest."
    [LeafVillage, Orochimaru, Chunin, Rogue, Earth, Yin]
    [ [ Skill.new
        { Skill.name      = "Kunai Assault"
        , Skill.desc      = "Mizuki throws a series of kunai at an enemy, dealing 15 damage for 2 turns. Deals all 30 damage instantly during [Successful Ambush]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy $ damage 15 ]
        , Skill.changes   =
            changeWith "Successful Ambush" \x ->
              x { Skill.dur     = Instant
                , Skill.effects =
                  [ To Enemy $ damage 30 ]
                }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Execution Shuriken"
        , Skill.desc      = "Mizuki throws one of his two giant shurikens at an enemy, dealing 10 damage plus 10 per 20 health the target has lost. Deals 30 additional damage during [Successful Ambush]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai, Rand]
        , Skill.charges   = 2
        , Skill.effects   =
          [ To Enemy do
                targetHealth <- target health
                bonus        <- 30 `bonusIf` userHas "Successful Ambush"
                damage $ 10 + bonus + 10 * ((100 - targetHealth) `quot` 20)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Genjutsu Ambush Tactics"
        , Skill.desc      = "Mizuki lurks in the shadows. If no enemy uses a skill that deals damage to him, he becomes invulnerable for 1 turn as a Successful Ambush."
        , Skill.classes   = [Mental, Invisible]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                trap (-1) (OnDamaged All) $ remove "Genjutsu Ambush Tactics"
                bombWith [Hidden] (-1) []
                  [ To Expire $
                        apply' "Successful Ambush" (-1) [Invulnerable All] ]
          ]
        }
      ]
    , [ invuln "Dodge" "Mizuki" [Physical] ]
    ]
  , Character
    "Anko Mitarashi"
    "A former student of Orochimaru who bears his Curse Mark, Anko is now a jōnin teacher in the Hidden Leaf Village. She uses various poisons and forbidden techniques learned from Orochimaru to dismantle her enemies."
    [LeafVillage, AlliedForces, Jonin, Fire, Yin]
    [ [ Skill.new
        { Skill.name      = "Dual Pin"
        , Skill.desc      = "Anko pins herself to an enemy by stabbing a kunai through her hand, preventing the target from reducing damage or becoming invulnerable for 1 turn and dealing 5 damage. Deals 5 additional damage if the target is affected by [Dragon Flame]."
        , Skill.classes   = [Physical, Melee]
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Expose]
                bonus <- 5 `bonusIf` targetHas "Dragon Flame"
                damage (5 + bonus)
          , To Self $ apply' "Twin Snake Sacrifice" 1
                [Alternate "Dragon Flame" "Twin Snake Sacrifice"]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dragon Flame"
        , Skill.desc      = "Fire scorches the battlefield, dealing 10 affliction damage to all enemies for 2 turns and causing them to take 5 additional damage from bane effects. During [Dual Pin], this skill becomes [Twin Snake Sacrifice][n][n]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies $ apply 2 [Bleed [Bane] Flat 5, Afflict 5] ]
        }
      , Skill.new
        { Skill.name      = "Twin Snake Sacrifice"
        , Skill.desc      = "Anko introduces a lethal poison through the wound she shares with the target of [Dual Pin], killing both."
        , Skill.require   = HasU 1 "Dual Pin"
        , Skill.classes   = [Bane, Melee, Bypassing]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy kill
          , To Self killHard
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Striking Shadow Snakes"
        , Skill.desc      = "Numerous poisonous snakes attack an enemy, dealing 20 damage instantly and 5 affliction damage for 3 turns."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 3 [Afflict 5]
          ]
        }
      ]
    , [ invuln "Dodge" "Anko" [Physical] ]
    ]
  , Character
    "Hayate Gekkō"
    "A jōnin exam proctor from the Hidden Leaf Village, Hayate is calm and composed despite his poor health and chronic cough. He slips in and out of the shadows, gradually recovering his strength and honing his expert swordsmanship."
    [LeafVillage, Jonin]
    [ [ Skill.new
        { Skill.name      = "Secret Sword"
        , Skill.desc      = "Hayate leaps and attacks an enemy from above, dealing 15 damage. Increases Hayate's damage by 5."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy $ damage 15
          , To Self $ apply 0 [Strengthen [All] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crescent Moon Dance"
        , Skill.desc      = "Performing a genjutsu-aided triple sword strike, Hayate deals 30 damage to an enemy. Catching his opponents off guard, he also counters the first skill an enemy uses on him in next turn."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ damage 30
          , To Self $ trap 1 (Counter All) $ return ()
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Transparency Technique"
        , Skill.desc      = "Hayate melds into shadows, increasing his damage by 10 for 3 turns. Each turn, he gains 1 turn of damage reduction: 25 points on the first turn, 15 points on the second, and 5 points on the third."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                apply 1
                    [Strengthen [All] Flat 10, Reduce [All] Flat 25]
                delay 1 $ apply 1
                    [Strengthen [All] Flat 10, Reduce [All] Flat 15]
                delay 2 $ apply 1
                    [Strengthen [All] Flat 10, Reduce [All] Flat 5]
          ]
        }
      ]
    , [ invuln "Dodge" "Hayate" [Physical] ]
    ]
  , Character
    "Kakashi Hatake"
    "Team 7's jōnin squad leader, Kakashi puts the life of his teammates above all else. Known as the Copy Ninja, Kakashi analyzes and duplicates abilities used against him."
    [LeafVillage, Jonin, TeamLeader, Lightning, Water, Earth, Fire, Wind, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Kakashi anticipates any attacks against him, copying them faster than their user. All skills that enemies use on Kakashi next turn will be reflected back at them."
        , Skill.classes   = [Chakra, Melee, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ apply 1 [ReflectAll All] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Ninja Hounds"
        , Skill.desc      = "A pack of giant dogs bite at an enemy, stunning the target's non-mental skills for 1 turn."
        , Skill.classes   = [Summon, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun NonMental] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lightning Blade"
        , Skill.desc      = "Kakashi devastates an enemy with a blast of lightning chakra, dealing 50 piercing damage. Instantly kills targets affected by [Summoning: Ninja Hounds]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 50
                whenM (targetHas "Summoning: Ninja Hounds") kill
          ]
        }
      ]
    , [ invuln "Hide" "Kakashi" [Mental] ]
    ]
  , Character
    "Kurenai Yuhi"
    "Team 8's jōnin squad leader, Kurenai is caring and brave. A master of genjutsu, Kurenai traps her enemies in inescapable illusions."
    [LeafVillage, Jonin, Sensor, TeamLeader, Yin, Sarutobi]
    [ [ Skill.new
        { Skill.name      = "Demonic Illusion: Entrap"
        , Skill.desc      = "Kurenai hinders an enemy with her genjutsu. For 2 turns, the target's damage is weakened by 10, the chakra costs of their skills is increased by 1 arbitrary chakra, and they cannot reduce damage or become invulnerable. Kurenai then deals 10 damage to the target. Adds 5 destructible defense to Kurenai's next [Illusory Tree Meld]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                apply 2 [Weaken [All] Flat 10, Exhaust [All], Expose]
                damage 10
          , To Self $ addStack' "Illusion"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Illusory Tree Meld"
        , Skill.desc      = "Kurenai melds into an illusory tree, gaining 10 permanent destructible defense."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                stacks <- userStacks "Illusion"
                defend 0 (10 + 5 * stacks)
                remove "Illusion"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Demonic Illusion: Sylvan Fetters"
        , Skill.desc      = "Kurenai traps an enemy in a powerful genjutsu, stunning them for 2 turns. Adds 5 destructible defense to Kurenai's next [Illusory Tree Meld]. While active, this skill becomes [Sylvan Fetters Attack][r]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Gen]
        , Skill.cooldown  = 4
        , Skill.dur       = Control 2
        , Skill.start     =
          [ To Enemy $ addStack' "Illusion" ]
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun All]
          , To Self $ hide 1 [Alternate "Demonic Illusion: Sylvan Fetters"
                                        "Sylvan Fetters Attack"]
          ]
        , Skill.interrupt =
          [ To Self $ remove "demonic illusion: sylvan fetters" ]
        }
      , Skill.new
        { Skill.name      = "Sylvan Fetters Attack"
        , Skill.desc      = "Taking advantage of their immobility, Kurenai deals 30 piercing damage to the target of [Demonic Illusion: Sylvan Fetters]."
        , Skill.require   = HasU 1 "Demonic Illusion: Sylvan Fetters"
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 30 ]
        }
      ]
    , [ invuln "Vanish" "Kurenai" [Mental] ]
    ]
  , Character
    "Asuma Sarutobi"
    "Team 10's jōnin squad leader, Asuma is a chronic smoker and the third Hokage's son. He focuses on protecting his team, providing them with defense and taking blows in their stead."
    [LeafVillage, Jonin, TeamLeader, Wind, Fire, Sarutobi]
    [ [ Skill.new
        { Skill.name      = "Flying Swallow"
        , Skill.desc      = "Asuma carries out a series of chakra-enhanced knife attacks. For 2 turns, he deals 15 damage to all enemies and provides 15 points of damage reduction to his team. While active, this skill becomes [Finishing Blow][n][r] and [Sharpen Blades] becomes [Flying Kick][t][r]."
        , Skill.classes   = [Physical, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemies $ damage 15
          , To Allies $ apply 1 [Reduce [All] Flat 15]
          , To Self do
                remove "Sharpen Blades"
                hide 1 [ Alternate "Flying Swallow" "Finishing Blow"
                       , Alternate "Sharpen Blades" "Flying Kick"
                       ]
          ]
        , Skill.stunned   =
          [ To Self do
                remove "Sharpen Blades"
                hide 1 [ Alternate "Flying Swallow" "Finishing Blow"
                       , Alternate "Sharpen Blades" "Flying Kick"
                       ]
          ]
        , Skill.changes   =
            extendWith "Sharpen Blades" 1
        }
      , Skill.new
        { Skill.name      = "Finishing Blow"
        , Skill.desc      = "Asuma deals 35 piercing damage to an enemy with a powerful attack."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 35 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharpen Blades"
        , Skill.desc      = "Asuma hones his blades with a coating of wind chakra, increasing the duration of his next [Flying Swallow] by 1 additional turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self addStack ]
        }
      , Skill.new
        { Skill.name      = "Flying Kick"
        , Skill.desc      = "Asuma kicks an enemy squarely in the face, dealing 35 damage and stunning them for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 35
                apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Self-Sacrifice"
        , Skill.desc      = "Asuma continually protects one ally. All skills that enemies use on the target will be reflected to Asuma. This skill can be used again with no chakra cost to cancel its effect."
        , Skill.classes   = [Physical, Melee, Soulbound, Unremovable, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To XAlly do
                userSlot <- user slot
                bomb 0 [Redirect userSlot]
                    [ To Done $ self $ remove "self-sacrifice" ]
          , To Self $ hide 0 [Alternate "Self-Sacrifice" "Self-Sacrifice "]
          ]
        }
      , Skill.new
        { Skill.name      = "Self-Sacrifice "
        , Skill.desc      = "Ends the effect of [Self-Sacrifice]."
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.effects   =
          [ To Self $ everyone $ remove "Self-Sacrifice" ]
        }
      ]
    , [ invuln "Parry" "Asuma" [Physical] ]
    ]
  , Character
    "Might Guy"
    "Team Guy's jōnin squad leader, Guy is passionate and strong-willed. He treats his teammates like family, especially Lee, who looks up to him as a father figure. His taijutsu prowess is unmatched. Although opening his inner Gates takes a heavy toll on his body, it empowers his blows with inescapable destruction."
    [LeafVillage, Jonin, TeamLeader, Fire, Lightning]
    [ [ Skill.new
        { Skill.name      = "Leaf Hurricane"
        , Skill.desc      = "Guy delivers a powerful spinning kick to an enemy, dealing 30 damage. During [Sixth Gate Opening], this skill becomes [Severe Leaf Hurricane][t][r]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 30 ]
        }
      , Skill.new
        { Skill.name      = "Severe Leaf Hurricane"
        , Skill.desc      = "Guy delivers a spinning kick with enough force behind it to start a whirlwhind, dealing 60 piercing damage to an enemy."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 60 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sixth Gate Opening"
        , Skill.desc      = "Guy loses 40 health down to a minimum of 1 and becomes invulnerable for 2 turns."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                sacrifice 1 40
                apply 2 [ Invulnerable All
                        , Alternate "Leaf Hurricane" "Severe Leaf Hurricane"
                        ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Counter Punch"
        , Skill.desc      = "Guy singles out an enemy. If the target uses a skill on Guy or his allies next turn, they will be countered and receive 30 damage."
        , Skill.classes   = [Physical, Melee, Invisible]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 1 (Countered All) $ damage 30 ]
        }
      ]
    , [ invuln "Dodge" "Guy" [Physical] ]
    ]
  , Character
    "Baki"
    "A jōnin from the Hidden Sand Village, Baki is a ruthlessly efficient squad leader. He cloaks his attacks in illusions to prevent his enemies from defending against them."
    [SandVillage, Jonin, TeamLeader, Wind]
    [ [ Skill.new
        { Skill.name      = "Sudden Strike"
        , Skill.desc      = "Baki performs a blindingly fast attack concealed by decoy illusory trails, dealing 20 piercing damage to an enemy."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemy $ pierce 20 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wind Sword"
        , Skill.desc      = "A maelstrom of stabbing wind deals 40 piercing damage to an enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen, Nin]
        , Skill.effects   =
          [ To Enemy $ pierce 40 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flak Jacket"
        , Skill.desc      = "Using a top-of-the-line Hidden Sand Village flak jacket, Baki provides 50 permanent destructible defense to himself or an ally. While the target has destructible defense from this skill, they ignore harmful status effects."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ To Ally do
                apply 4 [Enrage]
                defend 4 50
                onBreak'
          ]
        }
      ]
    , [ invuln "Teleport" "Baki" [Chakra] ]
    ]
  , Character
    "Shizune"
    "A jōnin from the Hidden Leaf Village, Shizune is a talented medical-nin apprenticed to Tsunade. She is agile and fast, fully capable of holding her own in a fight whenever she isn't healing one of her allies."
    [LeafVillage, AlliedForces, Jonin]
    [ [ Skill.new
        { Skill.name      = "Poison Needle Volley"
        , Skill.desc      = "Shizune shoots hidden needles at an enemy, dealing 15 damage."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy $ damage 15 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Poison Fog"
        , Skill.desc      = "Shizune spews forth a toxic cloud to poison an enemy, causing them to receive 10 affliction damage for 3 turns. Cannot be used on an enemy already affected by [Poison Fog]."
        , Skill.require   = HasU 0 "Poison Fog"
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 3 [Afflict 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Regenerative Healing Technique"
        , Skill.desc      = "Using a notoriously difficult healing technique, Shizune restores 35 health to herself or an ally and cures the target of baneful effects."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly do
                cureBane
                heal 35
          ]
        }
      ]
    , [ invuln "Dodge" "Shizune" [Physical] ]
    ]
  ]
