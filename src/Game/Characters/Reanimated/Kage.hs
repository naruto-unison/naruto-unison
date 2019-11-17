{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated.Kage (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Category -> Character]
characters =
  [ Character
    "Hashirama Senju"
    "Reanimated by Orochimaru, Hashirama was the founder of the Hidden Leaf Village and its first Hokage. His unique ability to manipulate wood allows him give life to trees, which protect his allies and impair his enemies."
    [LeafVillage, Kage, Sage, Earth, Water, Fire, Wind, Lightning, Yin, Yang, Senju]
    [ [ Skill.new
        { Skill.name      = "Tree Wave Destruction"
        , Skill.desc      = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ damage 10
          , To Allies $ defend 0 5
          ]
        }
      , Skill.new
        { Skill.name      = "Tree Wave Destruction "
        , Skill.desc      = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemies $ damage 10
          , To Allies $ defend 0 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tree Strangulation"
        , Skill.desc      = "A tree sprouts from the ground and snares an enemy in its branches, dealing 25 damage and stunning their physical and chakra skills for 1 turn. Stuns all skills during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy do
              damage 25
              has <- userHas "Deep Forest Creation"
              apply 1 if has then [Stun All] else [Stun Physical, Stun Chakra]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Deep Forest Creation"
        , Skill.desc      = "The battlefield transforms into a forest. For 2 turns, enemy cooldowns are increased by 1 turn and the cost of enemy non-mental skills is increased by 1 arbitrary chakra. While active, this skill becomes [Deep Forest Flourishing][b][b]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Enemies $ apply 2 [Snare 1, Exhaust [NonMental]]
          , To Self $ apply 2
                [ Alternate "Tree Wave Destruction" "Tree Wave Destruction "
                , Alternate "Deep Forest Creation" "Deep Forest Flourishing"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Deep Forest Flourishing"
        , Skill.desc      = "Provides 30 permanent destructible defense to Hashirama's team and resets their cooldowns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Allies do
                defend 0 30
                resetAll
          ]
        }
      ]
    , [ invuln "Parry" "Hashirama" [Physical] ]
    ]
    150
  , Character
    "Tobirama Senju"
    "Reanimated by Orochimaru, Hashirama was the second Hokage. His water-manipulating skills flood the battlefield, impairing and harming the enemy team."
    [LeafVillage, Kage, Sensor, TeamLeader, Water, Fire, Wind, Lightning, Earth, Yin, Yang, Senju]
    [ [ Skill.new
        { Skill.name      = "Water Prison"
        , Skill.desc      = "Water surrounds an enemy, dealing 15 damage and making them ignore helpful effects for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 15 `bonusIf` channeling "Water Shockwave"
                damage (15 + bonus)
                apply 1 [Seal]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Shockwave"
        , Skill.desc      = "A giant wave of water floods the enemy team for 3 turns, dealing 15 damage, stunning their bane skills, and increasing the damage of [Water Prison] by 15."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen, Nin]
        , Skill.cooldown  = 3
        , Skill.dur       = Ongoing 3
        , Skill.effects   =
          [ To Enemies do
                damage 15
                apply 1 [Stun Bane]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Infinite Darkness"
        , Skill.desc      = "Tobirama plunges the battlefield into darkness, making his team invulnerable to physical and mental skills for 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Allies $ apply 1 [Invulnerable Physical, Invulnerable Mental] ]
        }
      ]
    , [ invuln "Water Wall" "Tobirama" [Physical] ]
    ]
    150
  , Character
    "Minato Namikaze"
    "Reanimated by Orochimaru, Minato was the fourth Hokage. Free at last from the Reaper's eternal torment, Minato's soul carries the Yin half of Kurama, the nine-tailed beast. On his own, he was renowned as a legendary fighter and the fastest man in history; as an immortal jinchūriki, he is a threat unto gods."
    [LeafVillage, Kage, Jinchuriki, Sage, Sensor, TeamLeader, Fire, Wind, Lightning, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Space-Time Marking"
        , Skill.desc      = "Minato opportunistically marks targets to use as teleport destinations for avoiding attacks. Allies who do not use skills this turn and enemies who do not use skills next turn will be marked for 4 turns. Minato gains 5 points of damage reduction for each marked target. This skill stacks."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAllies $ trap -1 OnNoAction do
                applyWith [Invisible] 4 []
                self $ applyWith [Invisible] 4 [Reduce [All] Flat 5]
          , To Enemies $ trap -1 OnNoAction do
                applyWith [Invisible] -4 []
                self $ applyWith [Invisible] -4 [Reduce [All] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Reciprocal Round-Robin"
        , Skill.desc      = "Minato prepares to switch places with an ally. If enemies use skills on Minato next turn, the skills will be reflected onto the target, and Minato's skills next turn will bypass invulnerability, counters, reflects, and damage reduction. If enemies use skills on the target next turn, the skills will be reflected onto Minato, and the target's skills next turn will bypass invulnerability, counters, reflects, and damage reduction."
        , Skill.classes   = [Chakra, Ranged, Invisible, Unremovable]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To XAlly do
                userSlot   <- user slot
                targetSlot <- target slot
                apply -1 [Redirect userSlot]
                trap -1 (OnHarmed All) $ self $
                    apply' "Round-Robin Surprise Attack" -1
                    [AntiCounter, Bypass, Pierce]
                self do
                    apply -1 [Redirect targetSlot]
                    trap -1 (OnHarmed All) $ withTarget targetSlot $
                        apply' "Round-Robin Surprise Attack" -1
                        [AntiCounter, Bypass, Pierce]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rasen-Flash Super-Circle Dance Howl (Stage III)"
        , Skill.desc      = "Minato teleports behind an enemy and slams a Rasengan orb into them, dealing 20 damage. Wreathed in yellow lightning, he teleports in quick succession to each enemy affected by [Space-Time Marking], dealing 20 damage for every stack of [Space-Time Marking] on them."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Enemies do
                stacks <- targetStacks "Space-Time Marking"
                damage (20 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra-Arm Raijin"
        , Skill.desc      = "Minato and allies affected by [Space-Time Marking] become invulnerable for 1 turn."
        , Skill.require   = HasU 1 "Space-Time Marking"
        , Skill.classes   = [Chakra, Bypassing]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Allies $ apply 1 [Invulnerable All] ]
        }
      ]
    ]
    150
  , Character
    "Rasa"
    "Reanimated by Kabuto, Rasa was the fourth Kazekage of the Hidden Sand Village and the father of the Sand Siblings. Cold and calculating, Rasa buries his enemies beneath crushingly heavy gold dust that they must fight their way out of to survive."
    [SandVillage, Kabuto, Kage, Wind, Earth, Water, Yin, SandClan]
    [ [ Skill.new
        { Skill.name      = "Magnet Technique"
        , Skill.desc      = "Waves of gold flood the enemy team, dealing 10 damage and applying 10 permanent destructible barrier to them. The skills of enemies who have destructible barrier from this skill cost 1 additional arbitrary chakra. Applies 5 additional destructible barrier to targets with destructible barrier from [Gold Dust Waterfall]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                damage 10
                bonus <- 5 `bonusIf` targetHas' barrier "Gold Dust Waterfall"
                barricade' 0 (const $ return ()) (apply 1 [Exhaust [All]])
                    (10 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gold Dust Waterfall"
        , Skill.desc      = "A towering tidal wave of gold slams down on an enemy, dealing 35 damage and applying 30 permanent destructible barrier."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                damage 35
                barricade 0 30
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "24-Karat Barricade"
        , Skill.desc      = "Rasa constructs a golden blockade in front of an enemy. If they use a skill on Rasa or his allies next turn, it will be countered and they will gain 20 permanent destructible barrier. Applies 10 additional destructible barrier if the target has destructible barrier from [Gold Dust Waterfall]."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 1 (Countered All) do
                bonus <- 10 `bonusIf` targetHas' barrier "Gold Dust Waterfall"
                barricade 0 (20 + bonus)
          ]
        }
      ]
    , [ invuln "Gold Dust Shield" "Rasa" [Physical] ]
    ]
    150
  , Character
    "A"
    "Reanimated by Kabuto, A was the third Raikage of the Hidden Cloud Village. His legendary resilience and fortitude earned him the title of Strongest Shield."
    [CloudVillage, Kabuto, Kage, Lightning, Earth, Fire]
    [ [ Skill.new
        { Skill.name      = "Piercing Four-Fingered"
        , Skill.desc      = "A switches to his four-fingered style, increasing the damage of [Lightning Straight] by 5. For the rest of the game, enemies who use skills on A or his allies will be marked for 1 turn. If A uses [Lightning Straight] on a marked target, they will be stunned for 1 turn and immune to being marked for 4 turns. Once used, this skill becomes [Three-Fingered Assault][r][r]."
        , Skill.classes   = [Chakra, Melee, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Allies $ trapFrom 0 (OnHarmed All) $ tag 1
          , To Self $ hide 0
                [Alternate "Piercing Four-Fingered" "Three-Fingered Assault"]
          ]
        }
      , Skill.new
        { Skill.name      = "Three-Fingered Assault"
        , Skill.desc      = "A switches to his three-fingered style, increasing the damage of [Lightning Straight] by 5. For the rest of the game, whenever A is damaged, the cooldown of [Lightning Armor] decreases by 1 turn. Once used, this skill becomes [One-Fingered Assault][r][r][r]."
        , Skill.classes   = [Chakra, Melee, Unremovable]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Self do
                trap 0 (OnDamaged All) $ alterCd "Lightning Armor" -1
                hide 0
                    [Alternate "Piercing Four-Fingered" "One-Fingered Assault"]
          ]
        }
      , Skill.new
        { Skill.name      = "One-Fingered Assault"
        , Skill.desc      = "A switches to his one-fingered style, increasing the damage of [Lightning Straight] by 5 and becoming invulnerable to affliction damage. Enemies who are stunned by [Lightning Straight] will only be immune to marking for 3 turns."
        , Skill.classes   = [Chakra, Melee, Unremovable]
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Self do
                hide' "finger" 0 []
                apply 0 [Invulnerable Affliction]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lightning Armor"
        , Skill.desc      = "An electric field envelops A and supercharges his nervous system. The field deflects attacks for 3 turns, reducing all non-affliction damage A receives to 10 at most."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 8
        , Skill.effects   =
          [ To Self $ apply 3 [Limit 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lightning Straight"
        , Skill.desc      = "A rushes an opponent with lightning speed and strikes them with stiffened fingers, dealing 20 damage. If this skill deals damage, the cooldown of [Lightning Armor] will decrease by 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Self $ trap' -1 OnDamage $ alterCd "Lightning Armor" -1
          , To Enemy do
                bonus4 <- 5 `bonusIf` userHas "piercing four-fingered"
                bonus3 <- 5 `bonusIf` userHas "three-fingered assault"
                bonus1 <- 5 `bonusIf` userHas "One-Fingered Assault"
                damage (20 + bonus4 + bonus3 + bonus1)
                unlessM (targetHas "Aftershocks") do
                    apply 1 [Stun All]
                    bonus <- 1 `bonusIf` userHas "One-Fingered Assault"
                    tag' "Aftershocks" (4 - bonus)
          ]
        , Skill.changes   =
            \n x ->
              x { Skill.desc = "A rushes an opponent with lightning speed and strikes them with stiffened fingers, dealing " ++ tshow (20 + 5 * numActive "finger" n) ++ " damage. If this skill deals damage, the cooldown of [Lightning Armor] decreases by 1 additional turn." }
        }
      ]
    , [ invuln "Strongest Shield" "A" [Physical] ]
    ]
    150
  , Character
    "Mū"
    "Reanimated by Kabuto, Mū was the second Tsuchikage of the Hidden Stone Village. Unfailingly polite, he intends to ensure that his village benefits from the war. By manipulating matter at the atomic level, he disintegrates the defenses of his enemies."
    [StoneVillage, Kabuto, Kage, Sensor, Earth, Water, Fire, Wind, Lightning, Yang]
    [ [ Skill.new
        { Skill.name      = "Particle Beam"
        , Skill.desc      = "A ray of high-energy atomic particles blasts an enemy, dealing 25 piercing damage. Deals 10 additional damage if the target is invulnerable. Costs 1 ninjutsu chakra during [Fragmentation]."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` target invulnerable
                pierce (25 + bonus)
          ]
        , Skill.changes   =
            changeWith "Fragmentation" \x -> x { Skill.cost = [Nin] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fragmentation"
        , Skill.desc      = "Mū's body undergoes fission and splits into two. For 2 turns, Mū ignores stuns and disabling effects, and damage he receives is reduced by half. While active, Mū's damage is weakened by 5. If Mū's health reaches 0 during this skill, he regains 15 health and this skill ends."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                apply 2 [ Focus
                        , Reduce [All] Percent 50
                        , Weaken [All] Flat 5
                        ]
                trap 2 OnRes do
                    setHealth 15
                    remove      "Fragmentation"
                    removeTrap "Fragmentation"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Atomic Dismantling"
        , Skill.desc      = "The atomic bonds within an enemy shatter, dealing 40 piercing damage and demolishing their destructible defense and his own destructible barrier. Costs 1 ninjutsu chakra and 1 arbitrary chakra during [Fragmentation]."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = [Nin, Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                pierce 40
          ]
        , Skill.changes   =
            changeWith "Fragmentation" \x -> x { Skill.cost = [Nin, Rand] }
        }
      ]
    , [ invuln "Dustless Bewildering Cover" "Mū" [Chakra] ]
    ]
    150
  , Character
    "Gengetsu Hōzuki"
    "Reanimated by Kabuto, Gengetsu was the second Mizukage of the Hidden Mist Village. Charismatic and carefree, he cheerfully offers tips to his opponents on how to beat him. He is especially fond of one-on-one duels."
    [MistVillage, Kabuto, Kage, Water, Fire, Lightning, Earth, Yin, Yang, Hozuki]
    [ [ Skill.new
        { Skill.name      = "Major Summoning: Giant Clam"
        , Skill.desc      = "Gengetsu summons a huge clam that exudes illusory mist for 4 turns. Each turn, a random member of his team becomes a mirage, reflecting the first skill an enemy uses on them next turn, and a random member of his team gains 80 destructible defense for 1 turn. If the clam's destructible defense is destroyed, this skill is canceled."
        , Skill.classes   = [Summon]
        , Skill.cost      = [Nin, Gen, Rand]
        , Skill.cooldown  = 5
        , Skill.dur       = Ongoing 4
        , Skill.effects   =
          [ To RAlly $ apply 1 [Reflect]
          , To RAlly do
                defend 1 80
                onBreak'
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Pistol"
        , Skill.desc      = "Gengetsu fires a drop of water like a bullet at an enemy, dealing 10 piercing damage and killing them if their health drops to 10 or lower. Deals 10 additional damage and bypasses invulnerability during [Major Summoning: Giant Clam]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` channeling "Major Summoning: Giant Clam"
                pierce (10 + bonus)
                targetHealth <- target health
                when (targetHealth <= 10) kill
          ]
        , Skill.changes   =
            changeWithChannel "Major Summoning: Giant Clam" \x ->
                x { Skill.classes = insertSet Bypassing $ Skill.classes x }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Steaming Danger Tyranny Boy"
        , Skill.desc      = "Gengetsu isolates an enemy by repeatedly blasting the rest of their team back with a childlike figure of himself. For 2 turns, Gengetsu and his target are invulnerable to everyone else and cannot use skills on anyone else. At the start of the duel, both participants have their health set to 30. When the duel ends, they are restored to their health before the duel if still alive."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Unremovable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                userSlot     <- user slot
                targetSlot   <- target slot
                userHealth   <- user health
                targetHealth <- target health
                bomb 2 [Duel userSlot, Taunt userSlot]
                       [ To Expire $ setHealth targetHealth ]
                setHealth 30
                self do
                    bomb 2 [Duel targetSlot, Taunt targetSlot]
                           [ To Expire $ setHealth userHealth ]
                    setHealth 30
          ]
        }
      ]
    , [ invuln "Mirage" "Gengetsu" [Mental] ]
    ]
    150
  , Character
    "Hanzō"
    "Reanimated by Kabuto, Hanzō the Salamander was the leader of the Hidden Rain Village. In combination with his unrivaled combat prowess, the lethal venom sac implanted in his body makes him a feared legend throughout the world."
    [RainVillage, Kabuto, Kage, Fire]
    [ [ Skill.new
        { Skill.name      = "Major Summoning: Ibuse"
        , Skill.desc      = "Hanzō summons his fabled salamander to the battlefield. Ibuse starts with 30 health and redirects half of all damage against Hanzō to itself until it dies. While active, this skill becomes [Poison Fog][b][b]."
        , Skill.classes   = [Summon, Unreflectable, Unremovable]
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ To Self do
                has <- userHas "Venom Sac"
                if has then do
                    remove "Venom Sac"
                    alterCd "Major Summoning: Ibuse" -2
                else do
                    hide 0 [Reduce [Affliction] Percent 50]
                    applyStacks "Major Summoning: Ibuse" 30
                        [Alternate "Major Summoning: Ibuse" "Poison Fog"]
                    trapPer' 0 PerDamaged \i -> do
                        stacks <- userStacks "Major Summoning: Ibuse"
                        if stacks - i > 0 then
                            removeStacks "Major Summoning: Ibuse" i
                        else do
                            remove "Major Summoning: Ibuse"
                            remove "major summoning: ibuse"
                            removeTrap "Major Summoning: Ibuse"
                            cancelChannel "Poison Fog"
                            sacrifice 0 (i - stacks)
          ]
        }
      , Skill.new
        { Skill.name      = "Poison Fog"
        , Skill.desc      = "Ibuse opens its mouth to reveal a noxious cloud of deadly poison, dealing 10 affliction damage to all enemies until Ibuse dies. Cannot be used while active."
        , Skill.require   = HasI 0 "Poison Fog"
        , Skill.classes   = [Physical, Bane, Ranged, Unreflectable]
        , Skill.cost      = [Blood, Blood]
        , Skill.dur       = Ongoing 0
        , Skill.effects   =
          [ To Enemies $ afflict 10 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sickle Dance"
        , Skill.desc      = "Hanzō gouges an enemy with his sickle, dealing 15 piercing damage to them immediately and 5 affliction damage for 2 turns. During [Major Summoning: Ibuse], Ibuse swallows the target, stunning their non-mental skills for 1 turn and dealing 10 additional affliction damage."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                apply 2 [Afflict 5]
                whenM (userHas "Major Summoning: Ibuse") do
                    afflict 10
                    apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Venom Sac"
        , Skill.desc      = "The first enemy to use a non-mental skill on Hanzō next turn will rupture his implanted venom sac, taking 20 affliction damage every turn and causing Hanzō to take 10 affliction damage every turn. When Hanzō summons Ibuse or if [Major Summoning: Ibuse] is already active, Hanzō will replace his venom sac with Ibuse's, ending [Major Summoning: Ibuse], curing himself of this skill, and decreasing the current cooldown of [Major Summoning: Ibuse] by 3."
        , Skill.classes   = [Physical, Bane, Invisible]
        , Skill.cost      = [Blood]
        , Skill.effects   =
            [ To Self $ trapFrom 1 (OnHarmed NonMental) do
                  apply 0 [Afflict 20]
                  self $ removeTrap "Venom Sac"
                  has <- userHas "major summoning: ibuse"
                  if has then self do
                      remove "Major Summoning Ibuse"
                      remove "major summoning: ibuse"
                      alterCd "Major Summoning: Ibuse" -2
                      cancelChannel "Poison Fog"
                  else self $
                      apply 0 [Afflict 10]
            ]
        }
      ]
    , [ invuln "Block" "Hanzō" [Physical] ]
    ]
    200
  ]
