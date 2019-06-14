{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Reanimated (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Hashirama Senju"
    "Reanimated by Orochimaru, Hashirama was the founder of the Hidden Leaf Village and its first Hokage. His unique ability to manipulate wood allows him give life to trees, which protect his allies and impair his enemies."
    [ [ Skill.new
        { Skill.name      = "Tree Wave Destruction"
        , Skill.desc      = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies $ damage 10
          , p Allies  $ defend 0 5
          ]
        }
      , Skill.new
        { Skill.name      = "Tree Wave Destruction"
        , Skill.desc      = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.varicd    = True
        , Skill.effects   =
          [ p Enemies $ damage 10
          , p Allies  $ defend 0 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tree Strangulation"
        , Skill.desc      = "A tree sprouts from the ground and snares an enemy in its branches, dealing 25 damage and stunning their physical and chakra skills for 1 turn. Stuns all skills during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Blood, Rand]
        , Skill.effects   =
          [ p Enemy do
              damage 25
              has <- userHas "Deep ForestCreation"
              apply 1 if has then [Stun All] else [Stun Physical, Stun Chakra]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Deep Forest Creation"
        , Skill.desc      = "The battlefield transforms into a forest. For 2 turns, enemy cooldowns are increased by 1 and the cost of enemy non-mental skills is increased by 1 random chakra. While active, this skill becomes [Deep Forest Flourishing][b][b]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Blood, Blood]
        , Skill.effects   =
          [ p Enemies $ apply 2 [Snare 1, Exhaust NonMental]
          ,  p Self do
                tag 2
                vary' 2 "Tree Wave Destruction" "Tree Wave Destruction"
                vary' 2 "Deep Forest Creation" "Deep Forest Flourishing"
          ]
        }
      , Skill.new
        { Skill.name      = "Deep Forest Flourishing"
        , Skill.desc      = "Provides 30 permanent destructible defense to Hashirama's team and resets their cooldowns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Blood, Blood]
        , Skill.effects   =
          [ p Allies do
                defend 0 30
                resetAll
          ]
        }
      ]
    , [ invuln "Parry" "Hashirama" [Physical] ]
    ] []
  , Character
    "Tobirama Senju"
    "Reanimated by Orochimaru, Hashirama was the second Hokage. His water-manipulating skills flood the battlefield, impairing and harming the enemy team."
    [ [ Skill.new
        { Skill.name      = "Water Prison"
        , Skill.desc      = "Water surrounds an enemy, dealing 15 damage and making them immune to effects from allies for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 15 `bonusIf` channeling "Water Shockwave"
                damage (15 + bonus)
                apply 1 [Seal]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Shockwave"
        , Skill.desc      = "A giant wave of water floods the enemy team for 3 turns, dealing 15 damage, negating their affliction damage, and increasing the damage of [Water Prison] by 15."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Gen, Nin]
        , Skill.cooldown  = 3
        , Skill.channel   = Ongoing 3
        , Skill.effects   =
          [ p Enemies do
                damage 15
                apply 1 [Stun Affliction]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Infinite Darkness"
        , Skill.desc      = "Tobirama plunges the battlefield into darkness, making his team invulnerable to all harmful physical and mental skills for 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Allies $ apply 1 [Invulnerable Physical, Invulnerable Mental] ]
        }
      ]
    , [ invuln "Water Wall" "Tobirama" [Physical] ]
    ] []
  , Character
    "Hanzō"
    "Reanimated by Kabuto, Hanzō the Salamander was the leader of Amegakure. In combination with his unrivaled combat prowess, the lethal venom sac implanted in his body makes him a feared legend throughout the world."
    [ [ Skill.new
        { Skill.name      = "Major Summoning: Ibuse"
        , Skill.desc      = "Hanzō summons his fabled salamander to the battlefield. Ibuse starts with 30 health and redirects half of all damage against Hanzō to itself until it dies. While active, this skill becomes [Poison Fog][b][b]."
        , Skill.classes   = [Chakra, Summon, Unreflectable, Unremovable]
        , Skill.cost      =  k [Rand, Rand, Rand]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ p Self do
                has <- userHas "Venom Sac"
                if has then do
                    remove "Venom Sac"
                    alterCd 0 0 (-2)
                else do
                    hide' "Ibuse" 0 [Reduce Affliction Percent 50]
                    addStacks "Major Summoning: Ibuse" 30
                    vary "Major Summoning: Ibuse" "Poison Fog"
                    trapPer' 0 PerDamaged $
                        removeStacks "Major Summoning: Ibuse"
                    trap' 0 (OnDamaged All) $
                        unlessM (userHas "Major Summoning: Ibuse") do
                            remove "Ibuse"
                            removeTrap "Ibuse"
                            vary "Major Summoning: Ibuse" baseVariant
                            cancelChannel "Poison Fog"
          ]
        }
      , Skill.new
        { Skill.name      = "Poison Fog"
        , Skill.desc      = "Ibuse opens its mouth to reveal a noxious cloud of deadly poison, dealing 10 affliction damage to all enemies until Ibuse dies. Cannot be used while active."
        , Skill.classes   = [Ranged, Single, Unreflectable]
        , Skill.cost      = k [Blood, Blood]
        , Skill.channel   = Ongoing 0
        , Skill.effects   =
          [ p Enemies $ afflict 10 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sickle Dance"
        , Skill.desc      = "Hanzō slashes an enemy with his sickle, dealing 15 piercing damage to them immediately and 5 affliction damage for 2 turns. During [Major Summoning: Ibuse], Ibuse swallows the target, stunning their non-mental skills for 1 turn and dealing 10 additional affliction damage."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                pierce 15
                apply 2 [Afflict 15]
                whenM (userHas "Major Summoning: Ibuse") do
                    afflict 10
                    apply 1 [Stun All]
          ]
        , Skill.changes   = changeWith "Major Summoning: Ibuse"
                  \_ skill -> skill { Skill.pic = True }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Venom Sac"
        , Skill.desc      = "The first enemy to use a non-mental skill on Hanzō next turn will rupture his implanted venom sac, taking 20 affliction damage every turn and causing Hanzō to take 10 affliction damage every turn. When Hanzō summons Ibuse or if [Major Summoning: Ibuse] is already active, Hanzō will replace his venom sac with Ibuse's, ending [Major Summoning: Ibuse], curing himself of this skill, and decreasing the current cooldown of [Major Summoning: Ibuse] by 3."
        , Skill.classes   = [Bane, InvisibleTraps]
        , Skill.cost      = k [Blood]
        , Skill.effects   =
            [ p Self $ trapFrom (-1) (OnHarmed NonMental) do
                  apply 0 [Afflict 20]
                  has <- userHas "Ibuse"
                  if has then self do
                      remove "Major Summoning Ibuse"
                      vary "Major Summoning: Ibuse" baseVariant
                      alterCd 0 0 (-2)
                      cancelChannel "Poison Fog"
                  else self $
                      apply 0 [Afflict 20]
            ]
        }
      ]
    , [ invuln "Block" "Hanzō" [Physical] ]
    ] []
  , Character
    "Gengetsu Hōzuki"
    "Reanimated by Kabuto, Gengetsu was the second Mizukage of the Hidden Mist Village. Charismatic and carefree, he cheerfully offers tips to his opponents on how to beat him. He is especially fond of one-on-one duels."
    [ [ Skill.new
        { Skill.name      = "Major Summoning: Giant Clam"
        , Skill.desc      = "Gengetsu summons a huge clam that exudes illusory mist for 4 turns. Each turn, a random member of his team becomes a mirage, reflecting the first harmful skill used on them next turn, and a random member of his team gains 80 destructible defense for 1 turn. If the clam's destructible defense is destroyed, this skill is canceled."
        , Skill.classes   = [Chakra, Summon]
        , Skill.cost      = k [Nin, Gen, Rand]
        , Skill.channel   = Ongoing 4
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ p RAlly $ apply 1 [Reflect]
          , p RAlly do
                defend 1 80
                onBreak'
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Pistol"
        , Skill.desc      = "Gengetsu fires a drop of water like a bullet at an enemy, dealing 10 piercing damage and killing them if their health drops to 10 or lower. Deals 10 additional damage and bypasses invulnerability during [Major Summoning: Giant Clam]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 10 `bonusIf` userHas "Major Summoning: Giant Clam"
                pierce (10 + bonus)
                targetHealth <- target health
                when (targetHealth <= 10) kill
          ]
        , Skill.changes   = changeWith "Major Summoning: Giant Clam" $
                            addClass Bypassing
        }
      ]
    , [ Skill.new
        { Skill.name      = "Steaming Danger Tyranny Boy"
        , Skill.desc      = "Gengetsu isolates an enemy by repeatedly blasting the rest of their team back with a childlike figure of himself. For 2 turns, Gengetsu and his target are invulnerable to everyone else and cannot use skills on anyone else. At the start of the duel, both participants have their health set to 30. When the duel ends, they are restored to their health before the duel if still alive."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Unremovable]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy do
                userSlot     <- user slot
                targetSlot   <- target slot
                userHealth   <- user health
                targetHealth <- target health
                bomb 2 [Duel userSlot, Taunt userSlot] 
                       [ p Done $ setHealth targetHealth ]
                setHealth 30
                self do
                    bomb 2 [Duel targetSlot, Taunt targetSlot] 
                           [ p Done $ setHealth userHealth ]
                    setHealth 30
          ]
        }
      ]
    , [ invuln "Mirage" "Gengetsu" [Mental] ]
    ] []
  , Character
    "Mū"
    "Reanimated by Kabuto, Mū was the second Tsuchikage of the Hidden Rock Village. Unfailingly polite, he intends to ensure that his village benefits from the war. By manipulating matter at the atomic level, he disintegrates the defenses of his enemies."
    [ [ Skill.new
        { Skill.name      = "Particle Beam"
        , Skill.desc      = "A ray of high-energy atomic particles blasts an enemy, dealing 25 piercing damage. Deals 10 additional damage if the target is invulnerable. Deals 5 fewer damage and costs 1 ninjutsu chakra during [Fragmentation]."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemy do
                targetBonus <- 10 `bonusIf` target immune
                userBonus   <- (-5) `bonusIf` userHas "Fragmentation"
                pierce (25 + targetBonus + userBonus)
          ]
        , Skill.changes   = changeWith "Fragmentation" $ setCost [Nin]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Fragmentation"
        , Skill.desc      = "Mū's body undergoes fission and splits into two. For 2 turns, Mū ignores stuns and reduces damage against him by half. If Mū's health reaches 0 during this skill, he regains 15 health and this skill ends."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                apply 2 [Ignore Stun, Reduce Affliction Percent 50]
                trap 2 OnRes do
                    setHealth 15
                    remove      "Fragmentation"
                    removeTrap "Fragmentation"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Atomic Dismantling"
        , Skill.desc      = "The atomic bonds within an enemy shatter, dealing 40 piercing damage and demolishing their destructible defense and his own destructible barrier. Deals 5 fewer damage and costs 1 ninjutsu chakra and 1 random chakra during [Fragmentation]."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = k [Nin, Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                demolishAll
                bonus <- (-5) `bonusIf` userHas "Fragmentation"
                pierce (40 + bonus)
          ]
        , Skill.changes   = changeWith "Fragmentation" $ setCost [Nin, Rand]
        }
      ]
    , [ invuln "Dustless Bewildering Cover" "Mū" [Chakra] ]
    ] []
  , Character
    "A"
    "Reanimated by Kabuto, A was the third Raikage of the Hidden Cloud Village. His legendary resilience and fortitude earned him the title of Strongest Shield."
    [ [ Skill.new
        { Skill.name      = "Piercing Four-Fingered"
        , Skill.desc      = "A switches to his four-fingered style, increasing the damage of [Lightning Straight] by 5. For the rest of the game, any enemy who uses a harmful skill on A or his allies will be marked for 1 turn. If A uses [Lightning Straight] on a marked target, they will be stunned for 1 turn and immune to being marked for 4 turns. Once used, this skill becomes [Three-Fingered Assault][n]."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Allies $ trapFrom 0 (OnHarmed All) $ tag 1
          , p Self do
                hide' "finger" 0 []
                vary "Piercing Four-Fingered" "Three-Fingered Assault"
          ]
        }
      , Skill.new
        { Skill.name      = "Three-Fingered Assault"
        , Skill.desc      = "A switches to his three-fingered style, increasing the damage of [Lightning Straight] by 5. For the rest of the game, on any turn that A is damaged by an enemy, the cooldown of [Strongest Shield] will decrease by 1 additional turn. Once used, this skill becomes [One-Fingered Assault][n]."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Self do
                trap 0 (OnDamaged All) $ alterCd 1 0 (-1)
                hide' "finger" 0 []
                vary "Piercing Four-Fingered" "One-Fingered Assault"
          ]
        }
      , Skill.new
        { Skill.name      = "One-Fingered Assault"
        , Skill.desc      = "A switches to his one-fingered style, increasing the damage of [Lightning Straight] by 5. For the rest of the game, all damage against him—including piercing and affliction—is reduced by 10 points per dead ally. Enemies who are stunned by [Lightning Straight] will only be immune to marking for 3 turns."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = k [Nin]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p XAllies $ trap 0 OnDeath $
                self $ apply 0 [Reduce Affliction Flat 10]
          , p Self do
                hide' "finger" 0 []
                allies $ whenM (target $ not . alive) $
                    self $ apply 0 [Reduce Affliction Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Strongest Shield"
        , Skill.desc      = "Shrugging off injuries that would overwhelm any other fighter, A regains all health he lost from damage in the past 2 turns and heals himself for the same amount. For 2 turns, his health cannot drop below 1 and all damage against him—including piercing and affliction—is reduced by 20."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 8
        , Skill.effects   =
          [ p Self do
                stacks <- userStacks "Cracks in the Shield"
                heal stacks
                apply 2 [Endure, Reduce Affliction Flat 20]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lightning Straight"
        , Skill.desc      = "A rushes an opponent with lightning speed and strikes them with stiffened fingers, dealing 20 damage. If this skill deals damage, the cooldown of [Strongest Shield] decreases by 1 additional turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Self $ trap' (-1) OnDamage $ alterCd 1 0 (-1)
          , p Enemy do
                stacks <- userStacks "finger"
                damage (20 + 5 * stacks)
                unlessM (targetHas "Aftershocks") do
                    apply 1 [Stun All]
                    tag' "Aftershocks" 4
          ]
        , Skill.changes   = \n skill -> skill
            { Skill.desc = "A rushes an opponent with lightning speed and strikes them with stiffened fingers, dealing " ++ tshow (20 + 5 * numActive "finger" n) ++ " damage. If this skill deals damage, the cooldown of [Strongest Shield] decreases by 1 additional turn." }
        }
      ]
    , [ invuln "Dodge" "A" [Physical] ]
    ] [ (PerDamaged, addOwnStacks (-2) "Cracks in the Shield" 1 0) ]
  , Character
    "Rasa"
    "Reanimated by Kabuto, Rasa was the fourth Kazekage of the Hidden Sand Village and the father of the Sand Siblings. Cold and calculating, Rasa buries his enemies beneath crushingly heavy gold dust that they must fight their way out of to survive."
    [ [ Skill.new
        { Skill.name      = "Magnet Technique"
        , Skill.desc      = "Waves of gold flood the enemy team, dealing 10 damage to them and applying 10 permanent destructible barrier to each. The skills of enemies who have destructible barrier from this skill cost 1 additional random chakra."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies do
                damage 10
                bonus <- 10 `bonusIf` targetHas "Gold Dust Waterfall"
                barrierDoes 0 (const $ return ()) (apply 1 [Exhaust All])
                    (10 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gold Dust Waterfall"
        , Skill.desc      = "A towering tidal wave of gold slams down on an enemy, dealing 35 damage and applying 30 permanent destructible barrier. Next turn, [Gold Dust Wave] and [24-Karat Barricade] will apply twice as much destructible barrier to them."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                damage 35
                barrier 0 30
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "24-Karat Barricade"
        , Skill.desc      = "Rasa constructs a golden blockade in front of an enemy. If they use a harmful skill next turn, it will be countered and they will gain 20 permanent destructible barrier."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy $ trap 1 (OnCounter All) do
                bonus <- 20 `bonusIf` targetHas "Gold Dust Waterfall"
                barrier 0 (20 + bonus)
          ]
        }
      ]
    , [ invuln "Gold Dust Shield" "Rasa" [Physical] ]
    ] []
    , Character
      "Jirōbō"
      "Reanimated by Kabuto, Jirōbō was a member of the Sound Five. No longer concealing his anger beneath a facade of politeness, Jirōbō has only one thing on his mind: revenge."
      [ [ Skill.new
          { Skill.name      = "Rivalry"
          , Skill.desc      = "Jirōbō picks out an enemy as his rival. If they use a harmful skill next turn, they will be countered and permanently forced to target Jirōbō. Effect ends if Jirōbō uses a skill on a different enemy or uses this skill again. Cannot be used during [Summoning: Earth Prison Golem]."
          , Skill.require   = HasI (-1) "Summoning: Earth Prison Golem"
          , Skill.classes   = [Mental, Melee, InvisibleTraps]
          , Skill.cost      = k [Rand]
          , Skill.cooldown  = 3
          , Skill.effects   =
            [ p Enemy do
                  everyone $ remove "Rivalry"
                  userSlot <- user slot
                  trap (-1) (OnCounter All) $ apply 0 [Taunt userSlot]
            ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Sphere of Graves"
          , Skill.desc      = "Jirōbō lifts the ground up and hurls it forward, dealing 30 damage to an enemy and gaining a Scattered Rock. Costs one taijutsu chakra if [Earth Dome Prison] affected any enemies last turn."
          , Skill.classes   = [Physical, Ranged]
          , Skill.cost      = k [Tai, Rand]
          , Skill.effects   =
            [ p Enemy do
                  damage 30
                  unlessM (targetHas "Rivalry") . everyone $ remove "Rivalry"
            , p Self $ apply' "Scattered Rock" 0 []
            ]
          , Skill.changes   = changeWith "Earth Dome Prison" $ setCost [Tai]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Earth Dome Prison"
          , Skill.desc      = "Jirōbō encases an enemy in chakra-conductive rock and drains their energy, dealing 20 affliction damage. If this skill is used on the target of [Rivalry], the damage drains their health and adds it to Jirobo's health."
          , Skill.classes   = [Chakra, Melee]
          , Skill.cost      = k [Nin, Rand]
          , Skill.effects   =
            [ p Enemy do
                  has <- targetHas "Rivalry"
                  if has then leech 20 (self . heal)
                  else do
                      afflict 20
                      everyone $ remove "Rivalry"
                  self $ tag 1
            ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Summoning: Earth Prison Golem"
          , Skill.desc      = "Jirōbō spends two Scattered Rocks to summon a golem, gaining 35 destructible defense for 2 turns. While Jirōbō has destructible defense from this skill, all enemies are his Rivals and can only target him. The first enemy to use a skill on him each turn is instantly affected by [Earth Dome Prison]."
          , Skill.require   = HasI 2 "Scattered Rock"
          , Skill.classes   = [Summon]
          , Skill.cost      = k [Rand, Rand]
          , Skill.cooldown  = 4
          , Skill.effects   =
            [ p Self do
                  userSlot <- user slot
                  enemies do
                      remove "Rivalry"
                      apply' "Rivalry" 2 [Taunt userSlot]
                  removeStacks "Scattered Rock" 2
                  defend 2 35
                  trapFrom 2 (OnHarmed All) do
                      leech 20 (self . heal)
                      self $ tag' "Earth Dome Prison" 1
                  onBreak $ everyone do
                      remove "Rivalry"
                      removeTrap "Summoning: Earth Prison Golem"
            ]
          }
        ]
      ] []
    , Character
    "Haku"
    "Reanimated by Kabuto, Haku remains as loyal to Zabuza as he was in life. With his inherited ice manipulation techniques, he disrupts his enemies while hiding safely behind crystalline mirrors."
    [ [ Skill.new
        { Skill.name      = "Thousand Needles of Death"
        , Skill.desc      = "Haku flings numerous ice needles outward, dealing 10 piercing damage to the enemy team. During [Crystal Ice Mirrors], this skill deals all 30 damage to a single enemy. If an enemy damaged by this skill loses at least 50 health during the same turn, they are stunned for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Blood]
        , Skill.effects   =
          [ p Enemies do
                pierce 10
                trapPer (-1) TrackDamaged \i ->
                    when (i >= 50) $ apply 1 [Stun All]
          ]
        , Skill.changes   = changeWith "Crystal Ice Mirrors" \_ skill -> skill
              { Skill.effects =
                [ p Enemy do
                      pierce 30
                      trapPer (-1) TrackDamaged \i ->
                          when (i >= 50) $ apply 1 [Stun All] ]
              }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Acupuncture"
        , Skill.desc      = "Haku alters the flow of energy in an enemy by sticking a needle into one of their vital points, disabling the non-damage effects of their skills for 2 turns. Bypasses invulnerability and targets all enemies during [Crystal Ice Mirrors]."
        , Skill.classes   = [Physical, Ranged, Single]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy $ apply 2 [Silence] ]
        , Skill.changes   = changeWith "Crystal Ice Mirrors" $
                            addClass Bypassing `also` targetAll
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crystal Ice Mirrors"
        , Skill.desc      = "Disorienting crystalline mirrors form all around the battlefield, providing 20 permanent destructible defense to Haku. For 3 turns, if Haku loses all destructible defense from this skill, he will gain destructible defense equal to how much health he lost during the same turn. Cannot be used while Haku still has destructible defense from this skill."
        , Skill.classes   = [Chakra, Single]
        , Skill.cost      = k [Blood, Nin]
        , Skill.cooldown  = 6
        , Skill.channel   = Ongoing 3
        , Skill.start     =
          [ p Self $ defend 0 20 ]
        }
      ]
    , [ invuln "Ice Dome" "Haku" [Chakra] ]
    ]
    [ (PerDamaged, \i n ->
        if | hasDefense "Crystal Ice Mirrors" (slot n) n -> n
           | not $ isChanneling "Crystal Ice Mirrors" n  -> n
           | otherwise -> addOwnDefense 0 "Crystal Ice Mirrors" i n)
    ]
  , Character
    "Zabuza Momochi"
    "Reanimated by Kabuto, Zabuza was one of the Seven Swordsmen of the Mist and a renowned mercenary. Although he has been reunited with Haku, Zabuza is furious at being forced to fight against his will. He still wields Kubikiribōchō, his legendary executioner's broadsword, which feeds on the blood it spills to strengthen itself."
    [ [ Skill.new
        { Skill.name      = "Demon Shroud"
        , Skill.desc      = "Demonic chakra pours out of Zabuza as he gives in to his bloodlust, gaining 10 points of damage reduction for 2 turns and ignoring stuns. Each turn, a random enemy is affected by [Executioner's Butchering]."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ p Self $ apply' "Demon Shroud " 1 [Reduce All Flat 10, Ignore Stun]
          , p REnemy do
                pierce 30
                tag' "Executioner's Butchering" 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Blood Harvest"
        , Skill.desc      = "Kubikiribōchō drinks up the blood it has spilled and uses the iron to reinforce itself, draining 10 health from a target affected by [Executioner's Butchering] to provide 10 permanent destructible defense. Extends the duration of [Demon Shroud] by 1 turn if active."
        , Skill.require   = HasU "Executioner's Butchering"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy $ leech 10 $ self . heal
          , p Self do
                defend 0 10
                prolongChannel 1 "Demon Shroud"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Executioner's Butchering"
        , Skill.desc      = "Zabuza's sword carves into an enemy like the edge of a guillotine, dealing 30 piercing damage and spilling their blood for 1 turn. Cannot be used during [Demon Shroud]."
        , Skill.require   = HasI (-1) "Demon Shroud"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                pierce 30
                tag 1
          ]
        }
      ]
    , [ invuln "Block" "Zabuza" [Physical] ]
    ] []
  , Character
    "Ameyuri Ringo"
    "Reanimated by Kabuto, Ameyuri was one of the Seven Swordsmen of the Mist. Wielding Baki, the legendary twin lightning blades, Ameyuri cuts down her enemies using paralyzing electricity."
    [ [ Skill.new
        { Skill.name      = "Lightning Fang"
        , Skill.desc      = "Bolts of lightning cascade across the battlefield, applying 2 turns of Electricity to all enemies. Whenever someone affected by Electricity uses a skill, Electricity on them is refreshed to its maximum duration, and everyone affected by Electricity receives 5 affliction damage that bypasses invulnerability. Reapplying Electricity extends its duration instead of stacking."
        , Skill.classes   = [Bane, Chakra, Ranged, Extending]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Enemies do
                apply' "Electricity" 2 []
                unlessM (targetHas "electrocuted") do
                    hide' "electrocuted" 0 []
                    trap' 0 (OnAction All) $
                        whenM (targetHas "Electricity") do
                            refresh "Electricity"
                            everyone $
                                whenM (targetHas "Electricity") $ afflict 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Depth Charge"
        , Skill.desc      = "Ameyuri surrounds herself with lightning and electrocutes an opponent, dealing 30 damage. Deals affliction damage if the target is affected by Electricity. Next turn, enemies who use a skill on Ameyuri will have 1 turn of Electricity applied to them."
        , Skill.classes   = [Bane, Chakra, Melee, Extending]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemy do
                has <- targetHas "Electricity"
                if has then afflict 30 else damage 30
          , p Self $ trapFrom 1 (OnHarmed All) do
                apply' "Electricity" 1 []
                unlessM (targetHas "electrocuted") do
                    hide' "electrocuted" 0 []
                    trap' 0 (OnAction All) $
                        whenM (targetHas "Electricity") do
                            refresh "Electricity"
                            everyone $
                                whenM (targetHas "Electricity") $ afflict 5
           ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Thunder Gate"
        , Skill.desc      = "With the twin blades of Baki plunged into the ground, Ameyuri calls down lightning from the sky to incinerate the battlefield around an enemy, dealing 30 piercing damage to them. Deals 10 additional damage per enemy affected by Electricity. Removes 1 turn of Electricity from all enemies."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Nin, Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Enemy do
                affected <- numAffected "Electricity"
                pierce (30 + 10 * affected)
                everyone $ hasten 1 "Electricity"
          ]
        }
      ]
    , [ invuln "Parry" "Ameyuri" [Physical] ]
    ] []
  , Character
    "Kushimaru Kuriarare"
    "Reanimated by Kabuto, Kushimaru was one of the Seven Swordsmen of the Mist. Wielding Nuibari, the legendary razor-wire longsword, Kushimaru stitches together his enemies to prevent them from acting."
    [ [ Skill.new
        { Skill.name      = "Needle and Thread"
        , Skill.desc      = "Nuibari skewers an enemy, dealing 20 piercing damage and marking them for 1 turn. During [Stitching Spider], this skill deals 5 additional damage and also targets all other enemies affected by [Stitching Spider]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 5 `bonusIf` userHas "Stitching Spider"
                pierce (20 + bonus)
                tag 1
          , p XEnemies $ whenM (targetHas "Stitching Spider") do
                pierce 25
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Stitching Spider"
        , Skill.desc      = "Kushimaru lays a trap of wires on the ground. For 3 turns, enemies who use physical skills will receive 10 piercing damage and be marked until after this skill ends."
        , Skill.classes   = [Physical, Ranged, InvisibleTraps]
        , Skill.cost      = k [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.channel   = Control 3
        , Skill.effects   =
          [ p Enemies do
                prolong 1 "Stitching Spider"
                trap (-2) (OnAction Physical) $
                    whenM (userHas "Stitching Spider") do
                        pierce 10
                        tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wire Crucifixion"
        , Skill.desc      = "Kushimaru stitches up all enemies affected by [Needle and Thread], dealing 15 piercing damage and ending their Action and Control skills in progress."
        , Skill.require   = HasU "Needle and Thread"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemies do
                pierce 15
                interrupt
          ]
        }
      ]
    , [ invuln "Parry" "Kushimaru" [Physical] ]
    ] []
  , Character
    "Jinpachi Munashi"
    "Reanimated by Kabuto, Jinpachi was one of the Seven Swordsmen of the Mist. Wielding Shibuki, the legendary explosive blade, Jinpachi builds up stockpiles of paper bombs that he can detonate simultaneously."
    [ [ Skill.new
        { Skill.name      = "Blast Sword"
        , Skill.desc      = "Jinpachi swings his sword at an enemy, dealing 30 damage and making them immune to effects from allies for 1 turn. If Jinpachi does not have any Paper Bombs, he loses 15 health. Otherwise, he spends one Paper Bomb."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy do
                damage 30
                apply 1 [Seal]
          , p Self do
                unlessM (userHas "Paper Bomb") $ sacrifice 0 15
                removeStack "Paper Bomb"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Shibuki Bomb Reload"
        , Skill.desc      = "Adding it to his sword, Jinpachi gains 1 Paper Bomb. Each Paper Bomb provides 5 points of damage reduction."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ p Self $ apply' "Paper Bomb" 0 [Reduce All Flat 5] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Multiple Explosions of Death"
        , Skill.desc      = "Jinpachi sets off a chain reaction of bombs around himself, dealing 40 damage to an enemy and 40 damage to a random enemy. Requires at least two Paper Bombs. If Jinpachi only has two Paper Bombs, he loses 30 health. Spends all Paper Bombs."
        , Skill.require   = HasI 2 "Paper Bomb"
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Self do
              stacks <- userStacks "Paper Bomb"
              when (stacks <= 2) $ sacrifice 0 30
          , p Enemy  $ damage 40
          , p REnemy $ damage 40
          ]
        }
      ]
    , [ invuln "Parry" "Jinpachi" [Physical] ]
    ]
    []
  , Character
    "Fuguki Suikazan"
    "Reanimated by Kabuto, Fuguki was one of the Seven Swordsmen of the Mist who wielded the legendary sentient sword Samehada. Without his sword, he relies on his chakra-enhanced hair to heal himself and ensnare his opponents."
    [ [ Skill.new
        { Skill.name      = "Needle Senbon"
        , Skill.desc      = "Fuguki hardens his hair into needles and launches a barrage at an enemy, dealing 15 piercing damage for 2 turns. While active, if they use a skill on Fuguki or one of his allies, they will be unable to target anyone else for 2 turns. Costs 1 random chakra during [Chakra Weave]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 1
        , Skill.channel   = Action 2
        , Skill.start     = 
          [ p Enemy $ trapFrom 2 OnHarm do
              targetSlot <- target slot
              apply 2 [Taunt targetSlot]
          ]
        , Skill.effects   =
          [ p Enemy $ pierce 15 ]
        , Skill.changes   = changeWith "Chakra Weave" $ setCost [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Weave"
        , Skill.desc      = "Fuguki weaves strands of chakra into his hair to defend himself. During each of the next 4 turns, if he does not take any damage, he regains 10 health. Each time he damages an opponent with a new skill, he gains 5 points of damage reduction that end when this skill ends."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 5
        , Skill.channel   = Ongoing 4
        , Skill.start     =
          [ p Self do
                bombWith [Hidden] 4 [] [ p Done $ remove "Chakra Weave" ]
                trap' 4 (OnDamaged All) $ hide' "hair" (-1) []
          ]
        , Skill.effects   =
          [ p Self do
                trap 1 OnDamage $ apply 0 [Reduce All Flat 5]
                delay (-1) $ unlessM (userHas "hair") $ heal 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharp Hair Spear"
        , Skill.desc      = "Fuguki extends his hair to skewer enemies around him, dealing 10 damage to the enemy team and stunning their non-mental skills for 1 turn. Deals 5 additional damage and pierces during [Chakra Weave]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemies do
                apply 1 [Stun NonMental]
                has <- userHas "Chakra Weave"
                if has then pierce 15 else damage 10
          ]
        }
      ]
    , [ invuln "Block" "Fuguki" [Physical] ]
    ] []
  , Character
    "Itachi Uchiha"
    "Reanimated by Kabuto, Itachi intends to stop the reanimation process and end the war. Unfettered by disease or mortality, Itachi is at the height of his power."
    [ [ Skill.new
        { Skill.name      = "Phoenix Flower"
        , Skill.desc      = "Itachi throws burning shuriken at an enemy, dealing 10 piercing damage immediately and 5 affliction damage for 2 turns. Next turn, this skill costs two random chakra, targets all enemies, and has a cooldown of 1."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemy do
                pierce 10
                apply 2 [Afflict 5]
          , p Self $ vary' 1 "Phoenix Flower" "Phoenix Flower"
          ]
        }
      , Skill.new
        { Skill.name      = "Phoenix Flower"
        , Skill.desc      = "Itachi throws burning shuriken at an enemy, dealing 10 piercing damage immediately and 5 affliction damage for 2 turns. Next turn, this skill costs two random chakra, targets all enemies, and has a cooldown of 1."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies do
                pierce 10
                apply 2 [Afflict 5]
          , p Self $ vary' 1 "Phoenix Flower" "Phoenix Flower"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Susano'o"
        , Skill.desc      = "Using the mangekyō sharingan's signature ability, Itachi creates a colossus of chakra around himself. For 1 turn, all damage to Itachi—including piercing and affliction—is reduced by 15 points. While active, this skill becomes [Totsuka Blade][g]."
        , Skill.classes   = [Chakra, Invisible]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Self do
                apply 1 [Reduce Affliction Flat 15]
                vary' 1 "Susano'o" "Totsuka Blade"
          ]
        }
      , Skill.new
        { Skill.name      = "Totsuka Blade"
        , Skill.desc      = "Itachi slashes an enemy with an ethereal liquid blade, dealing 20 piercing damage and extending the duration of [Susano'o] by 1 turn."
        , Skill.classes   = [Chakra, Invisible]
        , Skill.cost      = k [Gen]
        , Skill.effects   =
          [ p Enemy $ pierce 20
          ,  p Self do
                apply 1 [Reduce Affliction Flat 15]
                vary' 1 "Susano'o" "Totsuka Blade"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Izanami"
        , Skill.desc      = "Sacrificing one of his eyes, Itachi locks an opponent in an endless memory loop. For 2 turns, any skills they use on Itachi's team will apply an Izanami Snapshot to their targets. After 2 turns, the Snapshot completely restores its owner to their state when it was applied. Izanami Snapshots do not stack."
        , Skill.classes   = [Mental, Ranged, Invisible, Single]
        , Skill.cost      = k [Blood, Gen]
        , Skill.charges   = 2
        , Skill.effects   =
          [ p Enemy $ trapFrom 2 OnDamage $ snapshot (-2) ]
        }
      ]
    , [ invuln "Block" "Itachi" [Physical] ]
    ] []
  , Character
    "Nagato"
    "Reanimated by Kabuto, Nagato is as much a pawn in the schemes of others as he was in life. With the power of the Rinnegan and all his Paths at his disposal, he uses the attacks of his opponents to strengthen his own abilities."
    [ [ Skill.new
        { Skill.name      = "Human Path"
        , Skill.desc      = "Nagato restores 15 health and deals 20 piercing damage to an enemy. If the target deals any damage next turn, the damage and healing of this skill will be set to the damage they dealt for 1 turn and its cost will increase by 1 random chakra."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = k [Gen]
        , Skill.effects   =
          [ p Self $  heal 15
          , p Enemy do
                pierce 20
                trap 1 OnDamage $ self $ vary' 1 "Human Path" "Human Path"
                trapPer (-1) TrackDamage $ self . addStacks' (-1) "Human Path"
          ]
        }
      , Skill.new
        { Skill.name      = "Human Path"
        , Skill.desc      = "Nagato restores 15 health and deals 20 piercing damage to an enemy. If the target deals any damage next turn, the damage and healing of this skill will be set to the damage they dealt for 1 turn and its cost will remain increased."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = k [Gen, Rand]
        , Skill.effects   =
          [ p Self do
                stacks <- userStacks "Human Path"
                heal stacks
          , p Enemy do
                stacks <- userStacks "Human Path"
                pierce stacks
                trap 1 OnDamage $ self $ vary' 1 "Human Path" "Human Path"
                trapPer (-1) TrackDamage $ self . addStacks' (-1) "Human Path"
            ]
        , Skill.changes   = \n skill -> skill
            { Skill.desc = "Nagato restores " ++ tshow (numActive "Human Path" n) ++ " health and deals " ++ tshow (numActive "Human Path" n) ++ " piercing damage to an enemy. If the target deals any damage next turn, the damage and healing of this skill will be set to the damage they dealt for 1 turn and its cost will remain increased." }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Preta Path"
        , Skill.desc      = "Nagato absorbs attacks against him, countering all enemy skills next turn. Each countered skill restores 10 health to Nagato and absorbs 1 random chakra from its user."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self $ apply (-1) [ParryAll All $ Play do
                absorb 1
                self do
                    heal 10
                    addStack
                    stacks <- userStacks "Preta Path"
                    when (stacks >= 2) $ vary' 1 "Naraka Path" "Naraka Path" ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Naraka Path"
        , Skill.desc      = "Nagato tracks a target's damage for 2 turns. If the target is an ally, they are healed for the damage total. If the target is an enemy, they are damaged for the damage total. If the target did not cause any damage, their chakra costs are modified for 2 turns: increased by 1 random chakra if an enemy, decreased by 1 random chakra if an ally. If [Preta Path] countered 2 or more skills last turn, this skill affects all allies and enemies."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Gen, Gen]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p XAlly do
              trapPer (-2) TrackDamage heal
              trap (-2) OnDamage $ remove "Naraka Path"
              bomb (-2) [] [ p Expire $ apply 2 [Unexhaust] ]
          , p Enemy do
                trapPer (-2) TrackDamage damage
                trap (-2) OnDamage $ remove "Naraka Path"
                bomb (-2) [] [ p Expire $ apply 2 [Exhaust All] ]
          ]
        }
      , Skill.new
        { Skill.name      = "Naraka Path"
        , Skill.desc      = "Nagato tracks a target's damage for 2 turns. If the target is an ally, they are healed for the damage total. If the target is an enemy, they are damaged for the damage total. If the target did not cause any damage, their chakra costs are modified for 2 turns: increased by 1 random chakra if an enemy, decreased by 1 random chakra if an ally. If [Preta Path] countered 2 or more skills last turn, this skill affects all allies and enemies."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = k [Gen, Gen]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p XAllies do
                trapPer (-2) TrackDamage heal
                trap (-2) OnDamage $ remove "Naraka Path"
                bomb (-2) [] [ p Expire $ apply 2 [Unexhaust] ]
          , p Enemies do
                trapPer (-2) TrackDamage damage
                trap (-2) OnDamage $ remove "Naraka Path"
                bomb (-2) [] [ p Expire $ apply 2 [Exhaust All] ]
          ]
        }
    ]
    , [ Skill.new
        { Skill.name      = "Rinnegan"
        , Skill.desc      = "Whenever Nagato deals damage to an enemy, he can use this skill the following turn to damage an enemy for half his damage total from that turn. Whenever Nagato is healed, he can use this skill the following turn to heal himself or an ally for half the amount of health he regained during that turn."
        , Skill.classes   = [Mental]
        , Skill.cooldown  = 2
        , Skill.changes   = \n skill -> skill
          { Skill.effects = snd <$> filter fst
                            [ ( hasOwn "Rinnegan" n, p Enemy do
                                    stacks <- userStacks "Rinnegan"
                                    damage stacks
                              )
                            , ( hasOwn "Rinnegan Heal" n, p XAlly do
                                    stacks <- userStacks "Rinnegan Heal"
                                    heal stacks
                              )
                            ]
          , Skill.require   = if
              | hasOwn "Rinnegan" n || hasOwn "Rinnegan Heal" n -> Usable
              | otherwise                                       -> Unusable
          }
        }
      ]
    ]
    [ (PerDamage, addOwnStacks 1 "Rinnegan"      3 0 . (+ 2))
    , (PerHealed, addOwnStacks 1 "Rinnegan Heal" 3 0 . (+ 2))
    ]
  ]