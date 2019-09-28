{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated (cs) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Hashirama Senju"
    "Reanimated by Orochimaru, Hashirama was the founder of the Hidden Leaf Village and its first Hokage. His unique ability to manipulate wood allows him give life to trees, which protect his allies and impair his enemies."
    [ [ Skill.new
        { Skill.name      = "Tree Wave Destruction"
        , Skill.desc      = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ damage 10
          , To Allies  $ defend 0 5
          ]
        }
      , Skill.new
        { Skill.name      = "Tree Wave Destruction"
        , Skill.desc      = "Sending out trees in all directions, Hashirama deals 10 damage to all enemies and provides 5 permanent destructible defense to his team. Has no cooldown during [Deep Forest Creation]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.varicd    = True
        , Skill.effects   =
          [ To Enemies $ damage 10
          , To Allies  $ defend 0 5
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
              has <- userHas "Deep ForestCreation"
              apply 1 if has then [Stun All] else [Stun Physical, Stun Chakra]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Deep Forest Creation"
        , Skill.desc      = "The battlefield transforms into a forest. For 2 turns, enemy cooldowns are increased by 1 and the cost of enemy non-mental skills is increased by 1 arbitrary chakra. While active, this skill becomes [Deep Forest Flourishing][b][b]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Enemies $ apply 2 [Snare 1, Exhaust NonMental]
          ,  To Self do
                tag 2
                vary' 2 "Tree Wave Destruction" "Tree Wave Destruction"
                vary' 2 "Deep Forest Creation" "Deep Forest Flourishing"
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
  , Character
    "Tobirama Senju"
    "Reanimated by Orochimaru, Hashirama was the second Hokage. His water-manipulating skills flood the battlefield, impairing and harming the enemy team."
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
        , Skill.desc      = "A giant wave of water floods the enemy team for 3 turns, dealing 15 damage, negating their affliction damage, and increasing the damage of [Water Prison] by 15."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen, Nin]
        , Skill.cooldown  = 3
        , Skill.dur       = Ongoing 3
        , Skill.effects   =
          [ To Enemies do
                damage 15
                apply 1 [Stun Affliction]
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
  , Character
    "Hanzō"
    "Reanimated by Kabuto, Hanzō the Salamander was the leader of Amegakure. In combination with his unrivaled combat prowess, the lethal venom sac implanted in his body makes him a feared legend throughout the world."
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
        , Skill.require   = HasI (-1) "Poison Fog"
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
                apply 2 [Afflict 15]
                whenM (userHas "Major Summoning: Ibuse") do
                    afflict 10
                    apply 1 [Stun All]
          ]
        , Skill.changes   =
            changeWith "Major Summoning: Ibuse" \x -> x { Skill.pic = True }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Venom Sac"
        , Skill.desc      = "The first enemy to use a non-mental skill on Hanzō next turn will rupture his implanted venom sac, taking 20 affliction damage every turn and causing Hanzō to take 10 affliction damage every turn. When Hanzō summons Ibuse or if [Major Summoning: Ibuse] is already active, Hanzō will replace his venom sac with Ibuse's, ending [Major Summoning: Ibuse], curing himself of this skill, and decreasing the current cooldown of [Major Summoning: Ibuse] by 3."
        , Skill.classes   = [Physical, Bane, Invisible]
        , Skill.cost      = [Blood]
        , Skill.effects   =
            [ To Self $ trapFrom (-1) (OnHarmed NonMental) do
                  apply 0 [Afflict 20]
                  self $ removeTrap "Venom Sac"
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
    ]
  , Character
    "Gengetsu Hōzuki"
    "Reanimated by Kabuto, Gengetsu was the second Mizukage of the Hidden Mist Village. Charismatic and carefree, he cheerfully offers tips to his opponents on how to beat him. He is especially fond of one-on-one duels."
    [ [ Skill.new
        { Skill.name      = "Major Summoning: Giant Clam"
        , Skill.desc      = "Gengetsu summons a huge clam that exudes illusory mist for 4 turns. Each turn, a random member of his team becomes a mirage, reflecting the first skill an enemy uses on them next turn, and a random member of his team gains 80 destructible defense for 1 turn. If the clam's destructible defense is destroyed, this skill is canceled."
        , Skill.classes   = [Summon]
        , Skill.cost      = [Nin, Gen, Rand]
        , Skill.dur       = Ongoing 4
        , Skill.cooldown  = 5
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
                bonus <- 10 `bonusIf` userHas "Major Summoning: Giant Clam"
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
                       [ To Done $ setHealth targetHealth ]
                setHealth 30
                self do
                    bomb 2 [Duel targetSlot, Taunt targetSlot]
                           [ To Done $ setHealth userHealth ]
                    setHealth 30
          ]
        }
      ]
    , [ invuln "Mirage" "Gengetsu" [Mental] ]
    ]
  , Character
    "Mū"
    "Reanimated by Kabuto, Mū was the second Tsuchikage of the Hidden Rock Village. Unfailingly polite, he intends to ensure that his village benefits from the war. By manipulating matter at the atomic level, he disintegrates the defenses of his enemies."
    [ [ Skill.new
        { Skill.name      = "Particle Beam"
        , Skill.desc      = "A ray of high-energy atomic particles blasts an enemy, dealing 25 piercing damage. Deals 10 additional damage if the target is invulnerable. Deals 5 fewer damage and costs 1 ninjutsu chakra during [Fragmentation]."
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
        , Skill.desc      = "Mū's body undergoes fission and splits into two. For 2 turns, Mū ignores stuns and reduces damage against him by half. While active, Mū's damage is weakened by 5. If Mū's health reaches 0 during this skill, he regains 15 health and this skill ends."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                apply 2 [ Focus
                        , Reduce All Percent 50
                        , Weaken All Flat 5
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
        , Skill.desc      = "The atomic bonds within an enemy shatter, dealing 40 piercing damage and demolishing their destructible defense and his own destructible barrier. Deals 5 fewer damage and costs 1 ninjutsu chakra and 1 arbitrary chakra during [Fragmentation]."
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
  , Character
    "Rasa"
    "Reanimated by Kabuto, Rasa was the fourth Kazekage of the Hidden Sand Village and the father of the Sand Siblings. Cold and calculating, Rasa buries his enemies beneath crushingly heavy gold dust that they must fight their way out of to survive."
    [ [ Skill.new
        { Skill.name      = "Magnet Technique"
        , Skill.desc      = "Waves of gold flood the enemy team, dealing 10 damage to them and applying 10 permanent destructible barrier to each. The skills of enemies who have destructible barrier from this skill cost 1 additional arbitrary chakra."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                damage 10
                bonus <- 10 `bonusIf` targetHas "Gold Dust Waterfall"
                barrierDoes 0 (const $ return ()) (apply 1 [Exhaust All])
                    (10 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gold Dust Waterfall"
        , Skill.desc      = "A towering tidal wave of gold slams down on an enemy, dealing 35 damage and applying 30 permanent destructible barrier. The following turn, [Gold Dust Wave] and [24-Karat Barricade] will apply twice as much destructible barrier to them."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                damage 35
                barrier 0 30
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "24-Karat Barricade"
        , Skill.desc      = "Rasa constructs a golden blockade in front of an enemy. If they use a skill on Rasa or his allies next turn, it will be countered and they will gain 20 permanent destructible barrier."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 1 (Countered All) do
                bonus <- 20 `bonusIf` targetHas "Gold Dust Waterfall"
                barrier 0 (20 + bonus)
          ]
        }
      ]
    , [ invuln "Gold Dust Shield" "Rasa" [Physical] ]
    ]
  , Character
    "Jirōbō"
    "Reanimated by Kabuto, Jirōbō was a member of the Sound Five. No longer concealing his anger beneath a facade of politeness, Jirōbō has only one thing on his mind: revenge."
    [ [ Skill.new
        { Skill.name      = "Rivalry"
        , Skill.desc      = "Jirōbō picks out an enemy as his rival. If they use a skill on Jirōbō or his allies next turn, they will be countered and permanently forced to target Jirōbō. Effect ends if Jirōbō uses a skill on a different enemy or uses this skill again. Cannot be used during [Summoning: Earth Prison Golem]."
        , Skill.require   = HasI (-1) "Summoning: Earth Prison Golem"
        , Skill.classes   = [Mental, Melee, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                everyone $ remove "Rivalry"
                userSlot <- user slot
                trap (-1) (Countered All) $ apply 0 [Taunt userSlot]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sphere of Graves"
        , Skill.desc      = "Jirōbō lifts the ground up and hurls it forward, dealing 30 damage to an enemy and gaining a Scattered Rock. Costs one taijutsu chakra if [Earth Dome Prison] affected any enemies last turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 30
                unlessM (targetHas "Rivalry") . everyone $ remove "Rivalry"
          , To Self $ apply' "Scattered Rock" 0 []
          ]
        , Skill.changes   =
            changeWith "Earth Dome Prison" \x -> x { Skill.cost = [Tai] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Earth Dome Prison"
        , Skill.desc      = "Jirōbō encases an enemy in chakra-conductive rock and drains their energy, dealing 20 affliction damage. If this skill is used on the target of [Rivalry], the damage drains their health and adds it to Jirobo's health."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                has <- targetHas "Rivalry"
                if has then leech 20 $ self . heal
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
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
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
    ]
  , Character
    "Haku"
    "Reanimated by Kabuto, Haku remains as loyal to Zabuza as he was in life. With his inherited ice manipulation techniques, he disrupts his enemies while hiding safely behind crystalline mirrors."
    [ [ Skill.new
        { Skill.name      = "Thousand Needles of Death"
        , Skill.desc      = "Haku flings numerous ice needles outward, dealing 10 piercing damage to the enemy team. During [Crystal Ice Mirrors], this skill deals all 30 damage to a single enemy. If an enemy damaged by this skill loses at least 50 health during the same turn, they are stunned for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies do
                pierce 10
                trapPer' (-1) PerDamaged \i ->
                    when (i >= 50) $ apply 1 [Stun All]
          ]
        , Skill.changes   =
            changeWithChannel "Crystal Ice Mirrors" \x ->
              x { Skill.effects =
                  [ To Enemy do
                        pierce 30
                        trapPer' (-1) PerDamaged \i ->
                            when (i >= 50) $ apply 1 [Stun All] ]
                }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Acupuncture"
        , Skill.desc      = "Haku alters the flow of energy in an enemy by sticking a needle into one of their vital points, disabling the non-damage effects of their skills on allies and enemies for 2 turns. Bypasses invulnerability and targets all enemies during [Crystal Ice Mirrors]."
        , Skill.require   = HasU (-1) "Acupuncture"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ apply 2 [Silence] ]
        , Skill.changes   =
            changeWithChannel "Crystal Ice Mirrors" \x ->
              targetAll
              x { Skill.classes = insertSet Bypassing $ Skill.classes x }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crystal Ice Mirrors"
        , Skill.desc      = "Disorienting crystalline mirrors form all around the battlefield, providing 20 permanent destructible defense to Haku. For 3 turns, if Haku loses all destructible defense from this skill, he will gain destructible defense equal to how much health he lost during the same turn. Cannot be used while Haku still has destructible defense from this skill."
        , Skill.require   = DefenseI (-1) "Crystal Ice Mirrors"
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 6
        , Skill.dur       = Ongoing 3
        , Skill.start     =
          [ To Self $ defend 0 20
          ]
        , Skill.effects   =
          [ To Self do
                defense <- userDefense "Crystal Ice Mirrors"
                when (defense > 0) $ trapPer (-1) PerDamaged \i -> do
                    defense' <- userDefense "Crystal Ice Mirrors"
                    when (defense' == 0) $ defend 0 i

          ]
        }
      ]
    , [ invuln "Ice Dome" "Haku" [Chakra] ]
    ]
  , Character
    "Zabuza Momochi"
    "Reanimated by Kabuto, Zabuza was one of the Seven Swordsmen of the Mist and a renowned mercenary. Although he has been reunited with Haku, Zabuza is furious at being forced to fight against his will. He still wields Kubikiribōchō, his legendary executioner's broadsword, which feeds on the blood it spills to strengthen itself."
    [ [ Skill.new
        { Skill.name      = "Demon Shroud"
        , Skill.desc      = "Demonic chakra pours out of Zabuza as he gives in to his bloodlust, gaining 10 points of damage reduction for 2 turns and ignoring stuns. Each turn, a random enemy is affected by [Executioner's Butchering]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Self $ apply' "Demon Shroud " 1
                [Reduce All Flat 10, Focus]
          , To REnemy do
                pierce 30
                tag' "Executioner's Butchering" 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Blood Harvest"
        , Skill.desc      = "Kubikiribōchō drinks up the blood it has spilled and uses the iron to reinforce itself, draining 10 health from a target marked by [Executioner's Butchering] to provide permanent destructible defense equal to the damage dealt. Extends the duration of [Demon Shroud] by 1 turn if active."
        , Skill.require   = HasU 1 "Executioner's Butchering"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ leech 10 $ self . defend 0
          , To Self  $ prolongChannel 1 "Demon Shroud"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Executioner's Butchering"
        , Skill.desc      = "Zabuza's sword carves into an enemy like the edge of a guillotine, dealing 30 piercing damage and marking them for 1 turn. Cannot be used during [Demon Shroud]."
        , Skill.require   = HasI (-1) "Demon Shroud"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                pierce 30
                tag 1
          ]
        }
      ]
    , [ invuln "Block" "Zabuza" [Physical] ]
    ]
  , Character
    "Ameyuri Ringo"
    "Reanimated by Kabuto, Ameyuri was one of the Seven Swordsmen of the Mist. Wielding Baki, the legendary twin lightning blades, Ameyuri cuts down her enemies using paralyzing electricity."
    [ [ Skill.new
        { Skill.name      = "Lightning Fang"
        , Skill.desc      = "Bolts of lightning cascade across the battlefield, applying 2 turns of Electricity to all enemies. Whenever someone affected by Electricity uses a skill, Electricity on them is refreshed to its maximum duration, and everyone affected by Electricity receives 5 affliction damage that bypasses invulnerability. Reapplying Electricity extends its duration instead of stacking."
        , Skill.classes   = [Bane, Chakra, Ranged, Extending]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemies do
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
        , Skill.desc      = "Ameyuri surrounds herself with lightning and electrocutes an opponent, dealing 30 damage. Deals affliction damage if the target is affected by Electricity. Enemies who use a skill on Ameyuri next turn will have 1 turn of Electricity applied to them."
        , Skill.classes   = [Bane, Chakra, Melee, Extending]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                has <- targetHas "Electricity"
                if has then afflict 30 else damage 30
          , To Self $ trapFrom 1 (OnHarmed All) do
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
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                affected <- numAffected "Electricity"
                pierce (30 + 10 * affected)
                everyone $ hasten 1 "Electricity"
          ]
        }
      ]
    , [ invuln "Parry" "Ameyuri" [Physical] ]
    ]
  , Character
    "Kushimaru Kuriarare"
    "Reanimated by Kabuto, Kushimaru was one of the Seven Swordsmen of the Mist. Wielding Nuibari, the legendary razor-wire longsword, Kushimaru stitches together his enemies to prevent them from acting."
    [ [ Skill.new
        { Skill.name      = "Needle and Thread"
        , Skill.desc      = "Nuibari skewers an enemy, dealing 20 piercing damage and marking them for 1 turn. During [Stitching Spider], this skill deals 5 additional damage and also targets all other enemies affected by [Stitching Spider]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` userHas "Stitching Spider"
                pierce (20 + bonus)
                tag 1
          , To XEnemies $ whenM (targetHas "Stitching Spider") do
                pierce 25
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Stitching Spider"
        , Skill.desc      = "Kushimaru lays a trap of wires on the ground. For 3 turns, enemies who use physical skills will take 10 piercing damage and be marked until after this skill ends."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Control 3
        , Skill.effects   =
          [ To Enemies do
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
        , Skill.require   = HasU 1 "Needle and Thread"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemies do
                pierce 15
                interrupt
          ]
        }
      ]
    , [ invuln "Parry" "Kushimaru" [Physical] ]
    ]
  , Character
    "Jinpachi Munashi"
    "Reanimated by Kabuto, Jinpachi was one of the Seven Swordsmen of the Mist. Wielding Shibuki, the legendary explosive blade, Jinpachi builds up stockpiles of paper bombs that he can detonate simultaneously."
    [ [ Skill.new
        { Skill.name      = "Blast Sword"
        , Skill.desc      = "Jinpachi swings his sword at an enemy, dealing 30 damage and making them ignore helpful effects for 1 turn. If Jinpachi does not have any Paper Bombs, he loses 15 health. Otherwise, he spends one Paper Bomb."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 30
                apply 1 [Seal]
          , To Self do
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
          [ To Self $ apply' "Paper Bomb" 0 [Reduce All Flat 5] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Multiple Explosions of Death"
        , Skill.desc      = "Jinpachi sets off a chain reaction of bombs around himself, dealing 40 damage to an enemy and 40 damage to a random enemy. Requires at least two Paper Bombs. If Jinpachi only has two Paper Bombs, he loses 30 health. Spends all Paper Bombs."
        , Skill.require   = HasI 2 "Paper Bomb"
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
              stacks <- userStacks "Paper Bomb"
              when (stacks <= 2) $ sacrifice 0 30
          , To Enemy  $ damage 40
          , To REnemy $ damage 40
          ]
        }
      ]
    , [ invuln "Parry" "Jinpachi" [Physical] ]
    ]
  , Character
    "Fuguki Suikazan"
    "Reanimated by Kabuto, Fuguki was one of the Seven Swordsmen of the Mist who wielded the legendary sentient sword Samehada. Without his sword, he relies on his chakra-enhanced hair to heal himself and ensnare his opponents."
    [ [ Skill.new
        { Skill.name      = "Needle Senbon"
        , Skill.desc      = "Fuguki hardens his hair into needles and launches a barrage at an enemy, dealing 15 piercing damage for 2 turns. While active, if they use a skill on Fuguki or his allies, they will be unable to target anyone else for 2 turns. Costs 1 arbitrary chakra during [Chakra Weave]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.start     =
          [ To Enemy $ trapFrom 2 OnHarm do
              targetSlot <- target slot
              apply 2 [Taunt targetSlot]
          ]
        , Skill.effects   =
          [ To Enemy $ pierce 15 ]
        , Skill.changes   =
            changeWithChannel "Chakra Weave" \x -> x { Skill.cost = [Rand] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Weave"
        , Skill.desc      = "Fuguki weaves strands of chakra into his hair to defend himself. During each of the next 4 turns, if he does not take any damage, he regains 10 health. Each time he uses a skill that damages an opponent, he gains 5 points of damage reduction that end when this skill ends."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 5
        , Skill.dur       = Action 4
        , Skill.start     =
          [ To Self do
                bombWith [Hidden] 4 [] [ To Done $ remove "Chakra Weave" ]
                trap' 4 (OnDamaged All) $ hide' "hair" (-1) []
          ]
        , Skill.effects   =
          [ To Self do
                trap 1 OnDamage $ apply 0 [Reduce All Flat 5]
                delay (-1) $ unlessM (userHas "hair") $ heal 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharp Hair Spear"
        , Skill.desc      = "Fuguki extends his hair to skewer enemies around him, dealing 10 damage to the enemy team and stunning their non-mental skills for 1 turn. Deals 5 additional damage and pierces during [Chakra Weave]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                apply 1 [Stun NonMental]
                has <- userHas "Chakra Weave"
                if has then pierce 15 else damage 10
          ]
        }
      ]
    , [ invuln "Block" "Fuguki" [Physical] ]
    ]
  ]
