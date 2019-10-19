{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated.Organizations (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Category -> Character]
characters =
  [ Character
    "Kimimaro"
    "Reanimated by Kabuto, Kimimaro was a member of the Sound Five until he was claimed by illness. Loyal to Orochimaru, Kimimaro now follows Kabuto, who carries Orochimaru's chakra and shares similar ambitions."
    [ [ Skill.new
        { Skill.name      = "Clematis Dance"
        , Skill.desc      = "Kimimaro attacks the enemy team with long, sharp bone spears, dealing 20 damage and killing them if their health reaches 5 or lower."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                damage 20
                hp <- target health
                when (hp < 5) kill
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Macabre Bone Pulse"
        , Skill.desc      = "Kimimmaro warps his skeleton into blades and attacks an enemy, dealing 45 piercing damage."
        , Skill.classes   = [Physical, Melee, Uncounterable]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 45 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Digital Shrapnel"
        , Skill.desc      = "A volley of bullets shoot forth from Kimimaro's fingertips, providing him with 50% damage reduction for 1 turn. Next turn, enemies who use skills will take 20 damage."
        , Skill.classes   = [Physical, Ranged, Bypassing, Invisible]
        , Skill.cooldown  = 2
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ trap 1 (OnAction All) $ damage 20
          , To Self $ apply 1 [Reduce [All] Percent 50]
          ]
        }
      ]
    , [ invuln "Block" "Kimimaro" [Physical] ]
    ]
    75
  , Character
    "Jirōbō"
    "Reanimated by Kabuto, Jirōbō was a member of the Sound Five. No longer concealing his anger beneath a facade of politeness, Jirōbō has only one thing on his mind: revenge."
    [ [ Skill.new
        { Skill.name      = "Rivalry"
        , Skill.desc      = "Jirōbō picks out an enemy as his rival. If they use a skill on Jirōbō or his allies next turn, they will be countered and forced to target Jirōbō. Effect ends if Jirōbō uses a skill on a different enemy or uses this skill again. Cannot be used during [Summoning: Earth Prison Golem]."
        , Skill.require   = HasI 0 "Summoning: Earth Prison Golem"
        , Skill.classes   = [Mental, Melee, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                everyone $ remove "Rivalry"
                userSlot <- user slot
                trap 1 (Countered All) $ apply 0 [Taunt userSlot]
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
    50
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
        , Skill.require   = HasU 0 "Acupuncture"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ apply 2 [Silence] ]
        , Skill.changes   =
            changeWithChannel "Crystal Ice Mirrors" \x ->
              targetAll
              x { Skill.classes = Bypassing `insertSet` Skill.classes x }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crystal Ice Mirrors"
        , Skill.desc      = "Disorienting crystalline mirrors form all around the battlefield, providing 20 permanent destructible defense to Haku. For 3 turns, if Haku loses all destructible defense from this skill, he will gain destructible defense equal to how much health he lost during the same turn. Cannot be used while Haku still has destructible defense from this skill."
        , Skill.require   = DefenseI 0 "Crystal Ice Mirrors"
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
    50
  , Character
    "Zabuza Momochi"
    "Reanimated by Kabuto, Zabuza was one of the Seven Swordsmen of the Mist and a renowned mercenary. Although he has been reunited with Haku, Zabuza is furious at being forced to fight against his will. He still wields Kubikiribōchō, his legendary executioner's broadsword, which feeds on the blood it spills to strengthen itself."
    [ [ Skill.new
        { Skill.name      = "Demon Shroud"
        , Skill.desc      = "Demonic chakra pours out of Zabuza as he gives in to his bloodlust, gaining 10 points of damage reduction for 2 turns and ignoring stuns and disabling effects. Each turn, a random enemy is affected by [Executioner's Butchering]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Self $ apply' "Demon Shroud " 1
                [Reduce [All] Flat 10, Focus]
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
          , To Self $ prolongChannel 1 "Demon Shroud"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Executioner's Butchering"
        , Skill.desc      = "Zabuza's sword carves into an enemy like the edge of a guillotine, dealing 30 piercing damage and marking them for 1 turn. Cannot be used during [Demon Shroud]."
        , Skill.require   = HasI 0 "Demon Shroud"
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
    75
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
    75
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
                trap 2 (OnAction Physical) $
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
    , [ invuln "Block" "Kushimaru" [Physical] ]
    ]
    75
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
        , Skill.desc      = "Jinpachi adds a Paper Bomb to his sword. In addition to fueling his other attacks, Paper Bombs provides 5 points of damage reduction each."
        , Skill.classes   = [Physical]
        , Skill.effects   =
          [ To Self $ apply' "Paper Bomb" 0 [Reduce [All] Flat 5] ]
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
          [ To Enemy $ damage 40
          , To REnemy $ damage 40
          , To Self do
              stacks <- userStacks "Paper Bomb"
              when (stacks <= 2) $ sacrifice 0 30
          ]
        }
      ]
    , [ invuln "Parry" "Jinpachi" [Physical] ]
    ]
    75
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
                trap 1 OnDamage $ apply 0 [Reduce [All] Flat 5]
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
    75
  , Character
    "Jinin Akebino"
    "Reanimated by Kabuto, Jinin was one of the Seven Swordsmen of the Mist. Wielding Kabutowari, the legendary blunt blade, Jinin cleaves the armor and protections of his enemies."
    [ [ Skill.new
        { Skill.name      = "Axe Chop"
        , Skill.desc      = "Slashing an enemy with the axe part of Kabutowari, Jinin deals 15 piercing damage, disables the countering effects of their skills, and prevents them from reducing damage or becoming invulnerable."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                apply 1 [Expose, Disable Counters]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hammer Bash"
        , Skill.desc      = "Jinin slams the hammer part of Kabutowari into an enemy, dealing 25 damage. If the target is affected by [Axe Slash], they are prevented from reducing damage or becoming invulnerable for 4 turns."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 25
                whenM (targetHas "Axe Slash") $ apply 4 [Expose]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Helm-Splitter"
        , Skill.desc      = "Bringing the full might of Kabutowari down upon an enemy, Jinin deals 35 damage and stuns the target's chakra and melee skills for 1 turn."
        , Skill.classes   = [Physical, Melee, Uncounterable]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 35
                apply 1 [Stun Chakra, Stun Melee]
          ]
        }
      ]
    , [ invuln "Dodge" "Jinin" [Physical] ]
    ]
    75
  , Character
    "Deidara"
    "Reanimated by Kabuto, Deidara was an Akatsuki member obsessed with art. His style remains largely as it was when he was alive, revolving around clay explosives that successively render its victims more vulnerable to subsequent explosions."
    [ [ Skill.new
        { Skill.name      = "Detonating Clay"
        , Skill.desc      = "Deidara tosses an explosive clay bomb at an enemy, dealing 20 piercing damage and becoming invulnerable to mental skills for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacksA <- targetStacks "Chakra Clay Trap"
                stacksB <- targetStacks "Sonar Bat Bombs"
                stacksC <- targetStacks "Jellyfish Explosives"
                pierce (20 + 5 * stacksA + 5 * stacksB + 10 * stacksC)
          , To Self $ apply 1 [Invulnerable Mental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Clay Trap"
        , Skill.desc      = "Deidara plants an explosive mine near an enemy. The next time they use a skill on Deidara or his allies, they will take 20 piercing damage and [Detonating Clay] will deal 5 additional damage to them. The trap does not stack."
        , Skill.classes   = [Chakra, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 0 OnHarm do
                pierce 20
                addStack
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sonar Bat Bombs"
        , Skill.desc      = "With piercing shrieks, a swarm of bats deals 10 affliction damage to all enemies and increases the damage of [Detonating Clay] on them by 5. Once used, this skill becomes [Jellyfish Explosives][n]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemies do
                afflict 10
                addStack
          , To Self $ hide 0
                [Alternate "Sonar Bat Bombs" "Jellyfish Explosives"]
          ]
        }
      , Skill.new
        { Skill.name      = "Jellyfish Explosives"
        , Skill.desc      = "Deidara sculpts a jellyfish out of clay that explodes near an enemy, dealing 25 piercing damage and increasing the damage of [Detonating Clay] to them by 10. Once used, this skill becomes [Sonar Bat Bombs][n]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                pierce 25
                addStack
          , To Self $ remove "sonar bat bombs"
          ]
        }
      ]
    , [ invuln "Clay Clone" "Deidara" [Chakra] ]
    ]
    100
  , Character
    "Sasori"
    "Reanimated by Kabuto, Sasori was an Akatsuki member who crafted puppets from human corpses. Finally inhabiting the perfect, ageless body he strived for in life, Sasori uses his chakra threads to control enemies and allies alike."
    [ [ Skill.new
        { Skill.name      = "Chakra Threads"
        , Skill.desc      = "Sasori defends himself by ensaring his enemies, gaining 10 points of damage reduction."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 4 [Reduce [All] Flat 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ally Control"
        , Skill.desc      = "Sasori manipulates an ally with puppeteering threads. All non-ranged skills that enemies use on the target next turn will be reflected back at them."
        , Skill.classes   = [Physical, Invisible]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly $ apply 1 [ReflectAll NonRanged] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Puppet Manipulation"
        , Skill.desc      = "Sasori attacks an enemy with chakra threads, dealing 15 damage and weakening their damage by 5 for 1 turn. If their health reaches 35 or lower, their physical and chakra skills are stunned for 1 turn. Deals 5 additional damage during [Chakra Threads]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` userHas "Chakra Threads"
                damage (15 + bonus)
                targetHealth <- target health
                if targetHealth <= 35 then
                    apply 1 [Weaken [All] Flat 5, Stun Physical, Stun Chakra]
                else
                    apply 1 [Weaken [All] Flat 5]
          ]
        }
      ]
    , [ invuln "Block" "Sasori" [Physical] ]
    ]
    100
  , Character
    "Nagato"
    "Reanimated by Kabuto, Nagato is as much a pawn in the schemes of others as he was in life. With the full power of the Rinnegan, he has all six Paths at his disposal."
    [ [ Skill.new
        { Skill.name      = "Human Path"
        , Skill.desc      = "Nagato attacks the soul of an enemy, dealing 30 damage and preventing them from affecting Nagato's team for 1 turn."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 30
                apply 1 [BlockEnemies]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Preta Path"
        , Skill.desc      = "Nagato absorbs an enemy's energy, stealing 10 health and absorbing a bloodline or taijutsu chakra."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                absorb1 [Blood, Tai]
                leech 10 $ self . heal
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Animal Path"
        , Skill.desc      = "Nagato summons a horde of beasts that deal 10 damage to all enemies for 2 turns. While active, enemies who use skills on Nagato will take 10 damage."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Ongoing 2
        , Skill.effects   =
          [ To Enemies $ damage 10
          , To Self $ trapFrom 1 (OnHarmed All) $ damage 10
          ]
        }
      ]
    , [ invuln "Asura Path" "Nagato" [Physical] ]
    ]
    125
  ]
