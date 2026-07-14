{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Reanimated.Organizations (characters, reanimations) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Category -> Text -> Character]
characters =
  [ Character
    "Jirōbō" 50
    "Reanimated by Kabuto, Jirōbō was a member of the Sound Five. No longer concealing his anger beneath a facade of politeness, Jirōbō has only one thing on his mind: revenge."
    [SoundVillage, Kabuto, Orochimaru, Earth]
    [ [ Skill.new
        { Skill.name      = "Rivalry"
        , Skill.desc      = "Jirōbō picks out an enemy as his rival. If they use a skill on Jirōbō or his allies next turn, they will be countered and forced to target Jirōbō. Effect ends if Jirōbō uses a skill on a different enemy or uses this skill again. Cannot be used during [Summoning: Earth Prison Golem]."
        , Skill.require   = [UserChannel False "Summoning: Earth Prison Golem"]
        , Skill.classes   = [Mental, Melee, Invisible, Soulbound, Bypassing]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                targeting Everyone $ remove skillName
                trap 1 skillName (Countered All) do
                    slot <- user slot
                    apply Permanent skillName [Taunt slot]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sphere of Graves"
        , Skill.desc      = "Jirōbō lifts the ground up and hurls it forward, dealing 30 damage to an enemy and gaining a Scattered Rock. Costs [t] if [Earth Dome Prison] affected any enemies last turn."
        , Skill.classes   = [Physical, Ranged, Resource]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 30
                unlessM (target has "Rivalry") $ targeting Everyone $
                    remove "Rivalry"
          , To Self $ addStack "Scattered Rock"
          ]
        , Skill.changes   = changeWith "Earth Dome Prison" $ setCost [Tai]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Earth Dome Prison"
        , Skill.desc      = "Jirōbō encases an enemy in chakra-conductive rock and drains their energy, dealing 20 affliction damage. If this skill is used on the target of [Rivalry], the damage drains their health and adds it to Jirobo's health."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                rivalry <- target has "Rivalry"
                if rivalry then
                    leech 20 heal
                else do
                    afflict 20
                    targeting Everyone $ remove "Rivalry"
                targeting Self $ tag 1 skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Earth Prison Golem"
        , Skill.desc      = "Jirōbō spends two Scattered Rocks to summon a golem, gaining 35 destructible defense for 2 turns. While Jirōbō has destructible defense from this skill, all enemies are his Rivals and can only target him. The first enemy to use a skill on him each turn is instantly affected by [Earth Dome Prison]."
        , Skill.require   = [UserHas AtLeast 2 "Scattered Rock"]
        , Skill.classes   = [Summon, Soulbound, Bypassing]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Passive
        , Skill.start     =
          [ To Self do
                removeStacks "Scattered Rock" 2
                defend 2 35
                onBreak $ cancelChannel skillName
          ]
        , Skill.effects   =
          [ To Self do
                userSlot <- user slot
                targeting Enemies do
                    apply 1 "Rivalry" [Taunt userSlot]
                trapFrom 1 skillName (OnHarmed All) do
                    leech 20 heal
                    targeting Self do
                        removeTrap skillName
                        tag 1 "Earth Dome Prison"
          ]
        , Skill.end      =
          [ To Self $ targeting Enemies $ remove "Rivalry" ]
        }
      ]
    ]
  , Character
    "Kimimaro" 75
    "Reanimated by Kabuto, Kimimaro was a member of the Sound Five until he was claimed by illness. Loyal to Orochimaru, Kimimaro now follows Kabuto, who carries Orochimaru's chakra and shares similar ambitions."
    [SoundVillage, Kabuto, Orochimaru]
    [ [ Skill.new
        { Skill.name      = "Clematis Dance"
        , Skill.desc      = "Kimimaro attacks the enemy team with long, sharp bone spears, dealing 20 damage and killing them if their health reaches 5 or lower."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                damage 20
                executeAt 5
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
          [ To Enemies $ trap 1 skillName (OnAction All) $ asAction $
                damage 20
          , To Self $ apply 1 skillName [Reduce [All] Percent 50]
          ]
        }
      ]
    , [ invuln "Block" "Kimimaro" [Physical] ]
    ]
  , Character
    "Haku" 50
    "Reanimated by Kabuto, Haku remains as loyal to Zabuza as he was in life. With his inherited ice manipulation techniques, he disrupts his enemies while hiding safely behind crystalline mirrors."
    [MistVillage, Kabuto, Wind, Water]
    [ [ Skill.new
        { Skill.name      = "Thousand Needles of Death"
        , Skill.desc      = "Haku flings numerous ice needles outward, dealing 10 piercing damage to the enemy team. During [Crystal Ice Mirrors], this skill deals all 30 damage to a single enemy. If an enemy damaged by this skill loses at least 50 health during the same turn, they are stunned for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies do
                bonus <- 20 `bonusIf` channeling "Crystal Ice Mirrors"
                pierce (10 + bonus)
                trapPer' -1 skillName PerDamaged \i -> when (i >= 50) $
                    apply 1 skillName [Stun All]
          ]
        , Skill.changes   = changeWithChannel "Crystal Ice Mirrors" restrict
        }
      ]
    , [ Skill.new
        { Skill.name      = "Acupuncture"
        , Skill.desc      = "Haku alters the flow of energy in an enemy by sticking a needle into one of their vital points, disabling the non-damage effects of their skills on allies and enemies for 2 turns. Bypasses invulnerability and targets all enemies during [Crystal Ice Mirrors]."
        , Skill.require   = [TargetHas AtMost 0 "Acupuncture"]
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ apply 2 skillName [Silence] ]
        , Skill.changes   = changeWithChannel "Crystal Ice Mirrors" $
                            targetAll . addClasses [Bypassing]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crystal Ice Mirrors"
        , Skill.desc      = "Disorienting crystalline mirrors form all around the battlefield, providing 20 permanent destructible defense to Haku. For 3 turns, if Haku loses all destructible defense from this skill, he will gain destructible defense equal to how much health he lost during the same turn. Cannot be used while Haku still has destructible defense from this skill."
        , Skill.require   = [UserDefense AtMost 0 "Crystal Ice Mirrors"]
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 6
        , Skill.dur       = Ongoing 3
        , Skill.start     =
          [ To Self $ defend Permanent 20 ]
        , Skill.effects   =
          [ To Self $ whenM (user has' defense skillName) $
                trapPer 1 skillName PerDamaged $
                    unlessM (user has' defense skillName) .
                        defend Permanent
          ]
        }
      ]
    , [ invuln "Ice Dome" "Haku" [Chakra] ]
    ]
  , Character
    "Zabuza Momochi" 75
    "Reanimated by Kabuto, Zabuza was one of the Seven Swordsmen of the Mist and a renowned mercenary. Although he has been reunited with Haku, Zabuza is furious at being forced to fight against his will. He still wields Kubikiribōchō, his legendary executioner's broadsword, which feeds on the blood it spills to strengthen itself."
    [MistVillage, Kabuto, SevenSwordsmen, Rogue, Water]
    [ [ Skill.new
        { Skill.name      = "Demon Shroud"
        , Skill.desc      = "Demonic chakra pours out of Zabuza as he gives in to his bloodlust, gaining 10 points of damage reduction for 2 turns and ignoring stuns and disabling effects. Each turn, a random enemy is affected by [Executioner's Butchering]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Self $ apply 1 "Demon Shroud"
                [ Reduce [All] Flat 10
                , Focus
                ]
          , To REnemy do
                pierce 30
                tag 1 "Executioner's Butchering"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Blood Harvest"
        , Skill.desc      = "Kubikiribōchō drinks up the blood it has spilled and uses the iron to reinforce itself, draining 10 health from a target marked by [Executioner's Butchering] to provide permanent destructible defense equal to the damage dealt. Extends the duration of [Demon Shroud] by 1 turn if active."
        , Skill.require   = [TargetHas AtLeast 1 "Executioner's Butchering"]
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ leech 10 $ defend Permanent
          , To Self $ prolongChannel "Demon Shroud" 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Executioner's Butchering"
        , Skill.desc      = "Zabuza's sword carves into an enemy like the edge of a guillotine, dealing 30 piercing damage and marking them for 1 turn. Cannot be used during [Demon Shroud]."
        , Skill.require   = [UserChannel False "Demon Shroud"]
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                pierce 30
                tag 1 skillName
          ]
        }
      ]
    , [ invuln "Block" "Zabuza" [Physical] ]
    ]
  , Character
    "Ameyuri Ringo" 75
    "Reanimated by Kabuto, Ameyuri was one of the Seven Swordsmen of the Mist. Wielding Baki, the legendary twin lightning blades, Ameyuri cuts down her enemies using paralyzing electricity."
    [MistVillage, Kabuto, SevenSwordsmen, Jonin, Lightning]
    let
        electrocute dur = do
            unlessM (target has' traps "Electricity") $
                trapWith [Hidden] Permanent "Electricity" (OnAction All) do
                    electricity <- target amount "Electricity"
                    when (electricity > 0) do
                        remove "Electricity"
                        addStacks' (toEnum electricity) "Electricity" electricity
                        targeting Everyone $ whenM (target has "Electricity") $
                            asAction $ afflict 5

            mcurrentDur <- target duration "Electricity"
            case mcurrentDur of
                Nothing -> addStacks' dur "Electricity" $ fromEnum dur
                Just currentDur -> do
                    currentStacks <- target amount "Electricity"
                    let newDur    = currentDur + dur
                        newStacks = max currentStacks $ fromEnum newDur
                    remove "Electricity"
                    addStacks' newDur "Electricity" newStacks
    in
    [ [ Skill.new
        { Skill.name      = "Lightning Fang"
        , Skill.desc      = "Bolts of lightning cascade across the battlefield, applying 2 turns of Electricity to all enemies. Whenever someone affected by Electricity uses a skill, Electricity on them is refreshed to its maximum duration, and everyone affected by Electricity receives 5 affliction damage that bypasses invulnerability. Reapplying Electricity extends its duration instead of stacking."
        , Skill.classes   = [Bane, Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemies $ electrocute 2 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Depth Charge"
        , Skill.desc      = "Ameyuri surrounds herself with lightning and electrocutes an opponent, dealing 30 damage. Deals affliction damage if the target is affected by Electricity. Enemies who use a skill on Ameyuri next turn will have 1 turn of Electricity applied to them."
        , Skill.classes   = [Bane, Chakra, Melee, Nonstacking]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                electricity <- target has "Electricity"
                if electricity then afflict 30 else damage 30
          , To Self $ trapFrom 1 skillName (OnHarmed All) $
                electrocute 1
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
                bonus <- 10 `bonusPer` numAffected "Electricity"
                pierce (30 + bonus)
                targeting Everyone $ hasten 1 "Electricity"
          ]
        }
      ]
    , [ invuln "Parry" "Ameyuri" [Physical] ]
    ]
  , Character
    "Kushimaru Kuriarare" 75
    "Reanimated by Kabuto, Kushimaru was one of the Seven Swordsmen of the Mist. Wielding Nuibari, the legendary razor-wire longsword, Kushimaru stitches together his enemies to prevent them from acting."
    [MistVillage, Kabuto, SevenSwordsmen, Jonin]
    [ [ Skill.new
        { Skill.name      = "Needle Stitching"
        , Skill.desc      = "Nuibari skewers an enemy and pulls a wire through them, dealing 20 piercing damage and preventing them from affecting him for 1 turn. Deals 5 additional damage per person affected by [Needle Stitching] and extends its duration on them by 1 turn. Cannot be used on an enemy already affected by this skill."
        , Skill.require   = [TargetHas AtMost 0 "Needle Stitching"]
        , Skill.classes   = [Physical, Melee, Atemporal]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusPer` numAffected skillName
                targeting Everyone $ whenM (target has skillName) $
                    prolong 1 skillName
                pierce (20 + bonus)
                userSlot <- user slot
                bomb 1 skillName
                    [ Block userSlot ]
                    [ To Done $ targeting Self $ removeStack "needle stitching" ]
                targeting Self $ hide Permanent skillName []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Eviscerate"
        , Skill.desc      = "Using Nuibaru's razor wire, Kushimaru deals 20 piercing damage to all enemies and prolongs every [Needle Stitching] and [Wire Cruxifixion] by 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies do
                pierce 20
                prolong 1 "Needle Stitching"
                prolong 1 "Wire Crucifixion"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Wire Crucifixion"
        , Skill.desc      = "Kushimaru stitches up his victims and suspends them in air, dealing 15 damage to all enemies affected by [Needle Stitching] and stunning them for 1 turn. While active, targets cannot reduce damage or become invulnerable. Costs 1 arbitrary chakra per [Needle Stitching] active."
        , Skill.require   = [TargetHas AtLeast 1 "Needle Stitching"]
        , Skill.classes   = [Physical, Ranged]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                pierce 15
                apply 1 skillName
                    [ Stun All
                    , Expose
                    ]
          ]
        , Skill.changes   = changePer "needle stitching" \i ->
                                setCost $ replicate i Rand
        }
      ]
    , [ invuln "Block" "Kushimaru" [Physical] ]
    ]
  , Character
    "Jinpachi Munashi" 75
    "Reanimated by Kabuto, Jinpachi was one of the Seven Swordsmen of the Mist. Wielding Shibuki, the legendary explosive blade, Jinpachi sets off unrelenting chain reactions of paper bombs that deploy endlessly from its scroll."
    [MistVillage, Kabuto, SevenSwordsmen, Jonin]
    [ [ Skill.new
        { Skill.name      = "Splatter"
        , Skill.desc      = "Jinpachi swings his sword at an enemy and detonates numerous paper bombs on contact, dealing 30 affliction damage. For 1 turn, the target receives 5 additional damage from non-affliction skills. Deals 10 additional damage and lasts 1 additional turn if [Bomb Reload] was used last turn."
        , Skill.classes   = [Chakra, Melee, Bane]
        , Skill.cost      = [Tai]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Enemy do
                bonusDmg <- 10 `bonusIf` user has "Bomb Reload"
                afflict (30 + bonusDmg)
                bonusDur <- 1 `bonusIf` user has "Bomb Reload"
                apply (1 + bonusDur) skillName [Bleed [NonAffliction] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Bomb Reload"
        , Skill.desc      = "The next row of paper bombs slides out from Shibuki's integrated scroll, recharging [Splatter], and providing Jinpachi with 10 permanent destructible defense."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                defend Permanent 10
                recharge "Splatter"
                tag 1 skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Scroll Unraveling"
        , Skill.desc      = "Jinpachi fully unwinds the sheet of bombs around his sword in preparation for an attack. For 1 turn, enemies who use skills on Jinpachi will take 10 affliction damage every turn for the rest of the game. Next turn, this skill becomes [Multiple Explosions of Death][t][r][r]."
        , Skill.classes   = [Physical, Invisible, Bane]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                trapFrom 1 skillName (OnHarmed All) $
                    apply Permanent skillName [Afflict 10]
                apply 1 skillName
                    [ Alternate "Scroll Unraveling"
                                "Multiple Explosions of Death"
                    ]
          ]
        }
      , Skill.new
        { Skill.name      = "Multiple Explosions of Death"
        , Skill.desc      = "Jinpachi sets off a cascading chain reaction of bombs around himself, dealing 25 affliction damage to the enemy team. All targets take 5 affliction damage every turn for the rest of the game."
        , Skill.classes   = [Physical, Ranged, Bane]
        , Skill.cost      = [Tai, Rand, Rand]
        , Skill.effects   =
          [ To Enemies do
                afflict 25
                apply Permanent skillName [Afflict 5]
          ]
        }
      ]
    , [ invuln "Block" "Jinpachi" [Physical] ]
    ]
  , Character
    "Fuguki Suikazan" 75
    "Reanimated by Kabuto, Fuguki was one of the Seven Swordsmen of the Mist who wielded the legendary sentient sword Samehada. Without his sword, he relies on his chakra-enhanced hair to heal himself and ensnare his opponents."
    [MistVillage, Kabuto, SevenSwordsmen, Jonin]
    [ [ Skill.new
        { Skill.name      = "Needle Senbon"
        , Skill.desc      = "Fuguki hardens his hair into needles and launches a barrage at an enemy, dealing 15 piercing damage for 2 turns. While active, if they use a skill on Fuguki or his allies, they will be unable to target anyone else for 2 turns. Costs [r] during [Chakra Weave]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                trapFrom 1 skillName OnHarm do
                    targetSlot <- target slot
                    apply 2 skillName [Taunt targetSlot]
          ]
        , Skill.changes   = changeWithChannel "Chakra Weave" $ setCost [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Weave"
        , Skill.desc      = "Fuguki weaves strands of chakra into his hair to defend himself. During each of the next 4 turns, if he does not take any damage, he regains 10 health. Whenever he uses a skill that damages an opponent, he gains 5 points of damage reduction that end when this skill ends."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 5
        , Skill.dur       = Action 4
        , Skill.start     =
          [ To Self replaceChannel ]
        , Skill.effects   =
          [ To Self do
                trap 1 skillName OnNotDamaged $
                    heal 10
                trap 1 skillName OnDamage $
                    apply Permanent skillName [Reduce [All] Flat 5]
          ]
        , Skill.end       =
          [ To Self $ removeTrap skillName ]
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
                apply 1 skillName [Stun NonMental]
                weave <- channeling "Chakra Weave"
                if weave then pierce 15 else damage 10
          ]
        }
      ]
    , [ invuln "Block" "Fuguki" [Physical] ]
    ]
  , Character
    "Jinin Akebino" 75
    "Reanimated by Kabuto, Jinin was one of the Seven Swordsmen of the Mist. Wielding Kabutowari, the legendary blunt blade, Jinin cleaves the armor and protections of his enemies."
    [MistVillage, Kabuto, SevenSwordsmen, Jonin]
    [ [ Skill.new
        { Skill.name      = "Axe Chop"
        , Skill.desc      = "Slashing an enemy with the axe part of Kabutowari, Jinin deals 15 piercing damage, disables the countering effects of their skills, and prevents them from reducing damage or becoming invulnerable."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                apply 1 skillName
                    [ Expose
                    , Disable Counters
                    ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hammer Bash"
        , Skill.desc      = "Jinin slams the hammer part of Kabutowari into an enemy, dealing 25 damage. If the target is affected by [Axe Chop], they are prevented from reducing damage or becoming invulnerable for 4 turns."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 25
                whenM (target has "Axe Chop") $
                    apply 4 skillName [Expose]
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
                apply 1 skillName
                    [ Stun Chakra
                    , Stun Melee
                    ]
          ]
        }
      ]
    , [ invuln "Dodge" "Jinin" [Physical] ]
    ]
  , Character
    "Deidara" 100
    "Reanimated by Kabuto, Deidara was an Akatsuki member obsessed with art. His style remains largely as it was when he was alive, revolving around clay explosives that successively render its victims more vulnerable to subsequent explosions."
    [StoneVillage, Kabuto, Akatsuki, Rogue, SRank, Earth, Lightning]
    [ [ Skill.new
        { Skill.name      = "Detonating Clay"
        , Skill.desc      = "Deidara tosses an explosive clay bomb at an enemy, dealing 20 piercing damage and becoming invulnerable to mental skills for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonusA <- 5 `bonusPer` target amount "Chakra Clay Trap"
                bonusB <- 5 `bonusPer` target amount "Sonar Bat Bombs"
                bonusC <- 10 `bonusPer` target amount "Jellyfish Explosives"
                pierce (20 + bonusA + bonusB + bonusC)
          , To Self $ apply 1 skillName [Invulnerable Mental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Clay Trap"
        , Skill.desc      = "Deidara plants an explosive mine near an enemy. The next time they use a skill on Deidara or his allies, they will take 20 piercing damage and [Detonating Clay] will deal 5 additional damage to them. The trap does not stack."
        , Skill.classes   = [Chakra, Ranged, Invisible, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap Permanent skillName OnHarm do
                asAction $ pierce 20
                addStack skillName
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
                addStack skillName
          , To Self $ apply Permanent skillName
                [ Alternate "Sonar Bat Bombs"
                            "Jellyfish Explosives"
                ]
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
                addStack skillName
          , To Self $ remove "Sonar Bat Bombs"
          ]
        }
      ]
    , [ invuln "Clay Clone" "Deidara" [Chakra] ]
    ]
  , Character
    "Sasori" 100
    "Reanimated by Kabuto, Sasori was an Akatsuki member who crafted puppets from human corpses. Finally inhabiting the perfect, ageless body he strived for in life, Sasori uses his chakra threads to control enemies and allies alike."
    [SandVillage, Kabuto, Akatsuki, Rogue]
    [ [ Skill.new
        { Skill.name      = "Chakra Threads"
        , Skill.desc      = "Sasori defends himself by ensaring his enemies, gaining 10 points of damage reduction for 4 turns."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 4 skillName [Reduce [All] Flat 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ally Control"
        , Skill.desc      = "Sasori manipulates an ally with puppeteering threads. All non-ranged skills that enemies use on the target next turn will be reflected back at them."
        , Skill.classes   = [Physical, Invisible]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To XAlly $ apply 1 skillName [ReflectAll NonRanged] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Puppet Manipulation"
        , Skill.desc      = "Sasori attacks an enemy with chakra threads, dealing 15 damage and weakening their damage by 5 for 1 turn. If their health reaches 35 or lower, their physical and chakra skills are stunned for 1 turn. Deals 5 additional damage during [Chakra Threads]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` user has "Chakra Threads"
                damage (15 + bonus)
                targetHealth <- target health
                if targetHealth <= 35 then
                    apply 1 skillName
                        [ Weaken [All] Flat 5
                        , Stun Physical
                        , Stun Chakra
                        ]
                else
                    apply 1 skillName [Weaken [All] Flat 5]
          ]
        }
      ]
    , [ invuln "Block" "Sasori" [Physical] ]
    ]
  , Character
    "Nagato" 125
    "Reanimated by Kabuto, Nagato is as much a pawn in the schemes of others as he was in life. With the full power of the Rinnegan, all six Paths are at his disposal."
    [RainVillage, Kabuto, Akatsuki, Sensor, SRank, Fire, Wind, Lightning, Earth, Water, Yang, Uzumaki]
    [ [ Skill.new
        { Skill.name      = "Deva Path"
        , Skill.desc      = "Nagato creates a gravitational anchor that pulls in the enemy team and accumulates a rough sphere of rock and debris around them, applying 25 destructible barrier for 3 turns. While enemies have destructible barrier from this skill, the non-damage effects of their skills on allies and enemies are disabled. At the end of the 3 turns, enemies take damage equal to the remaining destructible barrier from this skill."
        , Skill.classes    = [Physical, Ranged]
        , Skill.cost       = [Blood, Gen, Tai]
        , Skill.cooldown   = 3
        , Skill.start      =
          [ To Enemies do
                barricade' 3 25 [Silence]
                onBreak do
                    remaining <- target amount' barrier "Deva Path"
                    when (remaining > 0) $
                        damage remaining
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Human Path"
        , Skill.desc      = "Nagato draws out the lifeforce of an enemy, revealing invisible effects from the target and the target's cooldowns. While active, this skill becomes [Naraka Path][g][r]."
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.dur       = Control Permanent
        , Skill.start     =
          [ To Enemy $ control [Reveal]
          , To Self $ control
                [Alternate "Human Path"
                           "Naraka Path"
                ]
          ]
        , Skill.effects   =
          [ To Enemy $ damage 15 ]
        }
      , Skill.new
        { Skill.name      = "Naraka Path"
        , Skill.desc      = "Judging the target of [Human Path] unworthy, the King of Hell absorbs 20 of their health and converts it into destructible defense."
        , Skill.require   = [TargetHas AtLeast 1 "Human Path"]
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To Enemy do
                defended <- user has' defense skillName
                leech 20 if defended then
                    increaseDefense skillName
                else
                    defend Permanent
          ]
        }
      ]
    , let preta = Skill.new
            { Skill.name      = "Preta Path"
            , Skill.desc      = "Nagato drains an enemy's energy, regaining 10 health per chakra that the target spent on their most recent skill and absorbing 3 random chakra. When Nagato's health is at or above 50, this skill becomes [Asura Path][t][r]."
            , Skill.classes   = [Melee, Chakra]
            , Skill.cooldown  = 1
            , Skill.effects   =
                [ To Enemy do
                    chakra <- target lastChakraSpent
                    targeting Self $ heal (10 * length chakra)
                    absorb 3
                ]
            }
      in
      [ Skill.new
        { Skill.name      = "Asura Path"
        , Skill.desc      = "Nagato unfolds an extra mechanical arm and seizes an enemy by the neck, preventing them from reducing damage or becoming invulnerable until one of their allies uses a skill on them, then deals 15 damage. When Nagato's health is below 50, this skill becomes [Preta Path][t][r]."
        , Skill.classes   = [Bane, Physical, Melee, Nonstacking]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                apply Permanent skillName [Expose]
                pierce 15
                trap Permanent skillName OnHelped do
                    removeTrap skillName
                    remove skillName
          ]
        , Skill.changes   = \n x -> if health n < 50 then
                                preta { Skill.owner = x.owner }
                            else
                                x
        }
      , preta
      ]
    , [ invuln "Animal Path" "Nagato" [Summon, Invisible] ]
    ]
  ]

reanimations :: [Skill]
reanimations =
    [ Skill.new
        { Skill.name    = "Jirōbō: Earth Dome Prison"
        , Skill.desc    = "Jirōbō encases an enemy in chakra-conductive rock and drains their energy, dealing 20 affliction damage."
        , Skill.classes = [Chakra, Melee]
        , Skill.effects =
          [ To Enemy $ afflict 20 ]
        }
    , Skill.new
        { Skill.name    = "Kimimaro: Digital Shrapnel"
        , Skill.desc    = "A volley of bullets shoot forth from Kimimaro's fingertips, providing his reanimator with 50% damage reduction for 1 turn. Next turn, enemies who use skills will take 20 damage."
        , Skill.classes = [Physical, Ranged]
        , Skill.effects =
          [ To Enemies $ trap 1 skillName (OnAction All) $ asAction $
                damage 20
          , To Self $ apply 1 "Digital Shrapnel" [Reduce [All] Percent 50]
          ]
        }
    , Skill.new
        { Skill.name    = "Haku: Acupuncture"
        , Skill.desc    = "Haku alters the flow of energy in an enemy by sticking a needle into one of their vital points, disabling the non-damage effects of their skills on allies and enemies for 2 turns."
        , Skill.classes = [Physical, Ranged]
        , Skill.effects =
          [ To Enemy $ apply 2 "Acupuncture" [Silence] ]
        }
    , Skill.new
        { Skill.name    = "Zabuza Momochi: Executioner's Butchering"
        , Skill.desc    = "Zabuza's sword carves into an enemy like the edge of a guillotine, dealing 30 piercing damage."
        , Skill.classes = [Physical, Melee]
        , Skill.effects =
          [ To Enemy $ pierce 30 ]
        }
    , Skill.new
        { Skill.name    = "Ameyuri Ringo: Thunder Gate"
        , Skill.desc    = "With the twin blades of Baki plunged into the ground, Ameyuri calls down lightning from the sky to incinerate the battlefield around an enemy, dealing 30 piercing damage to them."
        , Skill.classes = [Chakra, Ranged]
        , Skill.effects =
          [ To Enemy $ pierce 30 ]
        }
    , Skill.new
        { Skill.name    = "Kushimaru Kuriarare: Needle Stitching"
        , Skill.desc    = "Nuibari skewers an enemy and pulls a wire through them, dealing 20 piercing damage and preventing them from affecting his reanimator for 1 turn."
        , Skill.classes = [Chakra, Ranged]
        , Skill.effects =
          [ To Enemy do
                pierce 20
                userSlot <- user slot
                apply 1 "Needle Stitching" [Block userSlot]
          ]
        }
    , Skill.new
        { Skill.name    = "Jinpachi Munashi: Splatter"
        , Skill.desc    = "Jinpachi swings his sword at an enemy and detonates numerous paper bombs on contact, dealing 30 affliction damage. For 1 turn, the target receives 5 additional damage from non-affliction skills."
        , Skill.classes = [Chakra, Melee, Bane]
        , Skill.effects =
          [ To Enemy do
                afflict 30
                apply 1 "Splatter" [Bleed [NonAffliction] Flat 5]
          ]
        }
    , Skill.new
        { Skill.name    = "Fuguki Suikazan: Sharp Hair Spear"
        , Skill.desc    = "Fuguki extends his hair to skewer enemies around him, dealing 10 damage to the enemy team and stunning their non-mental skills for 1 turn."
        , Skill.classes = [Physical, Ranged]
        , Skill.effects =
          [ To Enemies do
                damage 10
                apply 1 "Sharp Hair Spear" [Stun NonMental]
          ]
        }
    , Skill.new
        { Skill.name    = "Jinin Akebino: Axe Chop"
        , Skill.desc    = "Slashing an enemy with the axe part of Kabutowari, Jinin deals 15 piercing damage, disables the countering effects of their skills, and prevents them from reducing damage or becoming invulnerable."
        , Skill.classes = [Physical, Melee]
        , Skill.effects =
          [ To Enemy do
                pierce 15
                apply 1 "Axe Chop"
                    [ Expose
                    , Disable Counters
                    ]
          ]
        }
    , Skill.new
        { Skill.name    = "Deidara: Detonating Clay"
        , Skill.desc    = "Deidara tosses an explosive clay bomb at an enemy, dealing 20 piercing damage and making his reanimator invulnerable to mental skills for 1 turn."
        , Skill.classes = [Chakra, Ranged]
        , Skill.effects =
          [ To Enemy $ pierce 20
          , To Self $ apply 1 "Detonating Clay" [Invulnerable Mental]
          ]
        }
    , Skill.new
        { Skill.name    = "Sasori: Ally Control"
        , Skill.desc    = "Sasori manipulates an ally with puppeteering threads. All non-ranged skills that enemies use on the target next turn will be reflected back at them."
        , Skill.classes = [Physical, Invisible]
        , Skill.effects =
          [ To XAlly $ apply 1 "Ally Control" [ReflectAll NonRanged] ]
        }
    , Skill.new
        { Skill.name    = "Nagato: Naraka Path"
        , Skill.desc    = "The King of Hell absorbs 20 of an enemy's health and converts it into destructible defense."
        , Skill.classes = [Mental, Ranged]
        , Skill.effects =
          [ To Enemy $ leech 20 $ defend Permanent ]
        }
    ]
