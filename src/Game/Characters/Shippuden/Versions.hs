{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Shippuden.Versions (characters) where

import Game.Characters.Import

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Sage Mode Naruto"
    "Naruto has trained on Mount Myōboku with Fukasaku and Shima, the Two Great Sage Toads. He has learned to absorb natural energy and use it to empower his attacks or heal himself."
    [LeafVillage, Eleven, AlliedForces, Genin, Jinchuriki, Sage, Sensor, Wind, Lightning, Earth, Water, Fire, Yin, Yang, Uzumaki]
    [ [ Skill.new
        { Skill.name      = "Frog Kumite"
        , Skill.desc      = "Surrounded by a field of natural energy that extends the range of his attacks, Naruto deals 20 damage to an enemy and stuns their physical and melee skills for 1 turn. Once used, this skill becomes [Rasen Shuriken][n][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Stun Physical, Stun Melee]
          , To Self $ hide Permanent [Alternate "Frog Kumite" "Rasen Shuriken"]
          ]
        }
      , Skill.new
        { Skill.name      = "Rasen Shuriken"
        , Skill.desc      = "A vortex of microscopic wind-chakra blades destroys an enemy's chakra circulatory system at a cellular level, dealing 50 piercing damage and weakening their chakra damage by 10%. Once used, this skill becomes [Frog Kumite][t]."
        , Skill.classes   = [Bane, Chakra, Ranged, Uncounterable]
        , Skill.cost      = [Nin, Tai]
        , Skill.effects   =
          [ To Enemy do
                pierce 50
                apply Permanent [Weaken [Chakra] Percent 10]
          , To Self $ remove "frog kumite"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sage Mode"
        , Skill.desc      = "Naruto holds still and absorbs natural energy from his surroundings to replenish his chakra, restoring 25 health."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ heal 25 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Natural Energy Assault"
        , Skill.desc      = "Naruto attacks all enemies with energy, disabling the stuns and disabling effects of their skills for 1 turn. Once used, this skill becomes [Rasengan Barrage][n][r]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemies $ apply 1 [Disable Stuns]
          , To Self $ hide Permanent
                [Alternate "Natural Energy Assault" "Rasengan Barrage"]
          ]
        }
      , Skill.new
        { Skill.name      = "Rasengan Barrage"
        , Skill.desc      = "The first enemy who uses a skill on Naruto next turn will be countered and take 30 damage. Once used, this skill becomes [Natural Energy Assault][r]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Self do
                trapFrom 1 (Counter All) $ damage 30
                remove "natural energy assault"
          ]
        }
      ]
    , [ invuln "Parry" "Naruto" [Physical] ]
    ]{-
  , Character
    "Proxy Commander Shikamaru"
    "TODO"
    [LeafVillage, Eleven, AlliedForces, Chunin, Fire, Earth, Yin, Nara]
    let
        formation :: (Int -> RunConstraint()) -> RunConstraint ()
        formation withAmount = do
            formingStacks <- user $ numAnyStacks "forming"
            if formingStacks > 0 then
                allies $ addStack' "formed"
            else do
                allies $ addStack' "forming"
                delay -1 do
                    formedStacks <- user $ numAnyStacks "formed"
                    when (formedStacks > 0) $ withAmount formedStacks
    in
    [ [ Skill.new
        { Skill.name      = "Team Formation"
        , Skill.desc      = "TODO"
        , Skill.classes   = [All, Mental]
        , Skill.effects   =
          [ To Self $ allies do
                targetSlot      <- target slot
                targetNumSkills <- target numSkills
                let skill = case toInt targetSlot `rem` teamSize of
                                0 -> "Formation C"
                                1 -> "Formation D"
                                _ -> "Formation E"
                teach 1 skill [targetNumSkills - 1]
          ]
        }
      , Skill.new
        { Skill.name      = "Formation C"
        , Skill.effects   =
          [ To Self $ formation \i -> return () ]
        }
    ] ] -}
  , Character
    "Regimental Commander Gaara"
    "Coordinating the Allied Shinobi Forces and personally commanding the Fourth Division, Gaara has proven to be an inspiring leader and talented strategist. His attacks scatter sand particles around the battlefield, which he draws back in with explosive force."
    [SandVillage, AlliedForces, Kage, Jinchuriki, Sensor, Wind, Earth, Lightning, SandClan]
    [ [ Skill.new
        { Skill.name      = "Sand Grasp"
        , Skill.desc      = "Gaara grabs an enemy with sand, first adding a Sand Bomb to them and then dealing 10 damage. Deals 5 additional damage per Sand Bomb on the target. Has no chakra cost during [Sand Mausoleum Seal]. Targets all enemies during [Mother's Embrace]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                stacks <- targetStacks "Sand Bomb"
                damage (15 + 5 * stacks)
                apply' "Sand Bomb" Permanent []
          ]
        , Skill.changes   =
            changeWithDefense "Mother's Embrace" targetAll `also`
            changeWith "Sand Mausoleum Seal" \x -> x { Skill.cost = [] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mother's Embrace"
        , Skill.desc      = "The soul of Gaara's deceased mother protects him with a shield of sand, providing 40 destructible defense for 3 turns. As long as Gaara has destructible defense from this skill, he ignores harmful status effects."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                apply 3 [Enrage]
                defend 3 50
                onBreak'
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Mausoleum Seal"
        , Skill.desc      = "Dense layers of sand entomb Gaara's enemies in a giant pyramid, dealing 15 damage to all enemies for 3 turns and increasing the costs of their skills by 1 arbitrary chakra. Each turn, deals 5 additional damage to each enemy per Sand Bomb on them and removes all Sand Bombs."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemies do
                stacks <- targetStacks "Sand Bomb"
                damage (15 + 5 * stacks)
                apply 1 [Exhaust [All]]
          , To Everyone $ remove "Sand Bomb"
          ]
        }
      ]
    , [ invuln "Sand Shield" "Gaara" [Physical] ]
    ]
  , Character
    "Puppet Master Kankurō"
    "After defeating Sasori, Kankurō considers himself one of the greatest puppeteers in history. Adding Sasori's body to his collection of puppets, Kankurō uses each puppet for a different purpose."
    [SandVillage, AlliedForces, Jonin, Wind, Lightning, Earth, Water, SandClan]
    [ [ Skill.new
        { Skill.name      = "Sasori Surrogate"
        , Skill.desc      = "Sasori's puppet body attacks an enemy, dealing 15 damage to them for 3 turns. While active, this skill becomes [Hidden Coil Strike][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemy $ damage 15
          , To Self $ hide 1 [Alternate "Sasori Surrogate" "Hidden Coil Strike"]
          ]
        , Skill.stunned   =
          [ To Self $
                hide 1 [Alternate "Sasori Surrogate" "Hidden Coil Strike"]
          ]
        }
      , Skill.new
        { Skill.name      = "Hidden Coil Strike"
        , Skill.desc      = "Kankurō hooks an enemy with the coil hidden in Sasori's body and pulls the target to him, dealing 10 piercing damage. For 1 turn, the target can only target Kankurō or themselves."
        , Skill.classes   = [Physical, Ranged, Bypassing, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                pierce 10
                userSlot <- user slot
                apply 1 [Taunt userSlot]
                remove "Kuroari Trap"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kuroari Trap"
        , Skill.desc      = "Kankurō's Kuroari puppet stalks an enemy for 5 turns. If Kankurō uses [Hidden Coil Strike] on the target, the trap is activated immediately; otherwise, it is activated at the end of the 5 turns. Activating the trap applies [Kuroari Ambush] to the target, stunning them for 1 turn and making them invulnerable to everyone but Kankurō. Once used, this skill becomes [Iron Maiden][r][r][r]."
        , Skill.classes   = [Physical, Ranged, Nonstacking, Invisible, Bypassing, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self $ hide Permanent [Alternate "Kuroari Trap" "Iron Maiden"]
          , To Enemy $ bomb 5 []
                [ To Done do
                    userSlot <- user slot
                    apply' "Kuroari Ambush" 1 [Stun All, Alone, Duel userSlot]
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Iron Maiden"
        , Skill.desc      = "Kankurō's Karasu puppet snaps shut around an enemy, dealing 20 piercing damage and 40 additional damage if the target is affected by [Kuroari Ambush]. Once used, this skill becomes [Kuroari Trap][r]."
        , Skill.classes   = [Physical, Ranged, Uncounterable, Unreflectable]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
            [ To Enemy do
                  bonus <- 40 `bonusIf` targetHas "Kuroari Ambush"
                  pierce (20 + bonus)
            , To Self $ remove "kuroari trap"
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Salamander Shield"
        , Skill.desc      = "Kankurō's Sanshōuo puppet shields him and his allies, providing Kankurō with 40 permanent destructible defense. While Kankurō has destructible defense from this skill, damage against his allies is reflected to him. Cannot be used while active."
        , Skill.classes   = [Physical, Soulbound, Unremovable, Unreflectable]
        , Skill.require   = DefenseI 0 "Salamander Shield"
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                defend Permanent 40
                onBreak'
          , To XAllies do
                userSlot <- user slot
                apply Permanent [Redirect userSlot]
          ]
        }
      ]
    , [ invuln "Puppet Distraction" "Kankurō" [Physical] ]
    ]
  , let
        rename "Sage Transformation" = "Bloodline Sage"
        rename "Bloodline Sage" = "Genjutsu Sage"
        rename "Genjutsu Sage"  = "Ninjutsu Sage"
        rename "Ninjutsu Sage"  = "Taijutsu Sage"
        rename "Taijutsu Sage"  = "Bloodline Sage"
        rename x                = x

        withMode f n x
          | isChanneling "Bloodline Sage" n = f Blood x
          | isChanneling "Genjutsu Sage"  n = f Gen x
          | isChanneling "Ninjutsu Sage"  n = f Nin x
          | isChanneling "Taijutsu Sage"  n = f Tai x
          | otherwise                       = f Rand x
    in
    Character
    "Sage Mode Kabuto"
    "Unable to find an identity of his own, Kabuto has spent his life taking on the traits of others. Years of research and experiments upon himself have reached their conclusion, and now Kabuto prepares for his final metamorphosis."
    [LeafVillage, Rogue, Sage, TeamLeader, Earth, Water, Wind, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Sage Transformation"
        , Skill.desc      = "By synthesizing rare genetic traits from other bloodlines inside his body, Kabuto becomes attuned to the flow of natural energy. Each turn, the chakra costs and type of chakra gained from his other skills cycle through the different types of chakra. Once used, this skill becomes [DNA Transmission Shadow][r][r][r]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.dur       = Ongoing Permanent
        , Skill.start     =
          [ To Self $ hide Permanent [Alternate "Sage Transformation"
                                                "DNA Transmission Shadow"]
          ]
        , Skill.effects   =
          [ To Self $ delay -1 $ renameChannels rename ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. At the end of the next turn, the target comes back to life at full health, removing all effects from them and resetting their cooldowns. The clone remains attached to Kabuto and will be destroyed if he dies. Using this skill again destroys the current clone."
        , Skill.require   = HealthU 0
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.dur       = Control -2
        , Skill.start     =
          [ To Self do
                flag
                everyone $
                    whenM (targetHas "DNA Transmission Shadow") killHard
                trap' Permanent OnDeath $ everyone $
                    whenM (targetHas "DNA Transmission Shadow") killHard
          ]
        , Skill.effects   =
          [ To XAlly $ unlessM (userHas "dna transmission shadow") do
                factory
                tag Permanent
          ]
        , Skill.interrupt  =
          [ To Self $ remove "dna transmission" ]
        , Skill.changes   =
            withMode \m x -> x { Skill.cost = [m, m, m] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, Kabuto's skills are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                trap' -1 OnDamage rechargeAll
                enemies $ apply 1 [Restrict]
          , To Enemies $ damage 10
          ]
        , Skill.changes   =
            withMode \m x -> x { Skill.cost = [m] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns, resetting the target's cooldowns, and curing them of bane effects. While being healed, the target is invulnerable to bane skills."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Ally do
                resetAll
                cureBane
                apply 3 [Heal 15, Invulnerable Bane]
          ]
        , Skill.changes   =
            withMode \m x ->
                x { Skill.effects = To Self (gain [m]) : Skill.effects x
                  , Skill.desc    = Skill.desc x ++ " Kabuto gains 1 "
                                    ++ chakraDesc m ++ " chakra."
                  }
        }
      ]
    , [ Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth that explodes in a flash of light and stuns all allies and enemies for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To XAllies $ apply 1 [Stun All]
          , To Enemies $ apply 1 [Stun All]
          ]
        , Skill.changes   =
            withMode \m x ->
                x { Skill.effects = To Self (gain [m, m]) : Skill.effects x
                  , Skill.desc    = Skill.desc x ++ " Kabuto gains 2 "
                                    ++ chakraDesc m ++ " chakra."
                  }
        }
      ]
    ]
  , Character
    "Eight-Gates Guy"
    "With the fate of the world at stake, Guy has opened all eight Gates and is holding nothing back. The effort will surely kill him, but while he lives, his strength outmatches even the legendary Madara Uchiha."
    [LeafVillage, AlliedForces, Jonin, TeamLeader, Fire, Lightning]
    [ [ Skill.new
        { Skill.name      = "Evening Elephant"
        , Skill.desc      = "Using a devastating sequence of punches, Guy deals 20 damage to an enemy. For 1 turn, they are invulnerable to allies and their non-mental skills are stunned. Guy loses 20 health down to a minimum of 1. Every time this skill is used, its damage increases by 20 and its cost increases by 1 additional arbitrary chakra."
        , Skill.classes   = [Physical, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Evening Elephant"
                damage (20 + 20 * stacks)
                apply 1 [Alone, Stun NonMental]
          , To Self do
                sacrifice 1 20
                addStack
          ]
        , Skill.changes   =
            costPer "Evening Elephant" [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Battle Stance"
        , Skill.desc      = "Next turn, Guy deals double damage and ignores harmful status effects. Guy loses 10 health down to a minimum of 1."
        , Skill.classes   = [Physical, Unremovable]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                apply 1 [Enrage, Strengthen [All] Percent 100]
                sacrifice 1 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Night Guy"
        , Skill.desc      = "As his blood evaporates into mist around him, Guy warps time and space to instantly attack an enemy, dealing 50 piercing damage. For 2 turns, the target ignores helpful effects, their damage is weakened by 5, and Guy cannot be healed. Guy loses 30 health down to a minimum of 1. Every time this skill is used, its damage increases by 25 and its cost increases by 1 taijutsu chakra."
        , Skill.classes   = [Physical, Melee, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Tai, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Night Guy"
                pierce (50 + 25 * stacks)
                apply 2 [Seal, Weaken [All] Flat 5]
          , To Self do
                sacrifice 1 30
                addStack
                apply' "Blood Mist" 2 [Plague]
          ]
        , Skill.changes   =
            costPer "Night Guy" [Tai]
        }
      ]
    , [ invuln "Dodge" "Guy" [Physical] ]
    ]
  , Character
    "Curse Mark Jūgo"
    "No longer recognizably human, Jūgo has been transformed by bloodlust into a terrifying monster. Tapping into limitless chakra, he is an unstoppable and uncontrollable force."
    [Orochimaru, Sage, Wind, Earth, Water, Yang]
    [ [ Skill.new
        { Skill.name      = "Psychotic Break"
        , Skill.desc      = "Jūgo fixates obsessively on an enemy, dealing 10 damage to them for 3 turns and gaining 20% damage reduction."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemy $ damage 10
          , To Self $ apply 1 [Reduce [All] Percent 20]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Connected Cannons"
        , Skill.desc      = "A powerful chakra blast deals 50 damage to an enemy."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ damage 50 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Cellular Absorption"
        , Skill.desc      = "Jūgo drains the lifeforce from an enemy with a needle-like appendage, stealing 15 health from them."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ leech 15 $ self . heal ]
        }
      ]
    , [ invuln "Block" "Jūgo" [Physical] ]
    ]
  , Character
    "Mangekyō Sasuke"
    "The trauma of Itachi's death has awakened Sasuke's Mangekyō Sharingan. With it, he has access to the most powerful techniques of the Uchiha clan. Although his sibling rivalry is at an end, Sasuke's need for vengeance has only grown stronger."
    [LeafVillage, Orochimaru, Orochimaru, Genin, Rogue, Lightning, Fire, Wind, Earth, Water, Yin, Uchiha]
    [ [ Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Using the mangekyō sharingan's signature ability, Sasuke creates a colossus of chakra around himself. For 3 turns, all damage to Sasuke—including piercing and affliction—is reduced by 15 points."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ apply 3 [ Reduce [Affliction] Flat 15
                              , Alternate "Chidori" "Blazing Arrow"
                              , Alternate "Amaterasu" "Yasaka Beads"
                              , Face
                              ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Sasuke hurls lightning energy at an enemy, dealing 20 piercing damage and stunning their melee skills for 1 turn. Next turn, Sasuke gains 15 points of physical damage reduction. If no physical skills are used on Sasuke by the end of the turn, the cost of this skill becomes 1 ninjutsu chakra and its cooldown resets. During [Susanoo], this skill becomes [Blazing Arrow][b][r]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                pierce 20
                apply 1 [Stun All]
          , To Self do
                trap -1 (OnDamaged Physical) $ remove "Chidori"
                bomb -1 [Reduce [Physical] Flat 15]
                    [ To Expire do
                          hide 1 []
                          reset "Chidori" ]
          ]
        , Skill.changes   =
            changeWith "Chidori" \x -> x { Skill.cost = [Nin] }
        }
      , Skill.new
        { Skill.name      = "Blazing Arrow"
        , Skill.desc      = "Sasuke forges three arrows out of flame and shoots them one after another at an enemy, dealing 15 damage for 3 turns. If this skill is stunned, Sasuke deals the remaining damage instantly and the cooldown of this skill resets."
        , Skill.classes   = [Bane, Chakra, Ranged, Resource]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.start     =
          [ To Self do
                remove "Blazing Arrow"
                addStacks "Blazing Arrow" 3
          ]
        , Skill.effects   =
          [ To Enemy $ damage 15
          , To Self $ removeStack "Blazing Arrow"
          ]
        , Skill.stunned   =
          [ To Enemy do
                stacks <- userStacks "Blazing Arrow"
                damage (15 * stacks)
          , To Self do
                remove "Blazing Arrow"
                cancelChannel "Blazing Arrow"
                reset "Blazing Arrow"
          ]
        , Skill.interrupt  =
          [ To Enemy do
                stacks <- userStacks "Blazing Arrow"
                damage (15 * stacks)
          , To Self do
                remove "Blazing Arrow"
                reset "Blazing Arrow"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amaterasu"
        , Skill.desc      = "Sasuke sets an enemy on fire, dealing 5 affliction damage to them until they become invulnerable. If an ally of the target uses a skill on them while they are affected, [Amaterasu] will spread to that ally. Every time [Amaterasu] ends on an enemy, the damage of [Yasaka Beads] increases by 5. During [Susanoo], this skill becomes [Yasaka Beads][n]."
        , Skill.classes   = [Bane, Chakra, Ranged, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          let
              amaterasu :: RunConstraint ()
              amaterasu = do
                  trapWith [Bypassing] Permanent OnInvulnerable do
                      remove "Amaterasu"
                      removeTrap "Amaterasu"
                  bombWith [Bypassing] Permanent [Afflict 5]
                      [ To Done $ self $ addStack ]
                  trapFrom Permanent OnHelped amaterasu
          in
          [ To Enemy amaterasu ]
        }
      , Skill.new
        { Skill.name      = "Yasaka Beads"
        , Skill.desc      = "Sasuke attacks an enemy with a Magatama of black flame, dealing 10 affliction damage. Damage increases by 5 every time an enemy is cured of [Amaterasu]. If the target uses a skill next turn, they take 20 additional affliction damage."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Amaterasu"
                afflict (10 + 5 * stacks)
                trap 1 (OnAction All) $ afflict 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mangekyō Foresight"
        , Skill.desc      = "Sasuke becomes invulnerable for 1 turn. Extends the duration of [Susanoo] by 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                apply 1 [Invulnerable All]
                prolong 1 "Susanoo"

          ]
        }
      ]
    ]
  , Character
    "True Form Sasori"
    "Having invented and perfected the art of human puppetry, Sasori accomplished its ultimate act: transforming himself into a living puppet. His immortal core now resides in an unnaturally youthful simulacrum filled to the brim with tools of slaughter, each of which he switches out for another as soon as he uses it."
    [SandVillage, Rogue]
    [ [ Skill.new
        { Skill.name      = "Poisonous Chain Skewer"
        , Skill.desc      = "Sasori hooks an enemy with the poison-soaked steel ropes inside his body and pulls himself to them, dealing 5 affliction damage for 3 turns. Next turn, the target can only target Sasori or themselves. Once used, this skill becomes [Impale][t]."
        , Skill.classes   = [Physical, Bane, Ranged, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                apply 3 [Afflict 5]
                userSlot <- user slot
                apply 1 [Taunt userSlot]
          , To Self $ hide Permanent
                [Alternate "Poisonous Chain Skewer" "Impale"]
          ]
        }
      , Skill.new
        { Skill.name      = "Impale"
        , Skill.desc      = "Sasori stabs an enemy with a poison-soaked blade, dealing 15 piercing damage immediately and 5 affliction damage for 2 turns. If the target is affected by [Poisonous Chain Skewer], they become affected by [Complex Toxin], which stuns them after 2 turns. Once used, this skill becomes [Poisonous Chain Skewer][r]."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Self $ remove "poisonous chain skewer"
          ,  To Enemy do
                  pierce 15
                  apply 2 [Afflict 5]
                  whenM (targetHas "Poisonous Chain Skewer") $
                      bomb' "Complex Toxin" 2 []
                            [ To Expire $ apply 1 [Stun All] ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flamethrower Jets"
        , Skill.desc      = "Using fuel stored in a sealing scroll, Sasori shoots flames at an enemy for 3 turns, dealing 10 affliction damage each turn. While active, Sasori is invulnerable to all other enemies and ignores harmful status effects. If Sasori uses any skill, [Flamethrower Jets] is canceled. After use, this skill becomes [Cutting Water Jets][n]."
        , Skill.classes   = [Bane, Physical, Ranged, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.start     =
          [ To Self do
                hide Permanent
                    [Alternate "Flamethrower Jets" "Cutting Water Jets"]
                flag' "first"
          ]
        , Skill.effects   =
          [ To Self do
                apply 1 [Enrage]
                unlessM (userHas "first") $ trap' 1 (OnAction All) do
                    cancelChannel "Flamethrower Jets"
                    everyone do
                        remove "Flame Blast"
                        remove "Flamethrower Jets"
          , To Enemy do
                afflict 10
                tag 1
                targetSlot <- target slot
                self $ apply' "Flame Blast" 1 [Duel targetSlot]
          ]
        }
      , Skill.new
        { Skill.name      = "Cutting Water Jets"
        , Skill.desc      = "Sasori shoots a high-pressure jet of water at an enemy, dealing 20 piercing damage. Deals 10 additional damage if the target is affected by [Flamethrower Jets]. Once used, this skill becomes [Flamethrower Jets][n][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` targetHas "Flamethrower Jets"
                pierce (20 + bonus)
          , To Self $ remove "flamethrower jets"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Performance of a Hundred Puppets"
        , Skill.desc      = "Proving his reputation as the greatest puppeteer in history, Sasori takes control of 100 puppets, each acting as pure extensions of his will. Sasori gains 50 permanent destructible defense and provides 25 permanent destructible defense to his allies. As long as Sasori has destructible defense from this skill, this skill becomes [Barrage of a Hundred Puppets][r][r]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Tai, Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
                hide Permanent [Alternate "Performance of a Hundred Puppets"
                                  "Barrage of a Hundred Puppets"]
                defend Permanent 50
                onBreak $ self $
                    remove "performance of a hundred puppets"
          , To XAllies $ defend Permanent 25
          ]
        }
      , Skill.new
        { Skill.name      = "Barrage of a Hundred Puppets"
        , Skill.desc      = "Sasori commands his puppet army to attack an enemy, dealing 30 damage and applying [Complex Toxin] to the target, which stuns them after 2 turns."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 30
                bomb' "Complex Toxin" 2 [] [ To Expire $ apply 1 [Stun All] ]
          , To Self do
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
          ]
        }
      ]
    , [ invuln "Heart Switch" "Sasori" [Physical] ]
    ]
  , Character
    "Konan of the Rain"
    "One of the founding members of Akatsuki, Konan has turned against her own organization in order to aid Naruto in his quest for peace. With Nagato dead, the young Uzumaki is her best hope for the future."
    [RainVillage, Akatsuki, Sensor, SRank, Wind, Earth, Water, Yang, Wind, Earth, Water, Yang]
    [ [ Skill.new
        { Skill.name      = "Paper Chakram"
        , Skill.desc      = "Konan hurls a razor-sharp disc at an enemy, dealing 35 piercing damage. Next turn, Konan ignores stuns and disabling effects."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 35
          , To Self $ apply 1 [Focus]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sacred Paper Emissary"
        , Skill.desc      = "Playing her trump card, Konan sets off countless explosive strips of paper disguised as an ocean. For 2 turns, her team gains 10 points of damage reduction and the enemy team's cooldowns are increased by 1 turn."
        , Skill.classes   = [Physical, Ranged, Bane]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Allies $ apply 2 [Reduce [All] Flat 10]
          , To Enemies $ apply 2 [Snare 1]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Paper Bomb"
        , Skill.desc      = "Konan sets off an explosive paper tag, dealing 15 damage to an enemy. Once used, this skill becomes [Paper Shuriken][n]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ hide Permanent [Alternate "Paper Bomb" "Paper Shuriken"]
          , To Enemy do
                stacks <- targetStacks "Paper Shuriken"
                damage (15 + 10 * stacks)
          ]
        }
      , Skill.new
        { Skill.name      = "Paper Shuriken"
        , Skill.desc      = "Konan attacks an enemy with a barrage of origami shuriken, dealing 20 piercing damage and increasing the damage of [Paper Bomb] to the target by 10. Once used, this skill becomes [Paper Bomb][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Self $ remove "paper bomb"
          , To Enemy do
                pierce 20
                addStack
          ]
        }
      ]
    , [ invuln "Paper Clone" "Konan" [Chakra] ]
    ]
  , Character
    "White Snake Orochimaru"
    "Orochimaru has cast off his body and revealed his true form as a giant serpent. Making use of the power he was granted by the White Sage Snake of Ryūchi Cave, Orochimaru transcends life and death in his endless hunger for knowledge and power."
    [LeafVillage, Orochimaru, Sannin, Rogue, TeamLeader, Wind, Lightning, Earth, Water, Fire, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Regenerative Bite"
        , Skill.desc      = "Orochimaru snaps his jaws around an enemy and steals 35 health from them. If Orochimaru acquires a new body, this skill becomes [Kusanagi][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   =
          [ To Enemy $ leech 35 $ self . heal ]
        }
      , Skill.new
        { Skill.name      = "Kusanagi"
        , Skill.desc      = "Spitting out his legendary sword, Orochimaru destroys an enemy's destructible defense and his own destructible barrier, then deals 30 piercing damage to the target."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                demolishAll
                pierce 30
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Immortality Transference"
        , Skill.desc      = "Orochimaru forces his soul on an enemy, dealing 15 damage to them for 3 turns and stunning their non-mental skills. If the target dies while affected by this skill, Orochimaru regains all lost health. If Orochimaru acquires a new body, this skill becomes [Eight-Headed Serpent][b][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen, Nin]
        , Skill.cooldown  = 3
        , Skill.dur       = Control 3
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 1 [Stun NonMental]
                trap 1 OnDeath $ self $ setHealth 100
          ]
        }
      , Skill.new
        { Skill.name      = "Eight-Headed Serpent"
        , Skill.desc      = "Orochimaru transforms into a colossal snake with eight heads and eight tails and deals 20 damage to all enemies for 3 turns. While active, Orochimaru ignores stuns and disabling effects, and enemies who stun him will take 20 damage and be stunned for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemies $ damage 20
          , To Self do
                apply 1 [Focus]
                trapFrom 1 OnStunned do
                    damage 20
                    apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name    = "Curse Mark Release"
        , Skill.desc    = "By giving an ally a curse mark, Orochimaru uses their body as an anchor for his soul after death. If the target's health reaches 25 or lower while Orochimaru is dead, Orochimaru will be resurrected into their body with full health and all status effects removed, and will become invulnerable to bane skills. Cannot be used while active. If Orochimaru acquires a new body, this skill becomes [Regeneration][g][n]."
        , Skill.require = HasI 0 "curse"
        , Skill.classes = [Physical, Unremovable, Bypassing, Uncounterable, Unreflectable, Invisible, Melee]
        , Skill.cost    = [Blood, Nin]
        , Skill.effects =
          [ To Ally do
                self $ hide Permanent []
                bomb Permanent []
                    [ To Done $ self $ remove "curse mark release" ]
                trap' Permanent (OnDamaged All) $ unlessM (user alive) do
                    targetHealth <- target health
                    when (25 >= targetHealth && targetHealth > 0) do
                        killHard
                        self do
                            setHealth 100
                            alternate [0, 0, 0, 0] 1
                            apply Permanent [Invulnerable Bane]
          ]
        }
      , Skill.new
        { Skill.name      = "Regeneration"
        , Skill.desc      = "Orochimaru regrows his form from the body of his ally, restoring 25 health to himself for 4 turns."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Gen, Nin]
        , Skill.cooldown  = 5
        , Skill.dur       = Action 4
        , Skill.effects   =
          [ To Self $ heal 25 ]
        }
      ]
    , [ invuln "Snake Wall" "Orochimaru" [Physical]
      , invuln "Snake Scales" "Orochimaru" [Physical]
      ]
    ]
  , Character
    "Sage Mode Jiraiya"
    "By fusing with the Toad Sages and absorbing natural energy, Jiraiya has enhanced his speed, strength, and skills. Unfortunately, the process gives him a distinctly toady appearance, which does no good for the lecherous sage's chances with women."
    [LeafVillage, Sannin, Sage, TeamLeader, Fire, Wind, Earth, Water, Yin, Yang]
    [ [ Skill.new
        { Skill.name      = "Bath of Burning Oil"
        , Skill.desc      = "Using a mixture of wind, oil, and fire, Jiraiya deals 20 damage and 15 piercing damage to an enemy. For 1 turn, counters applied by the target will have their duration reduced by 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                pierce 15
                apply 1 [Throttle 1 Counters]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Needle Senbon"
        , Skill.desc      = "Jiraiya infuses his hair with chakra to turn it needle-sharp, then deals 15 damage to all enemies and becomes invulnerable to melee skills for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ damage 15
          , To Self $ apply 1 [Invulnerable Melee]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Massive Rasengan"
        , Skill.desc      = "Jiraiya hits an enemy with an enormous orb of chakra, dealing 45 damage and preventing them from reducing damage or becoming invulnerable for 1 turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 45
                apply 1 [Expose]
          ]
        }
      ]
    , [ invuln "Block" "Jiraiya" [Physical] ]
    ]
  ]
