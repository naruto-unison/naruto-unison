{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Versions (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Mangekyō Sasuke"
    "The trauma of Itachi's death has awakened Sasuke's Mangekyō Sharingan. With it, he has access to the most powerful techniques of the Uchiha clan. Although his sibling rivalry is at an end, Sasuke's need for vengeance has only grown stronger."
    [ [ Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Sasuke encases himself in spectral armor that provides him with 20 points of damage reduction for 1 turn."
        , Skill.classes   = [Chakra, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ apply 1 [Reduce All Flat 20] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amaterasu"
        , Skill.desc      = "Sasuke shapes the black flames of Amaterasu into an inferno that surrounds him. For 3 turns, if an enemy uses a non-mental skill on Sasuke, they will receive 5 affliction damage every turn for 5 turns, stacking."
        , Skill.classes   = [Chakra, Bane]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self $ trapFrom 3 (OnHarmed NonMental) $ apply 5 [Afflict 5] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dark Genjutsu"
        , Skill.desc      = "Trapping an enemy in a powerful illusion, Sasuke deals 25 piercing damage and stuns the target's physical and melee skills for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 25
                apply 1 [Stun Physical, Stun Melee]
          ]
        }
      ]
    , [ invuln "Summoning: Hawk" "Sasuke" [Summon] ]
    ]
  , Character
    "Commander Gaara"
    "Coordinating the Allied Shinobi Forces and personally commanding the Fourth Division, Gaara has proven to be an inspiring leader and talented strategist. His attacks scatter sand particles around the battlefield, which he draws back in with explosive force."
    [ [ Skill.new
        { Skill.name      = "Sand Grasp"
        , Skill.desc      = "Gaara grabs an enemy with sand, first adding a Sand Bomb to them and then dealing 10 damage. Deals 5 additional damage per Sand Bomb on the target. Has no chakra cost during [Sand Mausoleum Seal]. Targets all enemies during [Mother's Embrace]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                stacks <- targetStacks "Sand Bomb"
                damage (15 + 5 * stacks)
                apply' "Sand Bomb" 0 []
          ]
        , Skill.changes   =
            changeWithDefense "Mother's Embrace" targetAll `also`
            changeWith "Sand Mausoleum Seal" \x -> x { Skill.cost = [] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mother's Embrace"
        , Skill.desc      = "The soul of Gaara's deceased mother protects him with a shield of sand, providing 40 destructible defense for 3 turns. As long as Gaara has destructible defense from this skill, he ignores status effects from enemies except chakra cost changes."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                defend 3 50
                onBreak'
                apply 3 [Enrage]
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
                apply 1 [Exhaust All]
          , To Everyone $ remove "Sand Bomb"
          ]
        }
      ]
    , [ invuln "Sand Shield" "Gaara" [Physical] ]
    ]
  , Character
    "Puppet Master Kankurō"
    "After defeating Sasori, Kankurō considers himself one of the greatest puppeteers in history. Adding Sasori's body to his collection of puppets, Kankurō uses each puppet for a different purpose."
    [ [ Skill.new
        { Skill.name      = "Sasori Surrogate"
        , Skill.desc      = "Sasori's puppet body attacks an enemy, dealing 15 damage to them for 3 turns. While active, this skill becomes [Hidden Coil Strike][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 3
        , Skill.start     =
          [ To Self $ vary "Sasori Surrogate" "Hidden Coil Strike" ]
        , Skill.effects   =
          [ To Enemy $ damage 15 ]
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
          [ To Self  $ vary "Kuroari Trap" "Iron Maiden"
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
            , To Self $ vary "Kuroari Trap" baseVariant
            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Salamander Shield"
        , Skill.desc      = "Kankurō's Sanshōuo puppet shields him and his allies, providing 40 permanent destructible defense to Kankurō. While Kankurō has destructible defense from this skill, damage against his allies is reflected to him. Cannot be used while active."
        , Skill.classes   = [Physical, Soulbound, Unremovable, Unreflectable]
        , Skill.require   = DefenseI (-1) "Salamander Shield"
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ To Self do
                defend 0 40
                onBreak'
          , To XAllies do
                userSlot <- user slot
                apply 0 [Redirect All userSlot]
          ]
        }
      ]
    , [ invuln "Puppet Distraction" "Kankurō" [Physical] ]
    ]
  , Character
    "Sage Mode Kabuto"
    "Unable to find an identity of his own, Kabuto has spent his life taking on the traits of others. Years of research and experiments upon himself have reached their conclusion, and now Kabuto prepares for his final metamorphosis."
    [ [ Skill.new
        { Skill.name      = "Sage Transformation"
        , Skill.desc      = "By synthesizing rare genetic traits from other bloodlines inside his body, Kabuto becomes attuned to the flow of natural energy. Each turn, the chakra costs and type of chakra gained from his other skills cycle through the different types of chakra. Once used, this skill becomes [DNA Transmission Shadow][r][r][r]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.dur       = Ongoing 0
        , Skill.effects   = [ To Self $ delay (-1) kabuto ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = [Rand, Rand, Rand]
        , Skill.dur       = Control 1
        , Skill.start     =
          [ To Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , To XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ To Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = [Blood, Blood, Blood]
        , Skill.dur       = Control 1
        , Skill.start     =
          [ To Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , To XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ To Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = [Gen, Gen, Gen]
        , Skill.dur       = Control 1
        , Skill.start     =
          [ To Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , To XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ To Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = [Nin, Nin, Nin]
        , Skill.dur       = Control 1
        , Skill.start     =
          [ To Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , To XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ To Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = [Tai, Tai, Tai]
        , Skill.dur       = Control 1
        , Skill.start     =
          [ To Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , To XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ To Self $ remove "dna" ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , To Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , To Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , To Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , To Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , To Enemies $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns, resetting the target's cooldowns, and curing them of bane effects. While being healed, the target is invulnerable to bane skills. Kabuto gains 1 random chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Ally do
                resetAll
                cureBane
                apply 3 [Heal 15, Invulnerable Bane]
          , To Self $ gain [Rand]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns, resetting the target's cooldowns, and curing them of bane effects. While being healed, the target is invulnerable to bane skills. Kabuto gains 1 bloodline chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Ally do
                resetAll
                cureBane
                apply 3 [Heal 15, Invulnerable Bane]
          , To Self $ gain [Blood]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns, resetting the target's cooldowns, and curing them of bane effects. While being healed, the target is invulnerable to bane skills. Kabuto gains 1 genjutsu chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Ally do
                resetAll
                cureBane
                apply 3 [Heal 15, Invulnerable Bane]
          , To Self $ gain [Gen]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns, resetting the target's cooldowns, and curing them of bane effects. While being healed, the target is invulnerable to bane skills. Kabuto gains 1 ninjutsu chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Ally do
                resetAll
                cureBane
                apply 3 [Heal 15, Invulnerable Bane]
          , To Self $ gain [Nin]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns, resetting the target's cooldowns, and curing them of bane effects. While being healed, the target is invulnerable to bane skills. Kabuto gains 1 taijutsu chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Ally do
                resetAll
                cureBane
                apply 3 [Heal 15, Invulnerable Bane]
          , To Self $ gain [Tai]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth that explodes in a flash of light and stuns all allies and enemies for 1 turn. Kabuto gains 2 random chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Self    $ gain [Rand, Rand]
          , To XAllies $ apply 1 [Stun All]
          , To Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth that explodes in a flash of light and stuns all allies and enemies for 1 turn. Kabuto gains 2 bloodline chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Self    $ gain [Blood, Blood]
          , To XAllies $ apply 1 [Stun All]
          , To Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth that explodes in a flash of light and stuns all allies and enemies for 1 turn. Kabuto gains 2 genjutsu chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Self    $ gain [Gen, Gen]
          , To XAllies $ apply 1 [Stun All]
          , To Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth that explodes in a flash of light and stuns all allies and enemies for 1 turn. Kabuto gains 2 ninjutsu chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Self    $ gain [Nin, Nin]
          , To XAllies $ apply 1 [Stun All]
          , To Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth that explodes in a flash of light and stuns all allies and enemies for 1 turn. Kabuto gains 2 taijutsu chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To Self    $ gain [Tai, Tai]
          , To XAllies $ apply 1 [Stun All]
          , To Enemies $ apply 1 [Stun All]
          ]
        }
      ]
    ]
  , Character
    "Eight-Gates Guy"
    "With the fate of the world at stake, Guy has opened all eight Gates and is holding nothing back. The effort will surely kill him, but while he lives, his strength outmatches even the legendary Madara Uchiha."
    [ [ Skill.new
        { Skill.name      = "Evening Elephant"
        , Skill.desc      = "Using a devastating sequence of punches, Guy deals 20 damage to an enemy. For 1 turn, they are invulnerable to allies and their non-mental skills are stunned. Guy loses 20 health down to a minimum of 1. Each time this skill is used, it permanently deals 20 additional damage and costs 1 additional arbitrary chakra."
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
        , Skill.desc      = "Next turn, Guy will deal double damage and ignores status effects from enemies except chakra cost changes. Guy loses 10 health down to a minimum of 1."
        , Skill.classes   = [Physical, Unremovable]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                sacrifice 1 10
                apply 1 [Enrage, Strengthen All Percent 200]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Night Guy"
        , Skill.desc      = "As his blood evaporates into mist around him, Guy warps time and space to instantly attack an enemy, dealing 50 piercing damage. For 2 turns, the target ignores helpful effects, their damage is weakened by 5, and Guy cannot be healed. Guy loses 30 health down to a minimum of 1. Each time this skill is used, it permanently deals 25 additional damage and costs 1 additional taijutsu chakra."
        , Skill.classes   = [Physical, Melee, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Tai, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Night Guy"
                pierce (50 + 25 * stacks)
                apply 2 [Seal, Weaken All Flat 5]
          , To Self do
                sacrifice 1 30
                addStack
                apply 2 [Plague]
          ]
        , Skill.changes   =
            costPer "Night Guy" [Tai]
        }
      ]
    , [ invuln "Dodge" "Guy" [Physical] ]
    ]
  , Character
    "True Form Sasori"
    "Having invented and perfected the art of human puppetry, Sasori accomplished its ultimate act: transforming himself into a living puppet. His immortal core now resides in an unnaturally youthful simulacrum filled to the brim with tools of slaughter, each of which he switches out for another as soon as he uses it."
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
          , To Self do
                vary "Poisonous Chain Skewer" "Impale"
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
          ]
        }
      , Skill.new
        { Skill.name      = "Impale"
        , Skill.desc      = "Sasori stabs an enemy with a poison-soaked blade, dealing 15 piercing damage immediately and 5 affliction damage for 2 turns. If the target is affected by [Poisonous Chain Skewer], they become affected by [Complex Toxin], which stuns them after 2 turns. Once used, this skill becomes [Poisonous Chain Skewer]."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Self do
                vary "Poisonous Chain Skewer" baseVariant
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
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
        , Skill.desc      = "Using fuel stored in a sealing scroll, Sasori shoots flames at an enemy for 3 turns, dealing 10 affliction damage each turn. While active, Sasori is invulnerable to all other enemies and ignores status effects from enemies except chakra cost changes. If Sasori uses any skill, [Flamethrower Jets] is canceled. After use, this skill becomes [Cutting Water Jets][n]."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.dur       = Action 3
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                afflict 10
                tag 1
                userSlot <- user slot
                self $ apply' "Flame Blast" 1 [Duel userSlot]
          , To Self do
                apply 1 [Enrage]
                vary "Flamethrower Jets" "Cutting Water Jets"
          ]
        }
      , Skill.new
        { Skill.name      = "Cutting Water Jets"
        , Skill.desc      = "Sasori shoots a high-pressure jet of water at an enemy, dealing 20 piercing damage. Deals 10 additional damage if the target is affected by [Flamethrower Jets]. Ends [Flamethrower Jets]. Once used, this skill becomes [Flamethrower Jets]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` targetHas "Flamethrower Jets"
                pierce (20 + bonus)
          , To Self do
                vary "Flamethrower Jets" baseVariant
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
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
                vary "Performance of a Hundred Puppets"
                     "Barrage of a Hundred Puppets"
                defend 0 50
                onBreak $ self $
                    vary "Performance of a Hundred Puppets" baseVariant
          , To XAllies $ defend 0 25
          ]
        }
      , Skill.new
        { Skill.name      = "Barrage of a Hundred Puppets"
        , Skill.desc      = "Sasori commands his puppet army to attack an enemy, dealing 30 damage and applying [Complex Toxin] to the target, which stuns them after 2 turns."
        , Skill.classes   = [Physical, Ranged]
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
    "Split Zetsu"
    "After Madara turned the Gedo statue's mutated victims into an army of servants, he chose one to lead them. Imbuing the White Zetsu entity with materialized will in the form of Black Zetsu, he created a hybrid being who became an official member of Akatsuki. White Zetsu and Black Zetsu have different approaches to combat, but both are able to take control of an enemy's abilities."
    [ [ Skill.new
        { Skill.name      = "White Zetsu"
        , Skill.desc      = "Zetsu's white half takes over, canceling [Black Zetsu]. While active, Zetsu gains 5 permanent destructible defense each turn. Once used, this skill becomes [Black Zetu]."
        , Skill.classes   = [Chakra]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self do
                cancelChannel "Black Zetsu"
                setFace
                vary "White Zetsu" "Black Zetsu"
                vary "Black Zetsu" "White Army"
                vary "Doppelgänger / Body Coating" "Doppelgänger"
          ]
        , Skill.effects   =
          [ To Self $ defend 0 5 ]
        }
      , Skill.new
        { Skill.name      = "Black Zetsu"
        , Skill.desc      = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains 1 random chakra every other turn. Once used, this skill becomes [White Zetsu]."
        , Skill.classes   = [Chakra]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self do
                cancelChannel "White Zetsu"
                setFace
                vary "White Zetsu" baseVariant
                vary "Black Zetsu" "Underground Roots"
                vary "Doppelgänger / Body Coating" "Body Coating"
          ]
        , Skill.effects   =
          [ To Self $ unlessM (userHas "chakra") do
                gain [Rand]
                hide' "chakra" 1 []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Black Zetsu"
        , Skill.desc      = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains 1 random chakra every other turn. Once used, this skill becomes [Underground Roots][b][r]. As White Zetsu, this skill becomes [White Army][g]."
        , Skill.classes   = [Chakra]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self do
                cancelChannel "White Zetsu"
                setFace
                vary "White Zetsu" baseVariant
                vary "Black Zetsu" "Underground Roots"
                vary "Doppelgänger / Body Coating" "Body Coating"
          ]
        , Skill.effects   =
          [ To Self $ unlessM (userHas "chakra") do
                gain [Rand]
                hide' "chakra" 1 []
          ]
        }
      , Skill.new
        { Skill.name      = "Underground Roots"
        , Skill.desc      = "Tree roots emerge from the ground and wrap around an enemy, dealing 20 damage for 2 turns. While active, the target's damage is weakened by half. As White Zetsu, this skill becomes [White Army][g]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Weaken All Percent 50]
          ]
        }
      , Skill.new
        { Skill.name      = "White Army"
        , Skill.desc      = "Zetsu creates numerous clones of himself which deal 5 damage to all enemies for 5 turns. As Black Zetsu, this skill becomes [Underground Roots][b][r]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen]
        , Skill.dur       = Ongoing 5
        , Skill.effects   =
          [ To Enemies $ damage 5 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Doppelgänger / Body Coating"
        , Skill.desc      = "Zetsu seizes an enemy and makes use of their abilities. As White Zetsu, this skill deals 20 damage, steals 1 random chakra, stuns their non-mental skill for 1 turn, and replaces itself with the last skill they used for 1 turn. As Black Zetsu, this skill causes the target's next reflectable non-unique skill to target allies instead of enemies and enemies instead of allies."
        , Skill.require   = Unusable
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        }
      , Skill.new
        { Skill.name      = "Body Coating"
        , Skill.desc      = "Zetsu melts and flows over an enemy, taking control of their body. The next skill they use will target allies instead of enemies and enemies instead of allies. As White Zetsu, this skill becomes [Doppelgänger][t][r]."
        , Skill.require   = HasU (-1) "Body Coating"
        , Skill.classes   = [Mental, Melee, Invisible, Unreflectable]
        , Skill.cost      = [Blood, Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                apply 0 [Swap]
                trap' 0 (OnAction All) do
                    remove "Body Coating"
                    removeTrap "Body Coating"
          ]
        }
      , Skill.new
        { Skill.name      = "Doppelgänger"
        , Skill.desc      = "Zetsu seizes an enemy and alters his chakra to match their own, dealing 20 damage, absorbing 1 random chakra, and stunning their non-mental skills for 1 turn. The last skill they used replaces this skill for 1 turn. Zetsu's copy of their skill has no chakra cost and ends when this skill reverts. As Black Zetsu, this skill becomes [Body Coating][b][g]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                copyLast 1
                apply 1 [Stun NonMental]
                damage 20
          ]
        }
      ]
    , [ invuln "Hide" "Zetsu" [Physical] ]
    ]
  , Character
    "Curse Mark Jūgo"
    "No longer recognizably human, Jūgo has been transformed by bloodlust into a terrifying monster. Tapping into limitless chakra, he is an unstoppable and uncontrollable force."
    [ [ Skill.new
        { Skill.name      = "Psychotic Break"
        , Skill.desc      = "Jūgo fixates obsessively on an enemy, dealing 10 damage to them for 3 turns and gaining 20% damage reduction."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemy $ damage 10
          , To Self  $ apply 1 [Reduce All Percent 20]
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
    "White Snake Orochimaru"
    "Orochimaru has cast off his body and revealed his true form of a giant serpent. Making use of the power he was granted by the White Sage Snake of Ryūchi Cave, Orochimaru transcends life and death in his endless hunger for knowledge and power."
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
        , Skill.desc      = "Orochimaru forces his soul on an enemy, dealing 15 damage to them for 3 turns and stunning their non-mental skills. If the target dies while this skill is active, Orochimaru's health is fully restored. If Orochimaru acquires a new body, this skill becomes [Eight-Headed Serpent Assault][b][t]."
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
        , Skill.desc    = "By giving an ally a curse mark, Orochimaru uses their body as an anchor for his soul after death. If the target's health reaches 25 or lower while Orochimaru is dead, Orochimaru will be resurrected into their body with full health and all status effects removed. If Orochimaru acquires a new body, this skill becomes [Regeneration][g][n]. Cannot be used while active."
        , Skill.require = HasI (-1) "curse"
        , Skill.classes = [Physical, Unremovable, Bypassing, Uncounterable, Unreflectable, Invisible, Melee]
        , Skill.cost    = [Blood, Nin]
        , Skill.effects =
          [ To Ally do
                self $ hide' "curse" 0 []
                bomb 0 [] [ To Done $ self $ remove "curse" ]
                trap' 0 OnDeath $ self $ remove "curse"
                trap' 0 (OnDamaged All) $ unlessM (user alive) do
                    targetHealth <- target health
                    when (25 >= targetHealth && targetHealth > 0) do
                        killHard
                        self do
                            setHealth 100
                            varyLoadout [0, 0, 0, 0] 1
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
  ]
