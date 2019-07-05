{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Versions (cs) where

import Characters.Base

import qualified Model.Skill as Skill
import qualified Model.Trap as Trap

cs :: [Category -> Character]
cs =
  [ let loadout = (1, 0, 0, True)
    in Character
    "Nine-Tailed Naruto"
    "Rage has triggered the beast within Naruto to emerge. As his hatred grows, so does the nine-tailed beast's power. If left unchecked, Kurama may break free of his seal, and Naruto himself will cease to exist."
    [ [ Skill.new
        { Skill.name      = "Four-Tailed Transformation"
        , Skill.desc      = "Naruto's rage takes over. He loses 5 health down to a minimum of 1 and gains 10 points of damage reduction and 10 permanent destructible defense. He permanently ignores all healing. His other skills become usable, and will increase in strength as his transformation progresses through further stages. Once used, this skill becomes [Six-Tailed Transformation][b][r]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.effects   =
          [ p Self do
                sacrifice 1 5
                defend 0 10
                varyLoadout loadout 0
                setFace 0
                apply 0 [Reduce All Flat 10, Plague]
          ]
        }
      , Skill.new
        { Skill.name      = "Six-Tailed Transformation"
        , Skill.desc      = "Naruto's fury drives him to the brink of madness. He loses 10 health down to a minimum of 1 and gains 20 points of damage reduction and 20 permanent destructible defense. He permanently ignores status effects from enemies except chakra cost changes and is immune to effects from his allies. The power of his other skills continues to grow. Once used, this skill becomes [Nine-Tailed Transformation][b][b]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = k [Blood, Rand]
        , Skill.effects   =
          [ p Self do
                remove "Four-Tailed Transformation"
                sacrifice 1 10
                defend 0 20
                varyLoadout loadout 1
                setFace 0
                apply 0 [Reduce All Flat 20, Plague, Seal, Enrage]
          ]
        }
      , Skill.new
        { Skill.name      = "Nine-Tailed Transformation"
        , Skill.desc      = "As Naruto's mind is overwhelmed by wrath, the seal breaks and Kurama takes over, unlocking the full extent of his abilities. He loses 15 health down to a minimum of 1 and gains 30 points of damage reduction and 30 permanent destructible defense. Once used, this skill becomes [Raging Flames][b][r]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = k [Blood, Blood]
        , Skill.effects   =
          [ p Self do
                remove "Six-Tailed Transformation"
                sacrifice 1 15
                defend 0 30
                varyLoadout loadout 2
                setFace 0
                apply 0 [Reduce All Flat 30, Plague, Seal, Enrage]
          ]
        }
      , Skill.new
        { Skill.name      = "Raging Flames"
        , Skill.desc      = "Finally emerging from years of imprisonment, Kurama is as cranky as he is powerful. He rains fire upon the enemy team, dealing 20 affliction damage and weakening their non-affliction damage by 10 for 1 turn."
        , Skill.classes   = [Bane, Chakra, Ranged, Bypassing]
        , Skill.cost      = k [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies do
                afflict 20
                apply 1 [Weaken All Flat 10] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Bomb"
        , Skill.desc      = "Naruto launches a sphere of condensed chakra at an opponent, dealing 30 piercing damage."
        , Skill.require   = HasI 1 "Four-Tailed Transformation"
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = k [Nin, Rand]
        , Skill.effects   =
          [ p Enemy $ pierce 30 ]
        }
      , Skill.new
        { Skill.name      = "Mini Tailed Beast Bomb Barrage"
        , Skill.desc      = "Naruto fires a volley of burning chakra orbs at an enemy, dealing 10 affliction damage to them for 3 turns. If used on an enemy affected by [Clasp], this skill deals all 30 damage instantly."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Enemy do
                has <- targetHas "Clasp"
                if has then do
                    afflict 30
                    self $ delay (-1) $
                        cancelChannel "Mini Tailed Beast Bomb Barrage"
                else
                    afflict 10
          ]
        }
      , Skill.new
        { Skill.name      = "Massive Tailed Beast Bomb"
        , Skill.desc      = "Kurama fires a gigantic sphere of condensed chakra at an enemy, dealing 60 piercing damage. Deals 40 additional damage if [Chakra Gathering] was used last turn."
        , Skill.classes   = [Chakra, Ranged, Bypassing]
        , Skill.cost      = k [Blood, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                bonus <- 40 `bonusIf` userHas "Chakra Gathering"
                pierce (60 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Burning Chakra Hand"
        , Skill.desc      = "Naruto extends a limb made of chakra to reach out and grab an enemy, dealing 20 damage and weakening their non-affliction damage by 5 for 1 turn."
        , Skill.require   = HasI 1 "Four-Tailed Transformation"
        , Skill.classes   = [Melee, Bypassing]
        , Skill.cost      = k [Blood]
        , Skill.effects   =
          [ p Enemy do
                afflict 20
                apply 1 [Weaken All Flat 5]
          ]
        }
      , Skill.new
        { Skill.name      = "Clasp"
        , Skill.desc      = "Naruto breaks through an enemy's defenses and takes hold of their head, dealing 10 damage and stunning their non-mental skills for 1 turn."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                damage 10
                apply 1 [Stun NonMental]
          ]
        }
      , Skill.new
        { Skill.name      = "Chakra Gathering"
        , Skill.desc      = "Kurama draws in chakra to improve his next [Tailed Beast Bomb]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Rand, Rand, Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self $ tag 1 ]
        }
      ]
     , [ invuln "Chakra Skin" "Naruto" [Chakra]
       , invuln "Hide" "Naruto" [Mental]
       , invuln "Block" "Kurama" [Physical]
       ]
    ] []
  , Character
    "Curse Mark Sasuke"
    "After training under Orochimaru for years, Sasuke has become a rogue ninja with complete control over his curse mark. With unlimited access to his strength and chakra, Sasuke empowers his abilities with dark energy and can even fly."
    [ [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "The dark energy of Sasuke's curse mark infuses his Sharingan, providing 10 points of damage reduction for 3 turns. While active, Sasuke ignores status effects from enemies except chakra cost changes."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self $ apply 3 [Reduce All Flat 10, Enrage] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Sasuke attacks an enemy from above, dealing 20 piercing damage and weakening their non-affliction damage by 10 for 1 turn. Deals 10 additional damage during [Sharingan]. If this skill kills an enemy, [Sharingan Genjutsu] will be applied to a random enemy."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 10 `bonusIf` userHas "Sharingan"
                pierce (20 + bonus)
                survived <- target alive
                if survived then
                    apply 1 [Weaken All Flat 10]
                else
                    self $ hide' "executed" (-1) []
          , p REnemy $ whenM (userHas "executed") do
                bonus <- 1 `bonusIf` userHas "Sharingan"
                trapWith Trap.To [Invisible] (1 + bonus) OnReflectAll $
                    return ()
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sharingan Genjutsu"
        , Skill.desc      = "Sasuke traps an enemy in an illusion that makes them believe they got the upper hand. For 1 turn, any skill that the target uses on Sasuke or his allies is reflected back to them. Lasts 1 additional turn and costs two genjutsu chakra during [Sharingan]."
        , Skill.classes   = [Mental, Ranged, Invisible]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Enemy do
                bonus <- 1 `bonusIf` userHas "Sharingan"
                trap (1 + bonus) OnReflectAll $ return ()
          ]
        , Skill.changes   = changeWith "Sharingan" $ setCost [Gen, Gen]
        }
      ]
    , [ invuln "Snake Shedding" "Sasuke" [Physical] ]
    ] []
  , Character
    "Mangekyō Sasuke"
    "Sasuke has finally slain his brother to avenge his clan, only to discover that Itachi had been protecting him from the start. Full of undirected anger, he uses his newly awakened mangekyō sharingan to strike back at anyone who gets in his way."
    [ [ Skill.new
        { Skill.name      = "Susano'o"
        , Skill.desc      = "Using the mangekyō sharingan's signature ability, Sasuke creates a colossus of chakra around himself. For 3 turns, all damage to Sasuke—including piercing and affliction—is reduced by 15 points."
        , Skill.classes   = [Chakra, Single]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                apply 3 [Reduce Affliction Flat 15]
                vary' 3 "Chidori" "Blazing Arrow"
                vary' 3 "Amaterasu" "Yasaka Beads"
                setFace 3
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chidori"
        , Skill.desc      = "Sasuke hurls lightning energy at an enemy, dealing 20 piercing damage and stunning their melee skills for 1 turn. Next turn, Sasuke gains 15 points of physical damage reduction. If no physical skills are used on Sasuke by the end of the turn, the cost of this skill becomes 1 ninjutsu chakra and its cooldown resets. During [Susano'o], this skill becomes [Blazing Arrow][b][r]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = k [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                pierce 20
                apply 1 [Stun All]
          , p Self do
                trap (-1) (OnDamaged Physical) $ remove "Chidori"
                bomb (-1) [Reduce Physical Flat 15]
                    [ p Expire do
                          hide 1 []
                          reset "Chidori" baseVariant
                    ]
          ]
        , Skill.changes   = changeWith "Chidori" $ setCost [Nin]
        }
      , Skill.new
        { Skill.name      = "Blazing Arrow"
        , Skill.desc      = "Sasuke forges three arrows out of flame and shoots them one after another at an enemy, dealing 15 damage for 3 turns. If this skill is stunned, Sasuke deals the remaining damage instantly and the cooldown of this skill resets."
        , Skill.classes   = [Chakra, Ranged, Resource]
        , Skill.cost      = k [Blood, Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Action 3
        , Skill.start     =
          [ p Self do
                remove "Blazing Arrow"
                addStacks "Blazing Arrow" 3
          ]
        , Skill.effects   =
        [ p Enemy $ damage 15
        , p Self $ removeStack "Blazing Arrow"
        ]
        , Skill.interrupt  =
          [ p Enemy do
                stacks <- userStacks "Blazing Arrow"
                damage (15 * stacks)
          , p Self do
                remove "Blazing Arrow"
                cancelChannel "Blazing Arrow"
                reset "Chidori" "Blazing Arrow"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amaterasu"
        , Skill.desc      = "Sasuke ignites an enemy, dealing 5 affliction damage to them for 4 turns. If the target becomes invulnerable, they are cured of the effect. During [Susano'o], this skill becomes [Yasaka Beads][n]. Each time an enemy is cured of [Amaterasu], the damage of [Amaterasu] and [Yasaka Beads] permanently increases by 5."
        , Skill.classes   = [Bane, Chakra, Ranged, Unreflectable]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                trap 4 OnImmune do
                    removeTrap "Amaterasu"
                    remove "Amaterasu"
                stacks <- userStacks "Amaterasu"
                bomb 4 [Afflict (5 + 5 * stacks)] [ p Remove $ self $ addStack ]
          ]
        }
      , Skill.new
        { Skill.name      = "Yasaka Beads"
        , Skill.desc      = "Sasuke attacks an enemy with a Magatama of black flame, dealing 10 affliction damage. Damage permanently increases by 5 each time an enemy is cured of [Amaterasu]. If the target uses a skill next turn, they take 10 additional affliction damage. If they receive any healing next turn, this skill deals 20 additional damage for 1 turn."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Enemy do
                bonus <- 20 `bonusIf` userHas "Yasaka Beads"
                stacks <- userStacks "Amaterasu"
                afflict (10 + bonus + 5 * stacks)
                trap (-1) (OnAction All) $ afflict 10
                trap 1 OnHealed $ self $ tag 1
          ]
        }
      ]
      {-
      Skill.new
    { Skill.name      = skillName
    , Skill.desc      = userName ++ " becomes invulnerable for 1 turn."
    , Skill.classes   = classes
    , Skill.cooldown  = 4
    , Skill.effects   = [(Self, Play $ apply 1 [Invulnerable All])]
    }
      -}
    , [ Skill.new
        { Skill.name      = "Mangekyō Foresight"
        , Skill.desc      = "Sasuke becomes invulnerable for 1 turn. Extends the duration of [Susano'o] by 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                apply 1 [Invulnerable All]
                prolong 1 "Susano'o"

          ]
        }
      ]
    ] []
  , Character
    "Commander Gaara"
    "Coordinating the Allied Shinobi Forces and personally commanding the Fourth Division, Gaara has proven to be an inspiring leader and talented strategist. His attacks scatter sand particles around the battlefield, which he draws back in with explosive force."
    [ [ Skill.new
        { Skill.name      = "Sand Grasp"
        , Skill.desc      = "Gaara grabs an enemy with sand, first adding a Sand Bomb to them and then dealing 10 damage. Deals 5 additional damage per Sand Bomb on the target. Has no chakra cost during [Sand Mausoleum Seal]. Targets all enemies during [Mother's Embrace]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Enemy do
                stacks <- targetStacks "Sand Bomb"
                damage (15 + 5 * stacks)
                apply' "Sand Bomb" 0 []
          ]
        , Skill.changes   = changeWith "Sand Mausoleum Seal" (setCost [])
                            `also` changeWith "Mother's Embrace" targetAll
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mother's Embrace"
        , Skill.desc      = "The soul of Gaara's deceased mother protects him with a shield of sand, providing 40 destructible defense for 3 turns. As long as Gaara has destructible defense from this skill, he ignores status effects from enemies except chakra cost changes."
        , Skill.classes   = [Physical]
        , Skill.cost      = k [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                defend 3 50
                onBreak'
                apply 3 [Enrage]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sand Mausoleum Seal"
        , Skill.desc      = "Dense layers of sand entomb Gaara's enemies in a giant pyramid, dealing 15 damage to all enemies for 3 turns and increasing the costs of their skills by 1 random chakra. Each turn, deals 5 additional damage to each enemy per Sand Bomb on them and removes all Sand Bombs."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = k [Blood, Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.channel   = Action 3
        , Skill.effects   =
          [ p Enemies do
                stacks <- targetStacks "Sand Bomb"
                damage (15 + 5 * stacks)
                apply 1 [Exhaust All]
          , p Everyone $ remove "Sand Bomb"
          ]
        }
      ]
    , [ invuln "Sand Shield" "Gaara" [Physical] ]
    ] []
  , Character
    "Sage Mode Kabuto"
    "Unable to find an identity of his own, Kabuto has spent his life taking on the traits of others. Years of research and experimenting upon himself have reached their conclusion, and now Kabuto prepares for his final metamorphosis."
    [ [ Skill.new
        { Skill.name      = "Sage Transformation"
        , Skill.desc      = "By synthesizing rare genetic traits from other bloodlines inside his body, Kabuto becomes attuned to the flow of natural energy. Each turn, the chakra costs and type of chakra gained from his other skills cycle through the different types of chakra. Once used, this skill becomes [DNA Transmission Shadow][r][r][r]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = k [Rand, Rand, Rand]
        , Skill.channel   = Ongoing 0
        , Skill.effects   = [p Self $ delay (-1) kabuto]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = k [Rand, Rand, Rand]
        , Skill.channel   = Control 1
        , Skill.start     =
          [ p Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , p XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ p Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = k [Blood, Blood, Blood]
        , Skill.channel   = Control 1
        , Skill.start     =
          [ p Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , p XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ p Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = k [Gen, Gen, Gen]
        , Skill.channel   = Control 1
        , Skill.start     =
          [ p Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , p XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ p Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = k [Nin, Nin, Nin]
        , Skill.channel   = Control 1
        , Skill.start     =
          [ p Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , p XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ p Self $ remove "dna" ]
        }
      , Skill.new
        { Skill.name      = "DNA Transmission Shadow"
        , Skill.desc      = "Kabuto focuses his attention on producing a clone of a dead ally. If he is not stunned during the next turn, the ally comes back to life at full health, removing all effects from them and resetting their cooldowns. They are stunned for the first turn after being created. Using this skill again destroys the current clone."
        , Skill.classes   = [Chakra, Necromancy, Unremovable, Unreflectable]
        , Skill.cost      = k [Tai, Tai, Tai]
        , Skill.channel   = Control 1
        , Skill.start     =
          [ p Self do
                hide' "dna" 1 []
                everyone $ whenM (targetHas "DNA Transmission Shadow") killHard
          , p XAlly $ delay (-1) $ whenM (userHas "dna") do
                factory
                apply 1 [Stun All]
          ]
        , Skill.interrupt  =
          [ p Self $ remove "dna" ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Rand]
        , Skill.effects   =
          [ p Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , p Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Blood]
        , Skill.effects   =
          [ p Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , p Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Gen]
        , Skill.effects   =
          [ p Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , p Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Nin]
        , Skill.effects   =
          [ p Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , p Enemies $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Inorganic Animation"
        , Skill.desc      = "Kabuto brings his surroundings to life, dealing 10 damage to all enemies. The shifting obstacles protect Kabuto's team, forcing enemies to target specific opponents with skills that would normally affect all opponents. If this skill damages any enemies, [Transfusion] and [White Extreme Attack] are recharged."
        , Skill.classes   = [Physical, Ranged, Unreflectable]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Self do
                trap' (-1) OnDamage resetCharges
                enemies $ apply 1 [Restrict]
          , p Enemies $ damage 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns and resetting the target's cooldowns. Kabuto gains 1 random chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Ally do
                resetAll
                apply 3 [Heal 15]
          , p Self $ gain [Rand]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns and resetting the target's cooldowns. Kabuto gains 1 bloodline chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Ally do
                resetAll
                apply 3 [Heal 15]
          , p Self $ gain [Blood]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns and resetting the target's cooldowns. Kabuto gains 1 genjutsu chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Ally do
                resetAll
                apply 3 [Heal 15]
          , p Self $ gain [Gen]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns and resetting the target's cooldowns. Kabuto gains 1 ninjutsu chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Ally do
                resetAll
                apply 3 [Heal 15]
          , p Self $ gain [Nin]
          ]
        }
      , Skill.new
        { Skill.name      = "Transfusion"
        , Skill.desc      = "Kabuto administers chakra-rich blood to himself or an ally, restoring 15 health for 3 turns and resetting the target's cooldowns. Kabuto gains 1 taijutsu chakra."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Ally do
                resetAll
                apply 3 [Heal 15]
          , p Self $ gain [Tai]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth which explodes in a flash of light that stuns all allies and enemies for 1 turn. Kabuto gains 2 random chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Self    $ gain [Rand, Rand]
          , p XAllies $ apply 1 [Stun All]
          , p Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth which explodes in a flash of light that stuns all allies and enemies for 1 turn. Kabuto gains 2 bloodline chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Self    $ gain [Blood, Blood]
          , p XAllies $ apply 1 [Stun All]
          , p Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth which explodes in a flash of light that stuns all allies and enemies for 1 turn. Kabuto gains 2 genjutsu chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Self    $ gain [Gen, Gen]
          , p XAllies $ apply 1 [Stun All]
          , p Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth which explodes in a flash of light that stuns all allies and enemies for 1 turn. Kabuto gains 2 ninjutsu chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Self    $ gain [Nin, Nin]
          , p XAllies $ apply 1 [Stun All]
          , p Enemies $ apply 1 [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "White Extreme Attack"
        , Skill.desc      = "Shuttering the brille over his eyes, Kabuto shoots a chakra dragon from his mouth which explodes in a flash of light that stuns all allies and enemies for 1 turn. Kabuto gains 2 taijutsu chakras."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cooldown  = 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ p Self    $ gain [Tai, Tai]
          , p XAllies $ apply 1 [Stun All]
          , p Enemies $ apply 1 [Stun All]
          ]
        }
      ]
    ] []
  , Character
    "Eight-Gates Guy"
    "With the fate of the world at stake, Guy has opened all eight Gates and is holding nothing back. The effort will surely kill him, but while he lives, his strength outmatches even the legendary Madara Uchiha."
    [ [ Skill.new
        { Skill.name      = "Evening Elephant"
        , Skill.desc      = "Using a devastating sequence of punches, Guy deals 20 damage to an enemy. For 1 turn, they are immune to effects from allies and their nonmental skills are stunned. Guy loses 20 health down to a minimum of 1. Each time this skill is used, it permanently deals 20 additional damage and costs 1 additional random chakra."
        , Skill.classes   = [Physical, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = k [Tai]
        , Skill.effects   =
          [ p Enemy do
                stacks <- userStacks "Evening Elephant"
                damage (20 + 20 * stacks)
                apply 1 [Seal, Stun NonMental]
          , p Self do
                sacrifice 1 20
                addStack
          ]
        , Skill.changes   = costPer "Evening Elephant" [Rand]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Battle Stance"
        , Skill.desc      = "Next turn, Guy will deal double damage and ignores status effects from enemies except chakra cost changes. Guy loses 10 health down to a minimum of 1."
        , Skill.classes   = [Physical, Unremovable]
        , Skill.cost      = k [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Self do
                sacrifice 1 10
                apply 1 [Enrage, Strengthen All Percent 200]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Night Guy"
        , Skill.desc      = "As his blood evaporates into mist around him, Guy warps time and space to instantly attack an enemy, dealing 50 piercing damage. For 2 turns, the target is immune to effects from allies, their damage is weakened by 5, and Guy cannot be healed. Guy loses 30 health down to a minimum of 1. Each time this skill is used, it permanently deals 25 additional damage and costs 1 additional taijutsu chakra."
        , Skill.classes   = [Physical, Melee, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = k [Tai, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                stacks <- userStacks "Night Guy"
                pierce (50 + 25 * stacks)
                apply 2 [Seal, Weaken All Flat 5]
          , p Self do
                sacrifice 1 30
                addStack
                apply 2 [Plague]
          ]
        , Skill.changes   = costPer "Night Guy" [Tai]
        }
      ]
    , [ invuln "Dodge" "Guy" [Physical] ]
    ] []
  , Character
    "Susano'o Itachi"
    "Plagued by a lethal disease that saps his strength, Itachi has been forced to go on the defensive. Out of other options, he now plays his trump card: the legendary armor Susano'o, created by the power of the mangekyō sharingan."
    [ [ Skill.new
        { Skill.name      = "Skeletal Susano'o"
        , Skill.desc      = "A bare skeleton, the first layer of Susano'o, forms around Itachi. All damage he receives—including piercing and affliction—is permanently reduced by 5. Each turn for the next 3 turns, Itachi gains 5 points of destructible defense. After 3 turns, this skill becomes [Armored Susano'o][b][b]."
        , Skill.classes   = [Chakra, Unremovable]
        , Skill.cost      = k [Rand]
        , Skill.charges   = 1
        , Skill.channel   = Ongoing 3
        , Skill.start     =
          [ p Self do
                apply 0 [Reduce Affliction Flat 5]
                delay (-3) $ vary' 0 "Skeletal Susano'o" "Armored Susano'o"
                hide' "susan" 0 []
          ]
        , Skill.effects   =
          [ p Self $ defend 0 5 ]
        }
      , Skill.new
        { Skill.name      = "Armored Susano'o"
        , Skill.desc      = "Using the full power of his mangekyō sharingan, Itachi causes ethereal muscles and skin to knit across the skeleton of Susano'o, providing 40 points of destructible defense for 2 turns. While he has destructible defense from this skill, he ignores stuns, the costs of his skills are decreased by 1 random chakra, and he is immune to affliction damage. All damage he receives—including piercing and affliction—is permanently reduced by 10, non-stacking."
        , Skill.classes   = [Chakra, Unremovable, Nonstacking]
        , Skill.cost      = k [Blood, Blood]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self do
                apply' "Armored Susano'o " 0 [Reduce Affliction Flat 10]
                defend 2 40
                onBreak'
                apply 2 [Ignore $ Any Stun, Invulnerable Affliction, Unexhaust]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Totsuka Blade"
        , Skill.desc      = "Itachi slashes an enemy with an ethereal liquid blade, dealing 25 affliction damage."
        , Skill.require   = HasI 1 "susan"
        , Skill.classes   = [Chakra, Melee, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = k [Gen, Rand]
        , Skill.effects   =
          [ p Enemy do
                stacks <- targetStacks "Hangover"
                afflict (25 + 5 * stacks)
                tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sealing Jar of Drunken Dreams"
        , Skill.desc      = "Itachi seals an enemy within the ethereal sake jar from which the Totsuka Blade is formed, depleting 1 random chakra from them. For 1 turn, they are stunned and immune to effects from allies. The target permanently takes 5 additional damage from Totsuka Blade. Can only be used on a target who was affected by [Totsuka Blade] last turn."
        , Skill.require   = HasU "Totsuka Blade"
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Blood, Rand]
        , Skill.effects   =
          [ p Enemy do
                deplete 1
                apply 1 [Stun All, Seal]
                apply' "Sealed" 0 []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Yata Mirror"
        , Skill.desc      = "Itachi becomes invincible for 1 turn. While active, every harmful skill used on him deals 10 damage to its user and causes Itachi to gain 10 destructible defense."
        , Skill.require   = HasI 1 "susan"
        , Skill.classes   = [Chakra, Invisible, Unremovable]
        , Skill.cost      = k [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self do
                apply 1 [Invincible All, Enrage]
                trapFrom 1 (OnHarmed All) do
                    damage 10
                    self $ defend 0 10
          ]
        }
      ]
    ] []
  ]
