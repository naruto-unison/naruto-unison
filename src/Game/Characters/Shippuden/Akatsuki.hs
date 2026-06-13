{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedLists #-}

module Game.Characters.Shippuden.Akatsuki (characters) where

import Game.Characters.Import
import Data.Bits (bit)

import qualified Game.Model.Skill as Skill

characters :: [Category -> Text -> Character]
characters =
  [ Character
    "Madara Uchiha" 0
    "The co-founder of the Hidden Leaf Village along with Hashirama Senju, Madara turned against his friend in pursuit of absolute, unrivaled power as a means to break the cycle of violence and establish lasting peace. Cynical and bitter, Madara works toward what he believes to be humanity's benefit without sparing a thought for those who get in his way."
    [LeafVillage, Akatsuki, Jinchuriki, Rogue, Sensor, Fire, Wind, Lightning, Earth, Water, Yin, Yang, Uchiha]
    [ [ Skill.new
        { Skill.name      = "Mangekyō Sharingan"
        , Skill.desc      = "Madara protects himself by predicting enemy attacks. For 4 turns, all non-affliction damage he receives is reduced to 25 at most. While active, this skill becomes [Eternal Mangekyō Sharingan][r]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ apply 4 skillName
                [ Limit 25
                , Alternate "Mangekyō Sharingan"
                            "Eternal Mangekyō Sharingan"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Eternal Mangekyō Sharingan"
        , Skill.desc      = "By predicting enemy attacks, Madara ignores harmful status effects for 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ apply 1 skillName [Enrage] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Madara encases himself in spectral armor that provides him with 70 permanent destructible defense. While Madara has destructible defense from this skill, he gains a stack of [Susanoo] every turn and this skill becomes [Armored Susanoo Assault][b][r]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 6
        , Skill.dur       = Passive
        , Skill.start     =
          [ To Self do
                defend' Permanent 70
                    [ Alternate "Susanoo"
                                "Armored Susanoo Assault"
                    ]
                onBreak $ cancelChannel skillName
          ]
        , Skill.effects   =
          [ To Self $ addStack skillName ]
        , Skill.end       =
          [ To Self $ remove skillName ]
        }
      , Skill.new
        { Skill.name      = "Armored Susanoo Assault"
        , Skill.desc      = "Wielding a massive spectral blade, Madara deals 30 damage to an enemy. Deals 5 additional damage per stack of [Susanoo]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                stacks <- user numStacks "Susanoo"
                damage (30 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Majestic Destroyer Flame"
        , Skill.desc      = "Madara immolates the battlefield, dealing 10 damage to an enemy and 5 damage to all other enemies for 3 turns. While active, enemies who use skills that grant damage reduction or destructible defense will take 10 damage."
        , Skill.classes   = [Bane]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemy $ damage 10
          , To XEnemies $ damage 5
          , To Enemies do
                trap 1 OnDefend $
                    damage 10
                trap 1 OnReduce $
                    damage 10
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Madara" [Chakra] ]
    ]
  , Character
    "Deidara" 0
    "An Akatsuki member who defected from the Hidden Stone Village's Demolitions Unit, Deidara posesses the unusual ability to turn clay into explosives by infusing it with lightning chakra. Most of his reckless decisions can be attributed to his pride and love of art, which usually outweigh any other priorities."
    [StoneVillage, Akatsuki, Rogue, SRank, Earth, Lightning]
    [ [ Skill.new
        { Skill.name      = "C1: Bird Bomb"
        , Skill.desc      = "Deidara hurls a clay bird at an enemy that explodes into shrapnel on impact, dealing 15 damage to the target and weakening their damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C3: Megaton Sculpture][n][r]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , Skill.classes   = [Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 4 skillName [Weaken [All] Flat 5]
          , To Self $ hide Permanent skillName
                [ Alternate "C1: Bird Bomb"
                            "C3: Megaton Sculpture"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "C3: Megaton Sculpture"
        , Skill.desc      = "Deidara drops a large explosive on the enemy team, dealing 20 damage to them and weakening their damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C1: Bird Bomb][r]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , Skill.classes   = [Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemies do
                damage 20
                apply 4 skillName [Weaken [All] Flat 5]
          , To Self $ remove "c1: bird bomb"
          ]
        }
      , Skill.new
        { Skill.name      = "C2: Dragon Missile"
        , Skill.desc      = "Deidara's dragon fires a long-range bomb at an opponent, dealing 30 damage and weakening their damage by 5 for 4 turns. Does not stack."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 30
                apply 4 skillName [Weaken [All] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "C2: Clay Dragon"
        , Skill.desc      = "Deidara sculpts a dragon out of clay and takes off, gaining 35 destructible defense for 3 turns. While active, this skill becomes [C2: Minefield][r]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self do
                defend' 3 35
                    [ Alternate "C1: Bird Bomb"
                                "C2: Dragon Missile"
                    , Alternate "C2: Clay Dragon"
                                "C2: Minefield"
                    ]
          ]
        }
      , Skill.new
        { Skill.name      = "C2: Minefield"
        , Skill.desc      = "Deidara scatters mines that burrow into the ground around an enemy. The next time they use a non-mental skill within 2 turns, they will take 10 damage and their damage will be weakened by 5 for 4 turns. Does not stack."
        , Skill.classes   = [Chakra, Ranged, Invisible, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 2 (OnAction NonMental) do
                removeTrap skillName
                damage 10
                apply 4 skillName [Weaken [All] Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "C4: Karura"
        , Skill.desc      = "A cloud of microscopic bombs enter an enemy's bloodstream and repeatedly detonate, dealing 10 affliction damage to the target every turn for the rest of the game and weakening their damage by 5. Once used, this skill becomes [C0: Ultimate Art][b][n][n]."
        , Skill.classes   = [Bane, Chakra, Ranged, Uncounterable, Unremovable, Unreflectable]
        , Skill.cost      = [Blood, Nin]
        , Skill.effects   =
          [ To Enemy $ apply Permanent skillName
                [ Afflict 10
                , Weaken [All] Flat 5
                ]
          , To Self $ hide Permanent skillName
                [ Alternate "C4: Karura"
                            "C0: Ultimate Art"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "C0: Ultimate Art"
        , Skill.desc      = "Deidara fills his veins with explosives and becomes art. He deals 35 affliction damage to all enemies and dies. Requires Deidara's health to be at or below 40."
        , Skill.require   = UserHealth 40
        , Skill.classes   = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood, Nin, Nin]
        , Skill.effects   =
          [ To Enemies $ afflict 35
          , To Self killHard
          ]
        }
      ]
    , [ invuln "Clay Clone" "Deidara" [Chakra] ]
    ]
  , Character
    "Sasori" 0
    "An Akatsuki member who defected from the Hidden Sand Village's Puppet Brigade, Sasori of the Red Sand is as hollow and soullesss as his playthings. Obsessed with creating human puppets, Sasori prizes above all others the body of the Third Kazekage, through which he can wield magnetic abilities."
    [SandVillage, Akatsuki, Rogue]
    [ [ Skill.new
        { Skill.name      = "Kazekage Puppet Summoning"
        , Skill.desc      = "Sasori summons his most prized puppet, gaining 15 permanent destructible defense and enabling his other skills. Once used, this skill becomes [Iron Sand: World Order][b][n]. Every turn, Sasori gains a stack of Iron Sand."
        , Skill.classes   = [Physical]
        , Skill.dur       = Ongoing Permanent
        , Skill.start     =
          [ To Self $ defend Permanent 15 ]
        , Skill.effects   =
          [ To Self $ applyWith [Unremovable] Permanent "Iron Sand"
                [ Alternate "Kazekage Puppet Summoning"
                            "Iron Sand: World Order"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Iron Sand: World Order"
        , Skill.desc      = "Using the third Kazekage's magnetic abilities, Sasori shapes his Iron Sand into a massive tangle of branching iron spikes that looms overhead. As it comes crashing down on the battlefield, it deals 10 piercing damage to all enemies and 5 additional damage per stack of Iron Sand."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies do
                stacks <- user numStacks "Iron Sand"
                pierce (10 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Poison Blade Assault"
        , Skill.desc      = "Sasori directs the Kazekage puppet to single out an enemy and gains 20 destructible defense for 2 turns. While Sasori has destructible defense from this skill, he deals 10 damage and 10 affliction damage to the target."
        , Skill.require   = UserHas 1 "Iron Sand"
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Passive
        , Skill.start     =
          [ To Self do
                replaceChannel -- in case cooldown was reset
                defend 2 20
                onBreak $ cancelChannel skillName
          ]
        , Skill.effects   =
          [ To Enemy do
                damage 10
                afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Thousand Arms"
        , Skill.desc      = "Countless concealed arms lash out from Sasori's Kazekage puppet and flail wildly for 1 turn, pinning down anyone they catch. Enemies who do not use skills on Sasori or his allies next turn will be pinned for 1 turn, unable to reduce damage or become invulnerable. Next turn, this skill becomes [Poison Gas][r][r]."
        , Skill.require   = UserHas 1 "Iron Sand"
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.start     =
          [ To Enemies do
                bomb -1 skillName
                    []
                    [ To Expire $ apply 1 "Pinned" [Expose] ]
                trap -1 OnHarm $
                    remove skillName
          , To Self $ hide 1 skillName
                [ Alternate "Thousand Arms"
                            "Poison Gas"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Poison Gas"
        , Skill.desc      = "Sasori emits a cloud of poisonous gas, dealing 15 affliction damage to all enemies. Next turn, enemy cooldowns are increased by 1 turn and enemy chakra costs are increased by 1 arbitrary chakra. Lasts 2 turns on targets pinned by [Thousand Arms]."
        , Skill.classes   = [Physical, Bane, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemies do
                afflict 15
                bonus <- 1 `bonusIf` target has "Pinned"
                apply (1 + bonus) skillName
                    [ Snare 1
                    , Exhaust [All]
                    ]
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Sasori" [Chakra] ]
    ]
  , Character
    "Hidan" 0
    "An Akatsuki member who defected from the Hidden Hotspring Village, Hidan belongs to a cult that worships Jashin, a bloodthirsty and murderous god who blesses him with immortality. With no need to fear death, he binds his soul to his enemies and tortures himself endlessly."
    [Akatsuki, Rogue, SRank]
    [ [ Skill.new
        { Skill.name      = "Jashin Sigil"
        , Skill.require   = UserHas 0 "Jashin Sigil"
        , Skill.desc      = "Hidan prepares for his ritual by drawing an insignia on the ground in blood. Once used, this skill becomes [First Blood][r]."
        , Skill.classes   = [Physical, Unremovable, Uncounterable, Unreflectable]
        , Skill.effects   =
          [ To Self $ apply Permanent skillName
                [ Alternate "Jashin Sigil"
                            "First Blood"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "First Blood"
        , Skill.desc      = "Searching for a victim to join him in his ritual of death, Hidan deals 5 damage to an opponent and marks them for 2 turns. While they are marked, this skill becomes [Blood Curse][g]."
        , Skill.classes   = [Bane, Physical, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 5
                bomb 2 skillName
                    []
                    [ To Done $ targeting Self $ remove "first blood" ]
                targeting Self $ hide Permanent skillName
                    [ Alternate "Jashin Sigil"
                                "Blood Curse"
                    ]
          ]
        }
      , Skill.new
        { Skill.name      = "Blood Curse"
        , Skill.desc      = "Hidan begins his ritual by drinking the blood of [First Blood]'s target, instantly using [Prayer] and then linking himself to them for 3 turns. While active, skills used on Hidan and the target by their opponents are also reflected to each other, and this skill becomes [Death Blow][t][g]. Hidan ignores harmful status effects, although his target does not. Damage that Hidan deals to himself with his own skills while linked to a living target does not harm him."
        , Skill.require   = TargetHas 1 "First Blood"
        , Skill.classes   = [Chakra, Soulbound, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Self do
                stacks <- user numStacks "jashin"
                apply (fromIntegral $ 1 + stacks) "Prayer" [Endure]
                hide Permanent "jashin" []
          ,  To Enemy do
                userSlot   <- user slot
                targetSlot <- target slot
                apply 3 "Blood Curse" [Share userSlot]
                trap 3 OnDeath $ targeting Self $
                    remove "bloodlink"
                targeting Self do
                    hide 3 "bloodlink" []
                    bomb 3 "Blood Curse"
                        [ Enrage
                        , Share targetSlot
                        , Alternate "Jashin Sigil"
                                    "Death Blow"
                        ]
                        [ To Done do
                            remove "Jashin Sigil"
                            remove "bloodlink"
                        ]
          ]
        }
      , Skill.new
        { Skill.name      = "Death Blow"
        , Skill.desc      = "Hidan impales himself through his chest, dealing 50 piercing damage to himself."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen, Tai]
        , Skill.effects   =
          [ To Self do
                linked <- user has "bloodlink"
                if linked then
                    targeting Enemies $ whenM (target has "Blood Curse") $
                        pierce 50
                else
                    sacrifice 0 50
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Self-Mutilation"
        , Skill.desc      = "Hidan tears a gash in his stomach with his scythe, dealing 35 piercing damage to himself and stunning himself for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                linked <- user has "bloodlink"
                if linked then do
                        apply 1 skillName []
                        targeting Enemies $ whenM (target has "Blood Curse") do
                            pierce 35
                            apply 1 skillName [Stun All]
                else do
                    sacrifice 0 35
                    apply 1 skillName [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Prayer"
        , Skill.desc      = "Silently praying to Lord Jashin, Hidan prevents his health from dropping below 1 for 1 turn. Every time this skill is used, it costs 1 additional arbitrary chakra and its effect lasts 1 additional turn."
        , Skill.classes   = [Mental, Uncounterable, Unreflectable, Unremovable, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                stacks <- user numStacks "jashin"
                apply (fromIntegral $ 1 + stacks) skillName [Endure]
                hide Permanent "jashin" []
          ]
        , Skill.changes   = changePer "jashin" \i ->
                                setCost $ replicate (i + 1) Rand
        }
      ]
    , [ invuln "Block" "Hidan" [Physical] ]
    ]
  , Character
    "Kakuzu" 0
    "The self-proclaimed Treasurer of the Akatsuki, Kakuzu is a money-obsessed bounty hunter who defected from the Hidden Waterfall Village in pursuit of wealth. With his anger usually going unchecked, he has a bad habit of ripping out the heart of anyone who annoys him. He stores extra hearts in masks, each of which grants different abilities."
    [Akatsuki, Rogue, SRank, Earth, Water, Fire, Wind, Lightning]
    [ [ Skill.new
        { Skill.name     = "Pressure Damage"
        , Skill.desc     = "Attaching his wind-element mask, Kakuzu fires a tornado of compressed air at an enemy, dealing 30 damage and stunning their chakra and ranged skills for 1 turn. Once used, this skill becomes [Searing Migraine][b]."
        , Skill.classes  = [Chakra, Ranged]
        , Skill.cost     = [Nin, Rand]
        , Skill.effects  =
          [ To Enemy do
                damage 30
                apply 1 skillName
                    [ Stun Chakra
                    , Stun Ranged
                    ]
          , To Self $ hide Permanent skillName
                [ Alternate "Pressure Damage"
                            "Searing Migraine"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Searing Migraine"
        , Skill.desc      = "Attaching his fire-element mask, Kakuzu creates a firestorm that spreads across the battlefield, dealing 15 affliction damage to all enemies. Once used, this skill becomes [Pressure Damage][n][r]."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ afflict 15
          , To Self $ remove "pressure damage"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "False Darkness"
        , Skill.desc      = "Attaching his lightning-element mask, Kakuzu transfixes an enemy with energy spears that deal 30 piercing damage. Once used, this skill becomes [Blast Flames][b][n]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To Enemy $ pierce 30
          , To Self $ hide Permanent skillName
                [ Alternate "False Darkness"
                            "Blast Flames"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Blast Flames"
        , Skill.desc      = "Combining his fire-element and wind-element masks, Kakuzu creates a fiery tornado that deals 35 damage to an enemy and 20 damage to all other enemies. Once used, this skill becomes [False Darkness][g][r]."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.effects   =
          [ To Enemy $ damage 35
          , To XEnemies $ damage 20
          , To Self $ remove "false darkness"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Earth Grudge"
        , Skill.desc      = "Black thread-like tendrils tear the heart from an enemy whose health is at 20 or lower, killing the target and restoring 35 health to Kakuzu if successful."
        , Skill.require   = TargetHealth 20
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                kill
                unlessM (target alive) $ targeting Self $
                    heal 35
          ]
        }
      ]
    , [ invuln "Iron Skin" "Kakuzu" [Physical] ]
    ]
  , Character
    "Kisame Hoshigaki" 0
    "An Akatsuki member and one of the Seven Swordsmen of the Mist, Kisame is a rogue operative who hunts and captures tailed beasts. His water techniques and legendary sword Samehada flood his enemies."
    [MistVillage, Akatsuki, SevenSwordsmen, Rogue, SRank, Water, Fire, Wind, Earth]
    [ [ Skill.new
        { Skill.name      = "Thousand Hungry Sharks"
        , Skill.desc      = "A school of sharks erupts around Kisame. He gains ten stacks of [Hundred Hungry Sharks]. Every turn, the sharks deal 5 piercing damage to all enemies, spending one stack per enemy hit. The first enemy to use a skill on Kisame will be marked, causing the sharks to ignore other enemies until the target dies. Deals 5 additional damage during [Exploding Water Shockwave]. Once used, this skill becomes [Man-Eating Sharks][n]."
        , Skill.classes   = [Chakra, Ranged, Unreflectable, Resource]
        , Skill.cost      = [Nin]
        , Skill.dur       = Ongoing Permanent
        , Skill.start     =
          [ To Self do
                applyStacks "Hundred Hungry Sharks" 10
                    [ Alternate "Thousand Hungry Sharks"
                                "Man-Eating Sharks"
                    ]
                trapFrom' Permanent (OnHarmed All) do
                    targeting Self $ removeTrap skillName
                    targeting Enemies $ hide Permanent "ignored" []
                    remove "ignored"
                    tag Permanent skillName
                    trap' Permanent OnDeath $ targeting Everyone $
                        remove "ignored"
          ]
        , Skill.effects   =
          [ To Enemies $ unlessM (target has "ignored") $
                whenM (user has "Hundred Hungry Sharks") do
                    bonus <- 5 `bonusIf` channeling "Exploding Water Shockwave"
                    pierce (5 + bonus)
                    targeting Self $ removeStack "Hundred Hungry Sharks"
          , To Self $ unlessM (user has "Hundred Hungry Sharks") $
                cancelChannel skillName
          ]
        , Skill.end       =
          [ To Self $ targeting Everyone do
                remove "ignored"
                remove skillName
          ]
        }
      , Skill.new
        { Skill.name      = "Man-Eating Sharks"
        , Skill.desc      = "Spends all stacks of [Hundred Hungry Sharks] to deal 5 piercing damage per stack to an enemy. Costs [r] during [Exploding Water Shockwave]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                stacks <- user numStacks "Hundred Hungry Sharks"
                pierce (5 * stacks)
          , To Self do
                remove "Hundred Hungry Sharks"
                cancelChannel "Thousand Hungry Sharks"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Exploding Water Shockwave"
        , Skill.desc      = "As a giant orb of water fills the entire battlefield, Kisame merges with Samehada and transforms into a shark for 3 turns. While active, enemy cooldowns are increased by 1 turn and this skill becomes [Shark Dance][t]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 3
        , Skill.effects   =
          [ To Enemies $ apply 1 skillName [Snare 1]
          , To Self $ hide 1 skillName
                [ Alternate "Exploding Water Shockwave"
                            "Shark Dance"
                , Face
                ]
          ]
        , Skill.stunned   =
          [ To Self $ hide 1 skillName
                [ Alternate "Exploding Water Shockwave"
                            "Shark Dance"
                , Face
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Shark Dance"
        , Skill.desc      = "Deals 20 damage to an enemy and absorbs 1 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                damage 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Shark Bomb"
        , Skill.desc      = "Kisame traps an enemy for 1 turn. At the end of their turn, the target takes 30 damage. While active, if the target uses a chakra or mental skill on Kisame or his allies, they will be countered and will take 20 additional damage."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Invisible]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 4
        , Skill.dur       = Ongoing -1
        , Skill.effects   =
          [ To Enemy do
                trap 1 (Countered Chakra) $
                    flag skillName
                trap 1 (Countered Mental) $
                    flag skillName
          ]
        , Skill.end       =
          [ To Enemy do
                bonus <- 20 `bonusIf` target has "super shark bomb"
                damage (30 + bonus)
          ]
        }
      ]
    , [ invuln "Scale Shield" "Kisame" [Physical] ]
    ]
  , Character
    "Itachi Uchiha" 0
    "An Akatsuki member who defected from the Hidden Leaf Village, Itachi is known as the Clan Killer for slaughtering the rest of the Uchihas, sparing only his brother. Plagued by a lethal disease that saps his strength, Itachi has been forced to go on the defensive. Out of other options, he now plays his trump card: the legendary armor Susanoo, created by the power of the mangekyō sharingan."
    [LeafVillage, Akatsuki, Rogue, SRank, Fire, Wind, Water, Yin, Yang, Uchiha]
    [ [ Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Itachi loses 10 health and encases himself in spectral armor that provides him with 5 permanent destructible defense every turn. This skill can be used again with no chakra cost to cancel its effect and remove its destructible defense."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood]
        , Skill.dur       = Ongoing Permanent
        , Skill.start     =
          [ To Self $ sacrifice 0 10 ]
        , Skill.effects   =
          [ To Self do
                defend Permanent 5
                hide 1 skillName
                    [ Alternate "Susanoo"
                                "Susanoo"
                    , Alternate "Amaterasu"
                                "Totsuka Blade"
                    , Alternate "Mirage Crow"
                                "Yata Mirror"
                    ]
                targeting Everyone $ remove "Amaterasu"
          ]
        }
      , Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Ends the effect of [Susanoo] and removes its destructible defense."
        , Skill.classes   = [Chakra]
        , Skill.effects   =
          [ To Self do
                cancelChannel skillName
                removeDefense skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amaterasu"
        , Skill.desc      = "Itachi sets an enemy on fire, dealing 15 affliction damage to them every turn until he uses [Susanoo]. Does not stack. During [Susanoo], this skill becomes [Totsuka Blade][g]."
        , Skill.classes   = [Bane, Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply Permanent skillName [Afflict 15] ]
        }
      , Skill.new
        { Skill.name      = "Totsuka Blade"
        , Skill.desc      = "Itachi slashes an enemy with an ethereal liquid blade, dealing 25 piercing damage and depleting a bloodline or genjutsu chakra."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemy do
                pierce 25
                deplete1 [Blood, Gen]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Mirage Crow"
        , Skill.desc      = "Itachi traps an enemy in an illusion. If they use a skill on Itachi or his allies next turn, their physical and ranged skills will be stunned for 2 turns. During [Susanoo], this skill becomes [Yata Mirror][g]."
        , Skill.classes   = [Mental, Ranged, Invisible]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.dur       = Control 1
        , Skill.effects   =
          [ To Enemy $ controlTrap (Countered All) $
                apply 2 skillName
                    [ Stun Physical
                    , Stun Ranged
                    ]
          ]
        }
      , Skill.new
        { Skill.name      = "Yata Mirror"
        , Skill.desc      = "Itachi defends himself with an ethereal shield. Next turn, Itachi ignores enemy skills, and enemies who use skills on Itachi will have the costs of their skills increased by 1 additional arbitrary chakra for 1 turn."
        , Skill.classes    = [Chakra, Ranged, Invisible]
        , Skill.cost       = [Gen]
        , Skill.cooldown   = 2
        , Skill.effects    =
          [ To Self do
                trapFrom 1 (OnHarmed All) $
                    apply 1 skillName [Exhaust [All]]
                apply 1 skillName [Nullify]
          ]
        }
      ]
    , [ invuln "Dodge" "Itachi" [Physical] ]
    ]
  , Character
    "Zetsu" 0
    "After Madara turned the Gedo statue's mutated victims into an army of servants, he chose one to lead them. Imbuing the White Zetsu entity with materialized will in the form of Black Zetsu, he created a hybrid being who became an official member of Akatsuki. White Zetsu and Black Zetsu have different approaches to combat, but both are able to take control of an enemy's abilities."
    [Akatsuki, Sensor, SRank, Earth, Water, Fire, Wind, Lightning, Yin, Yang]
    let
        blackZetsuSkill :: Skill
        blackZetsuSkill = Skill.new
            { Skill.name      = "Black Zetsu"
            , Skill.classes   = [Chakra]
            , Skill.dur       = Ongoing Permanent
            , Skill.start     =
              [ To Self $ cancelChannel "White Zetsu" ]
            , Skill.effects   =
              [ To Self do
                    hide 1 skillName
                        [ Alternate "Black Zetsu"
                                    "Underground Roots"
                        , Alternate "Doppelgänger / Body Coating"
                                    "Body Coating"
                        , Face
                        ]
                    unlessM (user has "chakra") do
                        gain [Rand]
                        hide 1 "chakra" []
              ]
            }
    in
    [ [ Skill.new
        { Skill.name      = "White Zetsu"
        , Skill.desc      = "Zetsu's white half takes over, canceling [Black Zetsu]. While active, Zetsu gains 5 permanent destructible defense every turn. Once used, this skill becomes [Black Zetsu]."
        , Skill.classes   = [Chakra]
        , Skill.dur       = Ongoing Permanent
        , Skill.start     =
          [ To Self $ cancelChannel "Black Zetsu" ]
        , Skill.effects   =
          [ To Self do
                defend Permanent 5
                hide 1 skillName
                    [ Alternate "White Zetsu"
                                "Black Zetsu"
                    , Alternate "Black Zetsu"
                                "White Army"
                    , Alternate "Doppelgänger / Body Coating"
                                "Doppelgänger"
                    , Face
                    ]
          ]
        }
      , blackZetsuSkill
        { Skill.desc      = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains 1 random chakra every other turn. Once used, this skill becomes [White Zetsu]."
        }
      ]
    , [ blackZetsuSkill
        { Skill.desc      = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains 1 random chakra every other turn. Once used, this skill becomes [Underground Roots][b][r]. As White Zetsu, this skill becomes [White Army][g]."
        }
      , Skill.new
        { Skill.name      = "Underground Roots"
        , Skill.desc      = "Tree roots emerge from the ground and wrap around an enemy, dealing 20 damage for 2 turns. While active, the target's damage is weakened by half. As White Zetsu, this skill becomes [White Army][g]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Control 2
        , Skill.start     =
          [ To Enemy $ control [Weaken [All] Percent 50] ]
        , Skill.effects   =
          [ To Enemy $ damage 20 ]
        }
      , Skill.new
        { Skill.name      = "White Army"
        , Skill.desc      = "Zetsu creates numerous clones of himself which deal 5 damage to all enemies for 5 turns. As Black Zetsu, this skill becomes [Underground Roots][b][r]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen]
        , Skill.dur       = Action 5
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
        , Skill.require   = TargetHas 0 "Body Coating"
        , Skill.classes   = [Mental, Melee, Invisible, Unremovable, Unreflectable]
        , Skill.cost      = [Blood, Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                apply Permanent skillName [Swap]
                trap' Permanent (OnAction All) do
                    removeTrap skillName
                    remove skillName
          ]
        }
      , Skill.new
        { Skill.name      = "Doppelgänger"
        , Skill.desc      = "Zetsu seizes an enemy and alters his chakra to match their own, dealing 20 damage, absorbing 1 random chakra, and stunning their non-mental skills for 1 turn. The last skill they used replaces this skill for 1 turn. Copied skills cannot copy other skills and do not transform into alternates. As Black Zetsu, this skill becomes [Body Coating][b][g]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                copyLast -2
                apply 1 skillName [Stun NonMental]
                damage 20
          ]
        }
      ]
    , [ invuln "Hide" "Zetsu" [Physical] ]
    ]
  , Character
    "Tobi" 0
    "A peculiar new member of the Akatsuki who rarely takes anything seriously, Tobi claims to be Madara Uchiha despite all evidence to the contrary. Using his Izanagi, he can rewind his state to an earlier point and even come back from the dead."
    [LeafVillage, Akatsuki, SRank, Jinchuriki, Sensor, SRank, Fire, Wind, Lightning, Earth, Water, Yin, Yang, Uchiha]
    [ [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Tobi analyzes the battlefield to gain the upper hand. The next time an enemy uses a skill on him, it will be countered and this skill will become [Kamui][g][r] for 2 turns. Cannot be used while active."
        , Skill.require   = UserTrap False "Sharingan"
        , Skill.classes   = [Mental, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ trap Permanent (Counter All) $
                apply 2 skillName
                    [ Alternate "Sharingan"
                                "Kamui"
                    ]
          ]
        }
      , Skill.new
        { Skill.name      = "Kamui"
        , Skill.desc      = "Tobi uses a rare space-time technique to banish a target to his pocket dimension for 3 turns, making them invulnerable to allies as well as enemies and unable to affect anyone else. If used on an ally, cures all harmful effects on them. If used on an enemy, deals 20 piercing damage, purges them of helpful effects, and prevents them from reducing damage or becoming invulnerable. Ends if Tobi uses [Kamui] or [Kamui Strike] on someone else."
        , Skill.classes   = [Chakra, Ranged, Unreflectable, Unremovable]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To XAlly do
                targeting Everyone $ remove skillName
                cureAll
                apply 3 skillName
                    [ Alone
                    , Invulnerable All
                    , BlockAllies
                    , BlockEnemies
                    ]
          , To Enemy do
                targeting Everyone $ remove skillName
                purge
                apply 3 skillName
                    [ Expose
                    , Alone
                    , Invulnerable All
                    , BlockAllies
                    , BlockEnemies
                    ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kamui Strike"
        , Skill.desc      = "Tobi teleports behind an enemy and deals 20 piercing damage to them. Deals 20 additional damage if the target is affected by [Kamui]."
        , Skill.classes   = [Chakra, Melee, Bypassing]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemy do
                kamui <- target has "Kamui"
                if kamui then
                    pierce 40
                else do
                    targeting Everyone $ remove "Kamui"
                    pierce 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Izanagi"
        , Skill.desc      = "Tobi sacrifices one of his eyes to take control of reality on a local scale, reversing the flow of time. In 4 turns, he will be restored to his condition at the moment of using this skill. Cannot be used while active."
        , Skill.require    = UserHas 0 "Izanagi"
        , Skill.classes    = [Mental, Invisible, Unremovable]
        , Skill.cost       = [Blood, Blood]
        , Skill.charges    = 2
        , Skill.effects    =
          [ To Self do
                rewind <- user ()
                bombWith [Necromancy] 4 skillName
                    []
                    [ To Expire $ replaceWith rewind ]
          ]
        }
      ]
    , [ invuln "Phase" "Tobi" [Chakra] ]
    ]
  , Character
    "Deva Path Pain" 0
    "Having taken over the body of Yahiko, his deceased best friend, Pain now acts through it as one of his Six Paths. To honor Yahiko's memory, Pain uses the Deva Path as the leader of the Six Paths and his main body when interacting with others. Deva Path's specialization is gravity manipulation, which he uses to impair and imprison his enemies."
    [RainVillage, Akatsuki, SRank, Water, Fire, Wind, Lightning, Earth, Water]
    [ [ Skill.new
        { Skill.name      = "Almighty Push"
        , Skill.desc      = "Pain targets himself or an ally. The first skill an enemy uses on them next turn will be countered, and the person countered will take 20 damage. Once used, this skill alternates between [Universal Pull] and [Almighty Push] every turn. "
        , Skill.classes   = [Chakra, Ranged, Invisible, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.dur       = Passive
        , Skill.start     =
          [ To Ally $ trapFrom 1 (Counter All) $
                damage 20
          , To Self $ tag 1 "Tidal Force"
          ]
        , Skill.effects   =
          [ To Self do
                push <- user has "almighty push"
                let alt = if push then "Almighty Push" else "Universal Pull"
                hide 1 "_" [Alternate "Almighty Push" alt]
                unless (push) $
                    hide 1 skillName []
          ]
        }
      , Skill.new
        { Skill.name      = "Almighty Push"
        , Skill.desc      = "Pain targets himself or an ally. The first skill an enemy uses on them next turn will be countered, and the person countered will take 20 damage. This skill will become [Universal Pull] next turn."
        , Skill.classes   = [Chakra, Ranged, Invisible, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Ally $ trapFrom 1 (Counter All) $
                damage 20
          , To Self $ tag 1 "Tidal Force"
          ]
        }
      , Skill.new
        { Skill.name      = "Universal Pull"
        , Skill.desc      = "Pain manipulates gravity to pull an enemy toward him, ending their Action and Control skills in progress. Next turn, the target can only target Pain or themselves. If [Almighty Push] was used last turn, its effect is applied to Pain. This skill will become [Almighty Push] next turn."
        , Skill.classes   = [Chakra, Ranged, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemy do
                interrupt
                userSlot <- user slot
                apply 1 skillName [Taunt userSlot]
          , To Self $ whenM (user has "Tidal Force") $
                trapFrom 1 (Counter All) $
                        damage 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Receiver"
        , Skill.desc      = "Pain pierces an enemy with a black rod that attunes them to his chakra, dealing 15 piercing damage and applying 15 permanent destructible barrier. Starting 1 turn from now, while the target has destructible barrier from this skill, they are stunned every other turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.dur       = Passive
        , Skill.start     =
          [ To Enemy do
                replaceChannel
                pierce 15
                barricade Permanent 15
                onBreak do
                    remove skillName
                    unlessM (anyoneHas skillName) $
                        cancelChannel skillName
          ]
        , Skill.effects   =
          [ To Self $ targeting Enemies $
                whenM (target has' barrier skillName) do
                    notStunned <- target has "chakra receiver"
                    if notStunned then
                        apply 1 skillName [Stun All]
                    else
                        applyWith [Unremovable, Hidden, Nonstacking]
                            1 "chakra receiver" []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Planetary Devastation"
        , Skill.desc      = "Pain creates a gravitational anchor that pulls in an enemy and accumulates a rough sphere of rock and debris around them, applying 80 destructible barrier for 3 turns. While the target has destructible barrier from this skill, they are invulnerable to allies as well as enemies. At the end of the 3 turns, the target will take damage equal to the remaining destructible barrier from this skill."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Gen, Tai]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy do
                barricade' 3 80 [Alone, Invulnerable All]
                onBreak do
                    remaining <- target barrierAmount "Planetary Devastation"
                    when (remaining > 0) $
                        damage remaining
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Asura Path Pain" 0
    "Having taken over the body of a wandering puppeteer, Pain now acts through it as one of his Six Paths. Asura Path's body is heavily augmented with ballistic and mechanical weaponry."
    [Akatsuki, SRank]
    [ [ Skill.new
        { Skill.name      = "Metal Blade"
        , Skill.desc      = "Pain gouges an enemy with a blade that unfolds from his body, dealing 15 piercing damage and inflicting a deep wound. The target takes 10 affliction damage every turn until one of their allies uses a skill on them. Does not stack."
        , Skill.classes   = [Bane, Physical, Melee, Nonstacking]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                apply Permanent skillName [Afflict 10]
                trap Permanent OnHelped do
                    removeTrap skillName
                    remove skillName
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Missile Salvo"
        , Skill.desc      = "Pain launches a cluster of missiles at an enemy, dealing 10 damage to them for 2 turns and removing the effects of helpful skills from them. Once used, this skill becomes [Head Cannon][r][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 2
        , Skill.start     =
          [ To Enemy purge
          , To Self $ hide Permanent skillName
                [ Alternate "Missile Salvo"
                            "Head Cannon"
                ]
          ]
        , Skill.effects   =
          [ To Enemy $ damage 10 ]
        }
      , Skill.new
        { Skill.name      = "Head Cannon"
        , Skill.desc      = "Pain's head opens up to reveal a cannon, which explodes and deals 20 piercing damage to all enemies. Once used, this skill becomes [Missile Salvo][r][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ pierce 20
          , To Self $ remove "missile salvo"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Guided Missile"
        , Skill.desc      = "Pain fires a slow-moving but devastating missile at a target. Over the next four turns, the cost of this skill is 1 chakra that cycles through the different types of chakra. Each turn, it has a different effect on the target. Using the skill again resets it."
        , Skill.classes   = [Physical, Ranged, Bypassing, Invisible, Nonstacking]
        , Skill.dur       = Ongoing 4
        , Skill.start     =
          [ To Enemy $ tag 4 skillName ]
        , Skill.effects   =
          [ To Self $ nextAlternate skillName]
        , Skill.end       =
          [ To Self $ targeting Everyone $ remove skillName ]
        }
      , Skill.new
        { Skill.name      = "Bloodline Missile"
        , Skill.desc      = "Deals 25 damage to the target of [Guided Missile] and 25 damage to a random enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ whenM (target has "Guided Missile") $
                damage 25
          , To REnemy $ damage 25
          , To Self $ cancelChannel "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Genjutsu Missile"
        , Skill.desc      = "Prevents the target of [Guided Missile] from reducing damage or becoming invulnerable for 2 turns and deals 25 damage."
        , Skill.require   = TargetHas 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemies do
                apply 2 skillName [Expose]
                damage 25
          , To Self $ cancelChannel "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Ninjutsu Missile"
        , Skill.desc      = "Deals 25 damage to the target of [Guided Missile] and stuns them for 1 turn."
        , Skill.require   = TargetHas 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemies do
                damage 25
                apply 1 skillName [Stun All]
          , To Self $ cancelChannel "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Taijutsu Missile"
        , Skill.desc      = "Deals 30 piercing damage to the target of [Guided Missile]."
        , Skill.require   = TargetHas 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged, Bypassing]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemies $ pierce 30
          , To Self $ cancelChannel "Guided Missile"
          ]
        }
      ]
    , [ invuln "Flee" "Pain" [Physical] ]
    ]
  , Character
    "Human Path Pain" 0
    "Having taken over the body of a ninja from the Hidden Waterfall Village, Pain now acts through it as one of his Six Paths. Human Path's specialty is drawing the souls of his enemies from their bodies to reveal their secrets and drain their lifeforce."
    [Akatsuki, SRank]
    [ [ Skill.new
        { Skill.name      = "Mind Invasion"
        , Skill.desc      = "Pain invades the mind of an enemy, dealing 15 damage. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 1 skillName [Reveal]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Spirit Absorption"
        , Skill.desc      = "Pain draws out the lifeforce of an enemy affected by [Mind Invasion], stealing 20 health and absorbing 1 random chakra. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.require   = TargetHas 1 "Mind Invasion"
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                apply 1 skillName [Reveal]
                leech 20 heal
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Soul Rip"
        , Skill.desc      = "Pain pulls out the soul of an enemy affected by [Mind Invasion], stealing 30 health. If their health reaches 30 or lower, they die. If they survive, he absorbs 1 random chakra from them and stuns them for 1 turn. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.require   = TargetHas 1 "Mind Invasion"
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Gen, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                leech 30 heal
                executeAt 30
                whenM (target alive) do
                    absorb 1
                    apply 1 skillName
                        [ Stun All
                        , Reveal
                        ]
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Animal Path Pain" 0
    "Having taken over the body of a ninja from the Hidden Rain Village named Ajisai, Pain now acts through it as one of his Six Paths. Animal Path's specialization is summoning giant creatures that continue to fight for her even if she is immobilized."
    [RainVillage, Akatsuki, SRank]
    [ [ Skill.new
        { Skill.name      = "Summoning: Giant Centipede"
        , Skill.desc      = "Pain summons a huge centipede behind an enemy to ambush them. It deals 15 damage to them for 2 turns, and if the target does not use a skill during that time, they will be stunned for 1 turn. Once used, this skill becomes [Summoning: Giant Crustacean][r][r]."
        , Skill.classes   = [Summon, Melee]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.dur       = Ongoing 2
        , Skill.start     =
          [ To Enemy $ trap 2 (OnAction All) $
                removeTrap skillName
          ]
        , Skill.effects   =
          [ To Enemy $ damage 15
          , To Self $ hide 1 skillName
                [ Alternate "Summoning: Giant Centipede"
                            "Summoning: Giant Crustacean"
                ]
          ]
        , Skill.end       =
          [ To Self $ targeting Enemies $
                whenM (target has' traps skillName) $
                    apply 1 "Giant Centipede Stun" [Stun All]
          ]
        }
      , Skill.new
        { Skill.name      = "Summoning: Giant Crustacean"
        , Skill.desc      = "Pain summons a huge foaming lobster that sprays spittle over the battlefield, washing away oil, toxins, and the like. For 2 turns, all enemies take 10 damage and their cooldowns are increased by 1 turn. While active, the lobster provides 10 points of damage reduction to Pain and her team and makes them invulnerable to bane skills. Once used, this skill becomes [Summoning: Giant Centipede][n]."
        , Skill.classes   = [Summon, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Ongoing 2
        , Skill.start     =
          [ To Self $ remove "summoning: giant centipede" ]
        , Skill.effects   =
          [ To Enemies do
                damage 10
                apply 1 skillName [Exhaust [All]]
          , To Allies $ apply 1 skillName
                [ Reduce [All] Flat 10
                , Invulnerable Bane
                ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Giant Panda"
        , Skill.desc      = "Pain summons a huge panda that defends herself or an ally, providing 20 permanent destructible defense and making them invulnerable for 2 turns."
        , Skill.classes   = [Summon]
        , Skill.cost      = [Nin, Blood]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Ally do
                defend Permanent 20
                apply 2 skillName [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Giant Multi-Headed Dog"
        , Skill.desc      = "Pain summons a huge Cerberus hound that deals 10 damage to all enemies for 3 turns. Whenever an enemy uses a skill on Pain or her allies, the beast's heads multiply and their damage doubles."
        , Skill.classes   = [Summon, Melee, Bypassing, Unreflectable, Unremovable]
        , Skill.cost      = [Blood, Rand]
        , Skill.dur       = Ongoing 3
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Allies $ trap -1 (OnHarmed All) $ targeting Self $
                addStack skillName
          , To Enemies do
                stacks <- user numStacks skillName
                damage (10 * bit stacks)
          ]
        , Skill.end       =
          [ To Self $ remove skillName ]
        }
      ]
    , [ invuln "Summoning: Giant Chameleon" "Pain" [Summon, Invisible] ]
    ]
  , Character
    "Preta Path Pain" 0
    "Having taken over the body of a farmer from the Hidden Grass Village, Pain now acts through it as one of his Six Paths. Preta Path's specialization is absorbing chakra and nullifying ninjutsu abilities."
    [Akatsuki, SRank]
    [ [ Skill.new
        { Skill.name      = "Chakra Shield"
        , Skill.desc      = "Pain creates a protective barrier around himself that absorbs chakra. Next turn, enemy skills used on him will be nullified, and Pain will gain chakra equal to the chakra cost of nullified skills."
        , Skill.classes   = [Chakra, Ranged, Invisible, Nonstacking, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ apply 1 skillName
                [ Absorb
                , Nullify
                ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Preta Drain"
        , Skill.desc      = "Pain absorbs an enemy's energy, dealing 25 damage and regaining 10 health per chakra that the target spent on their most recent skill."
        , Skill.classes   = [Melee, Chakra]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                chakra <- target lastChakraSpent
                targeting Self $ heal (10 * length chakra)
                damage 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ninjutsu Absorption"
        , Skill.desc      = "Pain nullifies an enemy's chakra, absorbing 1 random chakra and preventing them from using skills that cost bloodline or ninjutsu chakra for 1 turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                apply 1 skillName
                    [ Stun Bloodline
                    , Stun Ninjutsu
                    ]
          ]
        }
      ]
      , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Naraka Path Pain" 0
    "Having taken over the body of a priest, Pain now acts through it as one of his Six Paths. Naraka Path's specialty is summoning and controlling the King of Hell, which shields and supports his allies by draining the strength of those it considers unworthy."
    [Akatsuki, SRank]
    [ [ Skill.new
        { Skill.name      = "Summoning: King of Hell"
        , Skill.desc      = "Pain calls upon a timeless being tied to the power of the rinnegan which exists beyond life and death. The King of Hell provides 20 permanent destructible defense to Pain. While Pain has destructible defense from the King of Hell, this skill becomes [Energy Transfer][g]."
        , Skill.classes   = [Summon]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ defend' Permanent 20
                [ Alternate "Summoning: King of Hell"
                            "Energy Transfer"
                ]
          ]
        }
      , Skill.new
        { Skill.name      = "Energy Transfer"
        , Skill.desc      = "Pain restores health to himself or an ally equal to his remaining defense from [Summoning: King of Hell]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Ally do
                defense <- user defenseAmount "Summoning: King of Hell"
                when (defense > 0) $
                    heal defense
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Choke Hold"
        , Skill.desc      = "Pain seizes an enemy by the throat, dealing 20 damage to them and stunning their non-mental skills for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 skillName [Stun NonMental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Judgment"
        , Skill.desc      = "Judging an enemy unworthy, the King of Hell absorbs 20 of their health. If Pain has destructible defense from [Summoning: King of Hell], the absorbed health is added to its destructible defense. Absorbs 20 additional health if the target is affected by [Choke Hold]."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                bonus <- 20 `bonusIf` target has "Choke Hold"
                leech (20 + bonus) $ increaseDefense "Summoning: King of Hell"
          ]
        }
      ]
    , [ invuln "Block" "Pain" [Physical] ]
    ]
  , Character
    "Nagato" 0
    "Nagato leads the Akatsuki as the six-bodied Pain. His true body has remained safely hidden for years, acting through the Gedo statue. Though vulnerable without his Paths to defend him, Nagato's rinnegan makes him a formidable opponent."
    [RainVillage, Akatsuki, Sensor, SRank, Fire, Wind, Lightning, Earth, Water, Yang, Uzumaki]
    [ [ Skill.new
        { Skill.name      = "Summoning: Gedo Statue"
        , Skill.desc      = "Nagato summons the empty vessel of the ten-tailed beast, which provides 10 points of damage reduction to him for 3 turns. While active, this skill becomes [Control][r]."
        , Skill.require   = UserChannel False "Rinne Rebirth"
        , Skill.classes   = [Summon, Unremovable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 4
        , Skill.dur       = Action -4
        , Skill.effects   =
          [ To Self do
                controlStacks <- user numStacks "Control"
                let maxReduce = min 25 $ 10 + 5 * controlStacks
                applyWith [Controlled] 1 skillName
                    [ Reduce [All] Flat maxReduce
                    , Alternate "Summoning: Gedo Statue"
                                "Control"
                    ]
          ]
        , Skill.end       =
          [ To Self $ remove "Control" ]
        }
      , Skill.new
        { Skill.name      = "Control"
        , Skill.desc      = "Nagato attempts to maintain control over the Gedo statue for a little longer, prolonging [Summoning: Gedo Statue] for 2 additional turns. Until it ends, [Summoning: Gedo Statue] provides 5 additional points of damage reduction up to a maximum of 25 and [Phantom Dragon] deals 5 additional damage. This skill has no chakra cost if [Phantom Dragon] was used last turn."
        , Skill.require   = UserChannel False "Rinne Rebirth"
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                addStack skillName
                prolongChannel "Summoning: Gedo Statue" 2
          ]
        , Skill.changes   = changeWith "Phantom Dragon" $ setCost []
        }
      ]
    , [ Skill.new
        { Skill.name      = "Phantom Dragon"
        , Skill.desc      = "Nagato summons a dragon to attack an enemy for 20 piercing damage. Costs [g] during [Summoning: Gedo Statue]."
        , Skill.require   = UserChannel False "Rinne Rebirth"
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To Self $ tag 1 skillName
          , To Enemy do
                controlStacks <- user numStacks "Control"
                pierce (20 + 5 * controlStacks)
          ]
        , Skill.changes   = changeWithChannel "Summoning: Gedo Statue"
                          $ setCost [Gen]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rinne Rebirth"
        , Skill.desc      = "Nagato draws on the strength of the Outer Path to infuse life into his comrades at the cost of his own. He restores 15 health to both living and dead allies for 3 turns, during which he cannot use his other skills. Resurrected allies are cleared of all effects and their cooldowns are reset. When this skill ends, Nagato dies."
        , Skill.classes   = [Mental, Necromancy]
        , Skill.cost      = [Blood, Gen, Nin]
        , Skill.dur       = Control 3
        , Skill.charges   = 1
        , Skill.effects   =
          [ To XAllies do
                targetAlive <- target alive
                if targetAlive then
                    heal 15
                else do
                    factory
                    setHealth 15
          ]
        , Skill.end       =
          [ To Self killHard ]
        }
      ]
    , [ (invuln "Rinnegan Foresight" "Nagato" [Mental])
        { Skill.require = UserChannel False "Rinne Rebirth" }
      ]
    ]
  , Character
    "Konan" 0
    "One of the founding members of Akatsuki, Konan is an elegant origamist from the Hidden Rain Village. Along with her closeness to god-like Nagato, her ability to fly with paper wings has earned her the title of Angel. Although Akatsuki has strayed far from its original methodologies, Konan holds fast to her goal of bringing peace to the world."
    [RainVillage, Akatsuki, Sensor, SRank, Wind, Earth, Water, Yang]
    [ [ Skill.new
        { Skill.name      = "Paper Cut"
        , Skill.desc      = "Konan slices an enemy with a razor-sharp blade made of durable paper strips, dealing 25 piercing damage. Deals 5 additional damage if the target is affected by [Dance of the Shikigami]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` target has "Dance of the Shikigami"
                pierce (25 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Dance of the Shikigami"
        , Skill.desc      = "Konan transforms into sheets of paper that wrap around an enemy, dealing 15 damage to them for 2 turns. While active, the target's physical and mental skills are stunned, and they are invulnerable to allies."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 3
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 1 skillName
                    [ Stun Physical
                    , Stun Mental
                    , Alone
                    ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Paper Shower"
        , Skill.desc      = "The Angel of Akatsuki fires countless razor-sharp paper strips from her wings, dealing 25 damage to an enemy and 25 to a random enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 25
          , To REnemy $ damage 25
          ]
        }
      ]
    , [ invuln "Paper Clone" "Konan" [Chakra] ]
    ]
  ]
