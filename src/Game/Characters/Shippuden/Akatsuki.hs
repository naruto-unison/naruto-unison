{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Shippuden.Akatsuki (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Madara Uchiha"
    "The co-founder of the Hidden Leaf Village along with Hashirama Senju, Madara turned against his friend in pursuit of absolute, unrivaled power as a means to break the cycle of violence and establish lasting peace. Cynical and bitter, Madara works toward what he believes to be humanity's benefit without sparing a thought for those who get in his way."
    [ [ Skill.new
        { Skill.name      = "Mangekyō Sharingan"
        , Skill.desc      = "Madara protects himself by predicting enemy attacks. For 4 turns, all non-affliction damage he receives is reduced to 25 at most. While active, this skill becomes [Eternal Mangekyō Sharingan][r]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.dur       = Instant
        , Skill.effects   =
          [ To Self do
                apply 4 [Limit 25]
                vary "Mangekyō Sharingan" "Eternal Mangekyō Sharingan"
          ]
        }
      , Skill.new
        { Skill.name      = "Eternal Mangekyō Sharingan"
        , Skill.desc      = "By predicting enemy attacks, Madara ignores status effects from enemies except chakra cost changes for 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ apply 1 [Enrage] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Madara encases himself in spectral armor that provides him with 70 permanent destructible defense. While Madara has destructible defense from this skill, he gains a stack of [Susanoo] each turn and this skill becomes [Armored Susanoo Assault][b][r]."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Blood]
        , Skill.cooldown  = 6
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self do
                defend 0 70
                vary "Susanoo" "Armored Susanoo Assault"
                onBreak'
          ]
        , Skill.effects   =
          [ To Self addStack ]
        }
      , Skill.new
        { Skill.name      = "Armored Susanoo Assault"
        , Skill.desc      = "Wielding a massive spectral blade, Madara deals 30 damage to an enemy. Deals 5 additional damage per stack of [Susanoo]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Susanoo"
                damage (30 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Majestic Destroyer Flame"
        , Skill.desc      = "Madara immolates the battlefield, dealing 10 damage to an enemy and 5 damage to all other enemies for 3 turns. While active, enemies who use skills that grant damage reduction or destructible defense will receive 10 damage."
        , Skill.classes   = [Affliction, Bane]
        , Skill.cost      = [Nin]
        , Skill.dur       = Action 3
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 10
                trap 1 OnDefend $ damage 10
                trap 1 OnReduce $ damage 10
          , To XEnemies do
                damage 5
                trap 1 OnDefend $ damage 10
                trap 1 OnReduce $ damage 10
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Madara" [Chakra] ]
    ]
  , Character
    "Deidara"
    "An Akatsuki member who defected from the Hidden Stone Village's Explosion Corps, Deidara posesses the unusual ability to turn clay into explosives by infusing it with lightning chakra. Most of his reckless decisions can be attributed to his pride and love of art, which usually outweigh any other priorities."
    [ [ Skill.new
        { Skill.name      = "C1: Bird Bomb"
        , Skill.desc      = "Deidara hurls a clay bird at an enemy that explodes into shrapnel on impact, dealing 15 damage to the target and weakening their damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C3: Megaton Sculpture][n][r]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , Skill.classes   = [Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 4 [ Weaken All Flat 5]
          , To Self $ vary "C1: Bird Bomb" "C3: Megaton Sculpture"
          ]
        }
      , Skill.new
        { Skill.name      = "C3: Megaton Sculpture"
        , Skill.desc      = "Deidara drops a large explosive on the enemy team, dealing 20 damage to them and weakening their damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C1: Bird Bomb]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , Skill.classes   = [Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemies do
                damage 20
                apply 4 [ Weaken All Flat 5]
          , To Self $ vary "C1: Bird Bomb" baseVariant
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
                apply 4 [Weaken All Flat 5]
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
                defend 3 35
                vary' 3 "C1: Bird Bomb"   "C2: Dragon Missile"
                vary' 3 "C2: Clay Dragon" "C2: Minefield"
          ]
        }
      , Skill.new
        { Skill.name      = "C2: Minefield"
        , Skill.desc      = "Deidara scatters mines that burrow into the ground around an enemy. The next time they use a non-mental skill within 2 turns, they will take 10 damage and their non-affliction damage will be weakened by 5 for 4 turns. Does not stack."
        , Skill.classes   = [Chakra, Ranged, Invisible, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap (-2) (OnAction NonMental) do
                removeTrap "C2: Minefield"
                damage 10
                apply 4 [Weaken All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "C4: Karura"
        , Skill.desc      = "A cloud of microscopic bombs enter an enemy's bloodstream and repeatedly detonate, dealing 10 affliction damage to the target each turn for the rest of the game and weakening their damage by 5. Once used, this skill becomes [C0: Ultimate Art][b][n][n]."
        , Skill.classes   = [Bane, Chakra, Ranged, Uncounterable, Unremovable, Unreflectable]
        , Skill.cost      = [Blood, Nin]
        , Skill.effects   =
          [ To Enemy $ apply 0 [Afflict 10, Weaken All Flat 5]
          , To Self  $ vary "C4: Karura" "C0: Ultimate Art"
          ]
        }
      , Skill.new
        { Skill.name      = "C0: Ultimate Art"
        , Skill.desc      = "Deidara fills his veins with explosives and becomes art. If his health is at or below 40, he deals 35 affliction damage to all enemies and dies."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood, Nin, Nin]
        , Skill.effects   =
          [ To Enemies do
                hp <- user health
                when (hp <= 40) $ afflict 35
          , To Self do
                hp <- user health
                when (hp <= 40) killHard
          ]
        }
      ]
    , [ invuln "Clay Clone" "Deidara" [Chakra] ]
    ]
  , Character
    "Sasori"
    "An Akatsuki member who defected from the Hidden Sand Village's Puppet Brigade, Sasori of the Red Sand is as hollow and soullesss as his playthings. Obsessed with creating human puppets, Sasori prizes above all others the body of the Third Kazekage, through which he can wield magnetic abilities."
    [ [ Skill.new
        { Skill.name      = "Kazekage Puppet Summoning"
        , Skill.desc      = "Sasori summons his most prized puppet, gaining 15 permanent destructible defense and enabling his other skills. Once used, this skill becomes [Iron Sand: World Order][b][n]. Each turn, Sasori gains a stack of Iron Sand."
        , Skill.classes   = [Physical]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self $ defend 0 15 ]
        , Skill.effects   =
          [ To Self $ apply' "Iron Sand" 0 [] ]
        }
      , Skill.new
        { Skill.name      = "Iron Sand: World Order"
        , Skill.desc      = "Using the third Kazekage's magnetic abilities, Sasori shapes his Iron Sand into a massive tangle of branching iron spikes that looms overhead. As it comes crashing down on the battlefield, it deals 10 piercing damage to all enemies and 5 additional damage per stack of Iron Sand."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies do
                stacks <- userStacks "Iron Sand"
                pierce (10 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Poison Blade Assault"
        , Skill.desc      = "Sasori directs the Kazekage puppet to single out an enemy and gains 20 destructible defense for 2 turns. While Sasori has destructible defense from this skill, he deals 10 damage and 10 affliction damage to the target."
        , Skill.require   = HasI 1 "Iron Sand"
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.dur       = Action 2
        , Skill.cooldown  = 3
        , Skill.start     =
          [ To Self do
                defend 2 20
                onBreak'
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
        , Skill.desc      = "Countless concealed arms lash out from Sasori's Kazekage puppet and flail wildly for 1 turn, pinning down anyone they catch. Enemies who do not use skills on Sasori or his allies next turn will be pinned for 1 turn, unable to reduce damage or become invulnerable. While active, this skill becomes [Poison Gas][r][r]."
        , Skill.require   = HasI 1 "Iron Sand"
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.dur       = Control 1
        , Skill.effects   =
          [ To Enemies $ trap (-1) OnHarm $ apply' "Pinned" (-1) [Expose]
          , To Self    $ vary "Thousand Arms" "Poison Gas"
          ]
        }
      , Skill.new
        { Skill.name      = "Poison Gas"
        , Skill.desc      = "Sasori emits a cloud of poisonous gas, dealing 15 affliction damage to all enemies. Next turn, enemy cooldowns are increased by 1 and enemy chakra costs are increased by 1 arbitrary chakra. Lasts 2 turns on targets pinned by [Thousand Arms]."
        , Skill.classes   = [Physical, Bane, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemies do
                afflict 15
                bonus <- 1 `bonusIf` targetHas "Pinned"
                apply (1 + bonus) [Snare 1, Exhaust All]
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Sasori" [Chakra] ]
    ]
  , Character
    "Hidan"
    "An Akatsuki member who defected from the Hidden Hotspring Village, Hidan belongs to a cult that worships Jashin, a bloodthirsty and murderous god who blesses him with immortality. With no need to fear death, he binds his soul to his enemies and tortures himself endlessly."
    [ [ Skill.new
        { Skill.name      = "Jashin Sigil"
        , Skill.require   = HasI (-1) "Jashin Sigil"
        , Skill.desc      = "Hidan prepares for his ritual by drawing an insignia on the ground in blood. Once used, this skill becomes [First Blood][r]."
        , Skill.classes   = [Physical, Unremovable, Uncounterable, Unreflectable]
        , Skill.effects   =
          [ To Self do
                tag 0
                vary "Jashin Sigil" "First Blood"
          ]
        }
      , Skill.new
        { Skill.name      = "First Blood"
        , Skill.desc      = "Searching for a victim to join him in his ritual of death, Hidan deals 5 damage to an opponent and marks them for 2 turns. For 2 turns, this skill becomes [Blood Curse Ritual][g]."
        , Skill.classes   = [Bane, Physical, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 5
                tag 2
                self $ vary' 2 "Jashin Sigil" "Blood Curse"
          ]
        }
      , Skill.new
        { Skill.name      = "Blood Curse Ritual"
        , Skill.desc      = "Hidan begins his ritual by drinking the blood of [First Blood]'s target, instantly using [Prayer] and then linking himself to them for 3 turns. While active, skills used on Hidan and the target by their opponents are also reflected to each other, and this skill becomes [Death Blow][t][g]. Hidan ignores status effects from enemies except chakra cost changes, although his target does not. Damage that Hidan deals to himself while Linked to a living target heals him instead."
        , Skill.require   = HasU 1 "First Blood"
        , Skill.classes   = [Chakra, Soulbound, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Self do
                stacks <- userStacks "jashin"
                apply' "Prayer" (1 + stacks) [Endure]
                hide' "jashin" 0 []
          ,  To Enemies do
                userSlot   <- user slot
                targetSlot <- target slot
                apply' "Blood Curse" 3 [Share userSlot]
                trap 3 OnDeath $ self $ remove "bloodlink"
                self do
                    hide' "bloodlink" 3 []
                    bomb' "Blood Curse" 3 [Enrage, Share targetSlot]
                        [ To Done do
                              remove "Jashin Sigil"
                              remove "bloodlink"
                              vary "Jashin Sigil" baseVariant
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
                has <- userHas "bloodlink"
                if has then do
                    heal 50
                    enemies $ whenM (targetHas "Blood Curse") $ pierce 50
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
                has <- userHas "bloodlink"
                if has then do
                    heal 35
                    enemies $ whenM (targetHas "Blood Curse") $ pierce 35
                else
                    sacrifice 0 35
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Prayer"
        , Skill.desc      = "Silently praying to Lord Jashin, Hidan prevents his health from dropping below 1 for 1 turn. Each time this skill is used, it costs 1 additional arbitrary chakra and its effect lasts 1 additional turn. Cannot be used while active."
        , Skill.require   = HasI (-1) "Prayer"
        , Skill.classes   = [Mental, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                stacks <- userStacks "jashin"
                apply (1 + stacks) [Endure]
                hide' "jashin" 0 []
          ]
        , Skill.changes   =
            costPer "jashin" [Rand]
        }
      ]
    , [ invuln "Block" "Hidan" [Physical] ]
    ]
  , Character
    "Kakuzu"
    "The self-proclaimed Treasurer of the Akatsuki, Kakuzu is a money-obsessed bounty hunter who defected from the Hidden Waterfall Village in pursuit of wealth. With his anger usually going unchecked, he has a bad habit of ripping out the heart of anyone who annoys him. He stores extra hearts in masks, each of which grants different abilities."
    [ [ Skill.new
        { Skill.name     = "Pressure Damage"
        , Skill.desc     = "Attaching his wind-element mask, Kakuzu fires a tornado of compressed air at an enemy, dealing 30 damage and stunning their chakra and ranged skills for 1 turn. Once used, this skill becomes [Searing Migraine][b]."
        , Skill.classes  = [Chakra, Ranged]
        , Skill.cost     = [Nin, Rand]
        , Skill.effects  =
          [ To Enemy do
                damage 30
                apply 1 [Stun Chakra, Stun Ranged]
          , To Self $ vary "Pressure Damage" "Searing Migraine"
          ]
        }
      , Skill.new
        { Skill.name      = "Searing Migraine"
        , Skill.desc      = "Attaching his fire-element mask, Kakuzu creates a firestorm that spreads across the battlefield, dealing 15 affliction damage to all enemies. Once used, this skill becomes [Pressure Damage][n][r]."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ afflict 15
          , To Self    $ vary "Pressure Damage" baseVariant
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
          , To Self  $ vary "False Darkness" "Blast Flames"
          ]
        }
      , Skill.new
        { Skill.name      = "Blast Flames"
        , Skill.desc      = "Combining his fire-element and wind-element masks, Kakuzu creates a fiery tornado that deals 35 damage to an enemy and 20 damage to all other enemies. Once used, this skill becomes [False Darkness][g][r]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.effects   =
          [ To Enemy    $ damage 35
          , To XEnemies $ damage 20
          , To Self     $ vary "False Darkness" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Earth Grudge"
        , Skill.desc      = "Black thread-like tendrils rip apart an enemy whose health is at 20 or lower, killing the target and restoring 35 health to Kakuzu if successful."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemy do
                targetHealth <- target health
                when (targetHealth <= 20) do
                    kill
                    unlessM (target alive) $ self $ heal 35
          ]
        }
      ]
    , [ invuln "Iron Skin" "Kakuzu" [Physical] ]
    ]
  , Character
    "Kisame Hoshigaki"
    "An Akatsuki member and one of the Seven Swordsmen of the Mist, Kisame is a rogue operative who hunts and captures tailed beasts. His water techniques and legendary sword Samehada flood his enemies."
    [ [ Skill.new
        { Skill.name      = "Thousand Hungry Sharks"
        , Skill.desc      = "A school of sharks erupts around Kisame. He gains ten stacks of [Hundred Hungry Sharks]. Each turn, the sharks deal 5 piercing damage to all enemies, spending one stack per enemy hit. The first enemy to use a skill on Kisame will be marked, causing the sharks to ignore other enemies until the target dies. Deals 5 additional damage to each enemy during [Exploding Water Shockwave]. Once used, this skill becomes [Man-Eating Sharks][n]."
        , Skill.require   = HasI (-1) "Thousand Hungry Sharks"
        , Skill.classes   = [Chakra, Ranged, Unreflectable, Resource]
        , Skill.cost      = [Nin]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self do
                addStacks "Hundred Hungry Sharks" 10
                trapFrom' 0 (OnHarmed All) do
                    enemies $ hide' "ignored" 0 []
                    remove "ignored"
                    tag 0
                    trap' 0 OnDeath $ everyone $ remove "ignored"
                    self $ removeTrap "Thousand Hungry Sharks"
          ]
        , Skill.effects   =
          [ To Enemies $ unlessM (targetHas "ignored") do
                bonus <- 5 `bonusIf` channeling "Exploding Water Shockwave"
                pierce (5 + bonus)
                self $ removeStack "Hundred Hungry Sharks"
          , To Self $ unlessM (userHas "Hundred Hungry Sharks") do
                cancelChannel "Thousand Hungry Sharks"
                everyone do
                    remove "ignored"
                    remove "Thousand Hungry Sharks"
          ]
        }
      , Skill.new
        { Skill.name      = "Man-Eating Sharks"
        , Skill.desc      = "Spends all stacks of [Hundred Hungry Sharks] to deal 5 piercing damage per stack to an enemy. Costs 1 arbitrary chakra during [Exploding Water Shockwave]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Hundred Hungry Sharks"
                pierce (5 * stacks)
          , To Self do
                cancelChannel "Thousand Hungry Sharks"
                remove "Hundred Hungry Sharks"
                everyone do
                    remove "ignored"
                    remove "Thousand Hungry Sharks"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Exploding Water Shockwave"
        , Skill.desc      = "As a giant orb of water fills the entire battlefield, Kisame merges with Samehada and transforms into a shark for 3 turns. While active, enemy cooldowns are increased by 1 and this skill becomes [Shark Dance][t]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 4
        , Skill.dur       = Action 3
        , Skill.start     =
          [ To Self do
                setFace
                vary "Exploding Water Shockwave" "Shark Dance"
          ]
        , Skill.effects   = [ To Enemies $ apply 1 [Snare 1] ]
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
        , Skill.effects   =
          [ To Enemy do
                trap 1 (Countered Chakra) flag
                trap 1 (Countered Mental) flag
                delay (-1) do
                    bonus <- 20 `bonusIf` targetHas "Super Shark Bomb"
                    damage (30 + bonus)
          ]
        }
      ]
    , [ invuln "Scale Shield" "Kisame" [Physical] ]
    ]
  , Character
    "Itachi Uchiha"
    "An Akatsuki member who defected from the Hidden Leaf Village, Itachi is known as the Clan Killer for slaughtering the rest of the Uchihas, sparing only his brother. Plagued by a lethal disease that saps his strength, Itachi has been forced to go on the defensive. Out of other options, he now plays his trump card: the legendary armor Susanoo, created by the power of the mangekyō sharingan."
    [ [ Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Itachi loses 10 health and encases himself in spectral armor that provides him with 5 permanent destructible defense every turn. This skill can be used again with no chakra cost to cancel its effect."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Self do
                sacrifice 0 10
                vary "Susanoo" "Susanoo"
                vary "Amaterasu" "Totsuka Blade"
                vary "Mirage Crow" "Yata Mirror"
          ]
        , Skill.effects   =
          [ To Self $ defend 0 5 ]
        }
      , Skill.new
        { Skill.name      = "Susanoo"
        , Skill.desc      = "Ends the effect of [Susanoo]."
        , Skill.classes   = [Chakra]
        , Skill.effects   =
          [ To Self $ cancelChannel "Susanoo" ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amaterasu"
        , Skill.desc      = "Itachi sets an enemy on fire, dealing 15 affliction damage to them for 2 turns. During [Susanoo], this skill becomes [Totsuka Blade][g]."
        , Skill.classes   = [Bane, Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 2 [Afflict 15] ]
        }
      , Skill.new
        { Skill.name      = "Totsuka Blade"
        , Skill.desc      = "Itachi slashes an enemy with an ethereal liquid blade, dealing 25 piercing damage and depleting a bloodline or genjutsu chakra."
        , Skill.classes   = [Chakra, Melee]
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
        , Skill.desc      = "Itachi traps an enemy in an illusion. If they use a skill on Itachi or his allies next turn, their physical and ranged skills will be stunned for 2 turns. During [Susanoo], this skill becomes [Yata Mirror][g][r]."
        , Skill.classes   = [Mental, Ranged, Invisible]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $
                trap 1 (Countered All) $ apply 2 [Stun Physical, Stun Ranged]
          ]
        }
      , Skill.new
        { Skill.name      = "Yata Mirror"
        , Skill.desc      = "Itachi defends himself with an ethereal shield. Next turn, Itachi ignores enemy skills, and enemies who use skills on Itachi will have the costs of their skills increased by 1 additional arbitrary chakra for 1 turn."
        , Skill.classes    = [Chakra, Ranged, Invisible]
        , Skill.cost       = [Gen, Rand]
        , Skill.cooldown   = 2
        , Skill.effects    =
          [ To Self do
                trapFrom 1 (OnHarmed All) $ apply 1 [Exhaust All]
                apply 1 [Nullify]
          ]
        }
      ]
    , [ invuln "Dodge" "Itachi" [Physical] ]
    ]
  , Character
    "Konan"
    "One of the founding members of Akatsuki, Konan is an elegant origamist from the Hidden Rain Village. Her ability to fly with paper wings has earned her the title of Angel. Although Akatsuki has strayed far from its original methodologies, Konan holds fast to her goal of bringing peace to the world."
    [ [ Skill.new
        { Skill.name      = "Paper Cut"
        , Skill.desc      = "Konan slices an enemy with a razor-sharp blade made of durable paper strips, dealing 25 piercing damage."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemy $ pierce 25 ]
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
                apply 1 [Stun Physical, Stun Mental, Alone]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Paper Shower"
        , Skill.desc      = "The Angel of Akatsuki fires countless razor-ship paper strips from her wings, dealing 25 damage to an enemy and 25 to a random enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy  $ damage 25
          , To REnemy $ damage 25
          ]
        }
      ]
    , [ invuln "Paper Clone" "Konan" [Chakra] ]
    ]
  , Character
    "Nagato"
    "Nagato leads the Akatsuki as the six-bodied Pain. His true body has remained safely hidden for years, acting through the Gedo statue. Though vulnerable without his Paths to defend him, Nagato's rinnegan makes him a formidable opponent."
    [ [ Skill.new
        { Skill.name      = "Summoning: Gedo Statue"
        , Skill.desc      = "Nagato summons the empty vessel of the ten-tailed beast, which provides 10 points of damage reduction to him for 3 turns. While active, this skill becomes [Control][r]."
        , Skill.classes   = [Summon, Unremovable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 4
        , Skill.dur       = Control (-4)
        , Skill.start     =
          [ To Self do
                remove "gedo"
                remove "dragon"
          ]
        , Skill.effects   =
          [ To Self do
                vary' 1 "Summoning: Gedo Statue" "Control"
                dragonStacks <- userStacks "dragon"
                addStacks' 1 "Control" dragonStacks
                gedoStacks   <- userStacks "gedo"
                apply 1 [Reduce All Flat (10 + 5 * gedoStacks)]
          ]
        , Skill.interrupt =
          [ To Self do
                remove "Summoning: Gedo Statue"
                remove "Control"
          ]
        }
      , Skill.new
        { Skill.name      = "Control"
        , Skill.desc      = "Nagato attempts to maintain control over the Gedo statue for a little longer, prolonging [Summoning: Gedo Statue] for 2 additional turns. Until it ends, [Summoning: Gedo Statue] provides 5 additional points of damage reduction up to a maximum of 25 and [Phantom Dragon] deals 5 additional damage. This skill has no chakra cost if [Phantom Dragon] was used last turn."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                prolongChannel 2 "Summoning: Gedo Statue"
                hide' "dragon" 0 []
                stacks <- userStacks "gedo"
                when (stacks < 3) $ hide' "gedo" 0 []
          ]
        , Skill.changes   =
            changeWith "Phantom Dragon" \x -> x { Skill.cost = [] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Phantom Dragon"
        , Skill.desc      = "Nagato summons a dragon to attack an enemy for 20 piercing damage. Costs 1 genjutsu chakra during [Summoning: Gedo Statue]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ To Self  $ tag 1
          , To Enemy $ pierce 20

          ]
        , Skill.changes   =
            changeWithChannel "Summoning: Gedo Statue" \x ->
              x { Skill.cost    = [Gen]
                , Skill.effects =
                  [ To Self  $ tag 1
                  , To Enemy do
                        stacks <- userStacks "dragon"
                        pierce (20 + 5 * stacks)
                  ]
                }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rinne Rebirth"
        , Skill.desc      = "Nagato draws on the strength of the Outer Path to infuse life into himself and his allies. During each of the next 3 turns, Nagato restores 15 health to his team, resets their cooldowns, and gains 1 random chakra. Requires [Summoning: Gedo Statue]."
        , Skill.require   = HasI 1 "Summoning: Gedo Statue"
        , Skill.classes   = [Mental]
        , Skill.cost      = [Blood, Gen, Nin]
        , Skill.cooldown  = 6
        , Skill.dur       = Control 3
        , Skill.effects   =
          [ To Allies do
                heal 15
                resetAll
          , To Self $ gain [Rand]
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Nagato" [Mental] ]
    ]
  , Character
    "Deva Path Pain"
    "Having taken over the body of Yahiko, his deceased best friend, Pain now acts through it as one of his Six Paths. To honor Yahiko's memory, Pain uses the Deva Path as the leader of the Six Paths and his main body when interacting with others. Deva Path's specialization is gravity manipulation, which he uses to impair and imprison his enemies."
    [ [ Skill.new
        { Skill.name      = "Almighty Push"
        , Skill.desc      = "Pain targets himself or an ally. The first skill an enemy uses on them next turn will be countered, and the person countered will receive 20 damage. Once used, this skill alternates between [Universal Pull] and [Almighty Push] each turn. "
        , Skill.classes   = [Chakra, Ranged, Invisible, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.dur       = Passive
        , Skill.start     =
          [ To Ally $ trapFrom 1 (Counter All) $ damage 20
          , To Self $ tag' "Tidal Force" 1
          ]
        , Skill.effects   =
          [ To Self do
                has <- userHas "pull"
                if has then
                    vary "Almighty Push" "Almighty Push"
                else do
                    vary "Almighty Push" "Universal Pull"
                    hide' "pull" 1 []
          ]
        }
      , Skill.new
        { Skill.name      = "Almighty Push"
        , Skill.desc      = "Pain targets himself or an ally. The first skill an enemy uses on them next turn will be countered, and the person countered will receive 20 damage. This skill will become [Universal Pull] next turn."
        , Skill.classes   = [Chakra, Ranged, Invisible, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Ally $ trapFrom 1 (Counter All) $ damage 20
          , To Self $ tag' "Tidal Force" 1
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
                apply 1 [Taunt userSlot]
          , To Self $ whenM (userHas "Tidal Force") $
                trapFrom 1 (Counter All) $ damage 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Chakra Receiver"
        , Skill.desc      = "Pain pierces an enemy with a black rod that attunes them to his chakra, dealing 15 piercing damage and applying 15 permanent destructible barrier. Starting 1 turn from now, while the target has destructible barrier from this skill, they are stunned every other turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                barrierDoes 0 (const $ return ()) (do
                    has <- targetHas "receive"
                    if has then apply 1 [Stun All] else hide' "receive" 1 []
                  ) 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Planetary Devastation"
        , Skill.desc      = "Pain creates a gravitational anchor that pulls in an enemy and accumulates a rough sphere of rock and debris around them, applying 80 destructible barrier for 3 turns. While the target has destructible barrier from this skill, they are invulnerable to allies as well as enemies. At the end of the 3 turns, the target will receive damage equal to the remaining destructible barrier from this skill."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Gen, Tai]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy $
                barrierDoes 3 damage (apply 1 [Alone, Invulnerable All]) 80
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Asura Path Pain"
    "Having taken over the body of a wandering puppeteer, Pain now acts through it as one of his Six Paths. Asura Path's body is heavily augmented with ballistic and mechanical weaponry."
    [ [ Skill.new
        { Skill.name      = "Metal Blade"
        , Skill.desc      = "Pain gouges an enemy with a blade that unfolds from his body, dealing 15 piercing damage and inflicting a deep wound. The target takes 10 affliction damage each turn until one of their allies uses a skill on them. Does not stack."
        , Skill.classes   = [Bane, Physical, Melee, Nonstacking]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                pierce 15
                apply 0 [Afflict 10]
                trap 0 OnHelped $ remove "Metal Blade"
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
          , To Self  $ vary' 0 "Missile Salvo" "Head Cannon"
          ]
        , Skill.effects   = [ To Enemy $ damage 10 ]
        }
      , Skill.new
        { Skill.name      = "Head Cannon"
        , Skill.desc      = "Pain's head opens up to reveal a cannon, which explodes and deals 20 piercing damage to all enemies. Once used, this skill becomes [Missile Salvo][r][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies $ pierce 20
          , To Self    $ vary "Missile Salvo" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Guided Missile"
        , Skill.desc      = "Pain fires a slow-moving but devastating missile at a target. Over the next four turns, the cost of this skill is 1 chakra that cycles through the different types of chakra. Each turn, it has a different effect on the target. Using the skill again resets it."
        , Skill.classes   = [Physical, Ranged, Bypassing, Invisible]
        , Skill.dur       = Ongoing 4
        , Skill.start     =
          [ To Self  $ hide' "missile" 1 []
          , To Enemy $ tag 4
          ]
        , Skill.effects   =
          [ To Self $ varyNext "Guided Missile" ]
        }
      , Skill.new
        { Skill.name      = "Bloodline Missile"
        , Skill.desc      = "Deals 25 damage to the target of [Guided Missile] and 25 damage to a random enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ whenM (targetHas "Guided Missile") $ damage 25
          , To REnemy  $ damage 25
          , To Self do
                cancelChannel "Guided Missile"
                everyone $ remove "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Genjutsu Missile"
        , Skill.desc      = "Prevents the target of [Guided Missile] from reducing damage or becoming invulnerable for 2 turns and deals 25 damage."
        , Skill.require   = HasU 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemies do
                apply 2 [Expose]
                damage 25
          , To Self do
              cancelChannel "Guided Missile"
              everyone $ remove "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Ninjutsu Missile"
        , Skill.desc      = "Deals 25 damage to the target of [Guided Missile] and stuns them for 1 turn."
        , Skill.require   = HasU 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Enemies do
                damage 25
                apply 1 [Stun All]
          , To Self do
                cancelChannel "Guided Missile"
                everyone $ remove "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Taijutsu Missile"
        , Skill.desc      = "Deals 30 piercing damage to the target of [Guided Missile]."
        , Skill.require   = HasU 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged, Bypassing]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemies $ pierce 30
          ,  To Self do
                cancelChannel "Guided Missile"
                everyone $ remove "Guided Missile"
          ]
        }
      ]
    , [ invuln "Flee" "Pain" [Physical] ]
    ]
  , Character
    "Human Path Pain"
    "Having taken over the body of a ninja from the Hidden Waterfall Village, Pain now acts through it as one of his Six Paths. Human Path's specialty is drawing the souls of his enemies from their bodies to reveal their secrets and drain their lifeforce."
    [ [ Skill.new
        { Skill.name      = "Mind Invasion"
        , Skill.desc      = "Pain invades the mind of an enemy, dealing 15 damage. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 1 [Reveal]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Spirit Absorption"
        , Skill.desc      = "Pain draws out the lifeforce of an enemy affected by [Mind Invasion], stealing 20 health and absorbing 1 random chakra. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.require   = HasU 1 "Mind Invasion"
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                apply 1 [Reveal]
                leech 20 $ self . heal
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Soul Rip"
        , Skill.desc      = "Pain pulls out the soul of an enemy affected by [Mind Invasion], stealing 30 health. If their health reaches 30 or lower, they die; if not, he absorbs 1 random chakra from them and stuns them for 1 turn. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.require   = HasU 1 "Mind Invasion"
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Gen, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Stun All, Reveal]
                leech 30 $ self . heal
                hp <- target health
                if hp <= 30 then kill else absorb 1
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Animal Path Pain"
    "Having taken over the body of a ninja from the Hidden Rain Village named Ajisai, Pain now acts through it as one of his Six Paths. Animal Path's specialization is summoning giant creatures that continue to fight for her even if she is immobilized."
    [ [ Skill.new
        { Skill.name      = "Summoning: Giant Centipede"
        , Skill.desc      = "Pain summons a huge centipede behind an enemy to ambush them. It deals 15 damage to them for 2 turns, and if the target does not use a skill during that time, they will be stunned for 1 turn. Once used, this skill becomes [Summoning: Giant Crustacean][r][r]."
        , Skill.classes   = [Summon, Melee]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.dur       = Ongoing 2
        , Skill.start     =
          [ To Enemy do
                trap 2 (OnAction All) do
                    remove "Summoming: Giant Centipede"
                    removeTrap "Summoning: Giant Centipede"
                bomb 2 []
                    [ To Expire $ apply' "Giant Centipede Stun" 1 [Stun All] ]
          ,  To Self $ vary "Summoning: Giant Centipede"
                           "Summoning: Giant Crustacean"
          ]
        , Skill.effects   = [ To Enemy $ afflict 15 ]
        }
      , Skill.new
        { Skill.name      = "Summoning: Giant Crustacean"
        , Skill.desc      = "Pain summons a huge foaming lobster that sprays spittle over the battlefield. For 2 turns, all enemies take 10 damage and their cooldowns are increased by 1 turn. While active, the lobster provides 10 points of damage reduction to Pain and her team. Once used, this skill becomes [Summoning: Giant Centipede][n]."
        , Skill.classes   = [Summon, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Ongoing 2
        , Skill.start     =
          [ To Self $ vary "Summoning: Giant Centipede" baseVariant ]
        , Skill.effects   =
          [ To Enemies do
                damage 10
                apply 1 [Exhaust All]
          , To Allies $ apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Giant Panda"
        , Skill.desc      = "Pain summons a huge panda that defends herself or an ally, providing 20 permanent destructible defense and making them invulnerable for 2 turns."
        , Skill.classes   = [Summon]
        , Skill.cost      = [Nin, Blood]
        , Skill.cooldown  = 4
        , Skill.dur       = Ongoing 2
        , Skill.start     =
          [ To Ally do
                defend 0 20
                apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Giant Multi-Headed Dog"
        , Skill.desc      = "Pain summons a huge Cerberus hound that deals 10 piercing damage to all enemies for 2 turns. The first enemy to use a skill on Pain or her allies will extend the effect of this skill on them by 2 turns. Cannot be used while active."
        , Skill.require   = HasI (-1) "Summoning: Giant Multi-Headed Dog"
        , Skill.classes   = [Summon, Melee, Bypassing, Unreflectable]
        , Skill.cost      = [Blood, Rand ]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Enemies $ tag 2
          , To Allies $ trapFrom 0 (OnHarmed All) do
                unlessM (targetHas "already") do
                    prolong 2 "Summoning: Giant Multi-Headed Dog"
                    flag' "already"
                allies $ removeTrap "Summoning: Giant Multi-Headed Dog"
          ]
        , Skill.effects   =
          [ To Enemies $ whenM (targetHas "Summoning: Giant Multi-Headed Dog") do
                pierce 10
                self $ flag' "keep going"
          , To Self $ unlessM (userHas "keep going") $
                cancelChannel "Summoning: Giant Multi-Headed Dog"
          ]
        }
      ]
    , [ invuln "Summoning: Giant Chameleon" "Pain" [Summon] ]
    ]
  , Character
    "Preta Path Pain"
    "Having taken over the body of a farmer from the Hidden Grass Village, Pain now acts through it as one of his Six Paths. Preta Path's specialization is absorbing chakra and nullifying ninjutsu abilities."
    [ [ Skill.new
        { Skill.name      = "Chakra Shield"
        , Skill.desc      = "Pain creates a protective barrier around himself that absorbs chakra. Next turn, enemy skills used on him will be nullified, and Pain will gain chakra equal to the chakra cost of the nullified skill."
        , Skill.classes   = [Chakra, Ranged, Invisible, Nonstacking, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ apply 1 [Absorb, Nullify] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Preta Drain"
        , Skill.desc      = "Pain absorbs an enemy's energy, dealing 25 damage and regaining 10 health per chakra that they spent on their most recent skill."
        , Skill.classes   = [Melee, Chakra]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                healFromChakra 10
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
                apply 1 [Stun Bloodline, Stun Ninjutsu]
          ]
        }
      ]
      , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Naraka Path Pain"
    "Having taken over the body of a priest, Pain now acts through it as one of his Six Paths. Naraka Path's specialty is summoning and controlling the King of Hell, which shields and supports his allies by draining the strength of those it considers unworthy."
    [ [ Skill.new
        { Skill.name      = "Summoning: King of Hell"
        , Skill.desc      = "Pain calls upon a timeless being tied to the power of the rinnegan which exists beyond life and death. The King of Hell provides 20 permanent destructible defense to Pain. While Pain has destructible defense from the King of Hell, this skill becomes [Energy Transfer][g]."
        , Skill.classes   = [Summon]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                vary "Summoning: King of Hell" "Energy Transfer"
                defend 0 20
                onBreak $ self $ vary "Summoning: King of Hell" baseVariant
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
                defense <- userDefense "Summoning: King of Hell"
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
                apply 1 [Stun NonMental]
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
                bonus <- 20 `bonusIf` targetHas "Choke Hold"
                leech (20 + bonus) $ self . addDefense "Summoning: King of Pain"
          ]
        }
      ]
    , [ invuln "Block" "Pain" [Physical] ]
    ]
  , Character
    "Zetsu"
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
  ]
