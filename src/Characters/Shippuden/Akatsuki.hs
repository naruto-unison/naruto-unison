{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Akatsuki (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Madara Uchiha"
    "The co-founder of the Hidden Leaf Village along with Hashirama Senju, Madara turned against his friend in pursuit of absolute, unrivaled power as a means to break the cycle of violence and establish lasting peace. Cynical and bitter, Madara works toward what he believes to be humanity's benefit without sparing a thought for those who get in his way."
    [ [ Skill.new
        { Skill.name      = "Mangekyō Sharingan"
        , Skill.desc      = "Madara protects himself by predicting enemy attacks. For 4 turns, all non-affliction damage he receives is reduced to 25 at most. While active, this skill becomes [Eternal Mangekyō Sharingan][r]."
        , Skill.classes   = [Mental]
        , Skill.cost      = [Rand]
        , Skill.dur       = Ongoing 4
        , Skill.start     =
          [ To Self $ vary "Mangekyō Sharingan" "Eternal Mangekyō Sharingan" ]
        , Skill.effects   =
          [ To Self $ apply 1 [Limit 25]
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
    "Kisame Hoshigaki"
    "An Akatsuki member and one of the Seven Swordsmen of the Mist, Kisame is an S-Rank rogue ninja who hunts and captures tailed beasts. His water techniques and legendary sword Samehada flood his enemies."
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
        , Skill.desc      = "Spends all stacks of [Hundred Hungry Sharks] to deal 5 piercing damage per stack to an enemy. Costs 1 random chakra during [Exploding Water Shockwave]."
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
        , Skill.dur       = Ongoing 3
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
    "Deidara"
    "An S-Rank rogue ninja from the Hidden Stone Village, Deidara has begrudgingly joined Akatsuki after losing in a bet to Itachi. As a former member of the Explosion Corps, he posesses the unusual ability to turn clay into explosives by infusing it with lightning chakra. Most of his reckless decisions can be attributed to his pride and his love of art, which usually outweighs any other priorities."
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
        , Skill.classes   = [Chakra, Ranged, InvisibleTraps, Nonstacking]
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
        , Skill.desc      = "Deidara fills his veins with explosives and becomes art. If he his health is at or below 40, he deals 35 affliction damage to all enemies and dies."
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
    "Hidan"
    "An S-Rank rogue ninja from the Hidden Hotspring Village, Hidan joined Akatsuki to learn the secrets of its members. He belongs to a cult that worships Jashin, a bloodthirsty and murderous god who blesses him with immortality. With no need to fear death, he binds his soul to his enemies and tortures himself endlessly."
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
        , Skill.classes   = [Soulbound, Uncounterable, Unreflectable, Unremovable]
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
        , Skill.desc      = "Silently praying to Lord Jashin, Hidan prevents his health from dropping below 1 for 1 turn. Each time this skill is used, it costs 1 additional random chakra and its effect lasts 1 additional turn. Cannot be used while active."
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
    "Hiruko Sasori"
    "The Akatsuki member Sasori is rarely seen outside of Hiruko, a heavily armored shell that allows him to perform puppetry without the usual weakness of leaving himself exposed. Its poisonous stinger and hidden traps make it a threat that cannot be ignored."
    [ [ Skill.new
        { Skill.name      = "Scorpion Tail Constriction"
        , Skill.desc      = "Sasori shifts Hiruko into offensive mode and seizes an enemy, dealing 10 damage and stunning their non-mental skills for 1 turn. The target receives 10 additional damage from skills for the rest of the turn. Once used, this skill becomes [Scorpion Tail Strike][t][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 10
                apply (-1) [Stun NonMental, Bleed All Flat 10]
          , To Self $ vary "Scorpion Tail Constriction" "Scorpion Tail Strike"
          ]
        }
      , Skill.new
        { Skill.name      = "Scorpion Tail Strike"
        , Skill.desc      = "Hiruko's poison-drenched tail stabs at an enemy, dealing 20 damage and 20 affliction damage. For 2 turns, the target's skills cost 2 additional random chakra."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                damage 20
                afflict 20
                apply 2 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hidden Toxic Needles"
        , Skill.desc      = "Sasori shifts Hiruko into a defensive stance and takes aim at his enemies for 1 turn. If the enemy team uses any skills, they will take 15 affliction damage for 2 turns. At the end of the turn, Sasori gains 20 permanent destructible defense."
        , Skill.classes   = [Bane, Physical, Ranged, InvisibleTraps]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self $ delay 1 $ defend 0 20
          , To Self $ tag (-1)
          , To Enemies do
                delay (-1) $ whenM (userHas "needles at the ready") $
                    apply 2 [Afflict 15]
                trap (-1) (OnAction All) $
                    self $ hide' "needles at the ready" 1 []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Defensive Puppeteering"
        , Skill.desc      = "Hiruko swings his tail about to knock back enemies, gaining 10 destructible defense and restoring 10 health for 2 turns. While active, Sasori ignores effects that prevent him from reducing damage or becoming invulnerable."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand, Rand]
        , Skill.dur       = Control 2
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self do
                defend 1 10
                heal 10
                apply 1 [Ignore $ Only Expose]
          ]
        }
      ]
    , [ invuln "Tail Block" "Sasori" [Physical] ]
    ]
  , Character
    "Kazekage Puppeteer Sasori"
    "Sasori's most prized human puppet is the body of the Third Kazekage, which allows him to wield its magnetic abilities. As Sasori's last resort, this puppet favors all-out attack. Its enemies must deal with it quickly before its iron sand takes over the battlefield."
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
        , Skill.desc      = "Sasori emits a cloud of poisonous gas, dealing 15 affliction damage to all enemies. Next turn, enemy cooldowns are increased by 1 and enemy chakra costs are increased by 1 random. Lasts 2 turns on targets pinned by [Thousand Arms]."
        , Skill.classes   = [Bane, Ranged]
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
    "True Form Sasori"
    "Having invented and perfected the art of human puppetry, Sasori accomplished its ultimate act: transforming himself into a living puppet. His immortal core now resides in an unnaturally youthful simulacrum filled to the brim with tools of slaughter, each of which he switches out for another as soon as he uses it."
    [ [ Skill.new
        { Skill.name      = "Poisonous Chain Skewer"
        , Skill.desc      = "Sasori hooks an enemy with the poison-soaked steel ropes inside his body and pulls himself to them, dealing 5 affliction damage for 3 turns. Next turn, the target can only target Sasori or themselves. Once used, this skill becomes [Impale][t]."
        , Skill.classes   = [Bane, Ranged, Unreflectable]
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
        , Skill.classes   = [Ranged, Unreflectable]
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
    "Animal Path Pain"
    "Having taken over the body of a ninja from the Hidden Rain Village named Ajisai, Pain now acts through it as one of his Six Paths. Animal Path's specialization is summoning giant creatures that continue to fight for her even if she is immobilized."
    [ [ Skill.new
        { Skill.name      = "Summoning: Giant Centipede"
        , Skill.desc      = "Pain summons a huge centipede behind an enemy to ambush them. It deals 15 damage to them for 2 turns, and if the target does not use a skill during that time, they will be stunned for 1 turn. Once used, this skill becomes [Summoning: Giant Crustacean][r][r]."
        , Skill.classes   = [Physical, Melee, Summon]
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
        , Skill.classes   = [Chakra, Ranged, Summon]
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
        , Skill.classes   = [Physical, Summon]
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
        , Skill.classes   = [Physical, Melee, Summon, Bypassing, Unreflectable]
        , Skill.cost      = [Blood, Rand ]
        , Skill.dur       = Ongoing 0
        , Skill.start     =
          [ To Enemies $ tag 2
          , To Allies $ trapFrom 0 (Counter Uncounterable) do
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
    , [ invuln "Summoning: Giant Chameleon" "Pain" [Physical, Summon] ]
    ]
  , Character
    "Preta Path Pain"
    "Having taken over the body of a farmer from the Hidden Grass Village, Pain now acts through it as one of his Six Paths. Preta Path's specialization is absorbing chakra and nullifying ninjutsu abilities."
    [ [ Skill.new
        { Skill.name      = "Chakra Shield"
        , Skill.desc      = "Pain creates a protective barrier around himself and his allies which reflects the next skill used on each."
        , Skill.classes   = [Chakra, Ranged, Invisible, Nonstacking, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Allies $ apply 0 [Reflect] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Preta Drain"
        , Skill.desc      = "Pain absorbs an enemy's chakra, dealing 25 damage and absorbing 1 random chakra."
        , Skill.classes   = [Melee, Chakra]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                damage 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ninjutsu Absorption"
        , Skill.desc      = "Pain nullifies an enemy's chakra, preventing them from using skills that cost bloodline or ninjutsu chakra for 1 turn."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun Bloodline, Stun Ninjutsu] ]
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
        , Skill.classes   = [Mental, Summon]
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
    "Asura Path Pain"
    "Having taken over the body of a wandering puppeteer, Pain now acts through it as one of his Six Paths. Asura Path's body is heavily augmented with ballistic and mechanical weaponry."
    [ [ Skill.new
        { Skill.name      = "Metal Blade"
        , Skill.desc      = "Pain attacks an enemy with a blade that unfolds from his body, dealing 15 piercing damage and inflicting a deep wound. The target takes 10 affliction damage each turn until one of their allies uses a skill on them. Does not stack."
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
        , Skill.desc      = "Deals 20 damage to the target of [Guided Missile] and prevents them from reducing damage or becoming invulnerable for 2 turns."
        , Skill.require   = HasU 1 "Guided Missile"
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ To Enemies do
                damage 25
                apply 2 [Expose]
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
                interrupt $ const True
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
        , Skill.desc      = "Pain creates a gravitational anchor that pulls in an enemy and accumulates a rough sphere of rock and debris around them, applying 80 destructible barrier for 3 turns. While the target has destructible barrier from this skill, they are immune to effects from allies and invulnerable. At the end of the 3 turns, the target will receive damage equal to the remaining destructible barrier from this skill."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Gen, Tai]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy $
                barrierDoes 3 damage (apply 1 [Invulnerable All, Seal]) 80
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ]
  , Character
    "Nagato"
    "Nagato leads the Akatsuki as the six-bodied Pain. His true body has remained safely hidden for years, acting through the Gedo statue. Though vulnerable without his Paths to defend him, Nagato's rinnegan makes him a formidable opponent."
    [ [ Skill.new
        { Skill.name      = "Summoning: Gedo Statue"
        , Skill.desc      = "Nagato summons the empty vessel of the ten-tailed beast, which provides 10 points of damage reduction to him for 3 turns. While active, Nagato can use his other skills and this skill becomes [Control][r]."
        , Skill.classes   = [Mental, Summon, Unremovable]
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
  ]
