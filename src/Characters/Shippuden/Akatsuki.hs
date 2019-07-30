{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Akatsuki (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Kisame Hoshigaki"
    "An Akatsuki member and one of the Seven Swordsmen of the Mist, Kisame is an S-Rank rogue ninja who hunts and captures tailed beasts. His water techniques and legendary sword Samehada flood his enemies."
    [ [ Skill.new
        { Skill.name      = "Thousand Hungry Sharks"
        , Skill.desc      = "A school of sharks erupts around Kisame. He gains ten stacks of [Hundred Hungry Sharks]. Each turn, the sharks deal 5 piercing damage to all enemies, spending one stack per enemy hit. The first enemy to use a skill on Kisame will be marked, causing the sharks to ignore other enemies until the target dies. Deals 5 additional damage to each enemy during [Exploding Water Shockwave]. Once used, this skill becomes [Man-Eating Sharks][n]."
        , Skill.classes   = [Chakra, Ranged, Single, Unreflectable, Resource]
        , Skill.cost      = [Nin]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Self do
                addStacks "Hundred Hungry Sharks" 10
                trapFrom' 0 (OnHarmed All) do
                    enemies $ hide' "ignored" 0 []
                    remove "ignored"
                    tag 0
                    trap' 0 OnDeath $ everyone $ remove "ignored"
                    self $ removeTrap "Thousand Hungry Sharks"
          ]
        , Skill.effects   =
          [ p Enemies $ unlessM (targetHas "ignored") do
                bonus <- 5 `bonusIf` channeling "Exploding Water Shockwave"
                pierce (5 + bonus)
                self $ removeStack "Hundred Hungry Sharks"
          , p Self $ unlessM (userHas "Hundred Hungry Sharks") do
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
          [ p Enemy do
                stacks <- userStacks "Hundred Hungry Sharks"
                pierce (5 * stacks)
          , p Self do
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
        , Skill.channel   = Ongoing 3
        , Skill.start     =
          [ p Self do
                setFace 3
                vary "Exploding Water Shockwave" "Shark Dance"
          ]
        , Skill.effects   = [ p Enemies $ apply 1 [Snare 1] ]
        }
      , Skill.new
        { Skill.name      = "Shark Dance"
        , Skill.desc      = "Deals 20 damage to an enemy and absorbs 1 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ p Enemy do
                absorb 1
                damage 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Shark Bomb"
        , Skill.desc      = "Kisame traps an enemy for 1 turn. At the end of their turn, the target takes 30 damage. If they use a harmful chakra or mental skill while active, they will be countered and receive 20 additional damage."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Invisible]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Enemy do
                trap 1 (OnCounter Chakra) flag
                trap 1 (OnCounter Mental) flag
                delay (-1) do
                    bonus <- 20 `bonusIf` targetHas "Super Shark Bomb"
                    damage (30 + bonus)
          ]
        }
      ]
    , [ invuln "Scale Shield" "Kisame" [Physical] ]
    ] []
  , Character
    "Deidara"
    "An S-Rank rogue ninja from the Hidden Stone Village, Deidara has begrudgingly joined Akatsuki after losing in a bet to Itachi. As a former member of the Explosion Corps, he posesses the unusual ability to turn clay into explosives by infusing it with lightning chakra. Most of his reckless decisions can be attributed to his pride and his love of art, which usually outweighs any other priorities."
    [ [ Skill.new
        { Skill.name      = "C1: Bird Bomb"
        , Skill.desc      = "Deidara hurls a clay bird at an enemy that explodes into shrapnel on impact, dealing 15 damage to the target and weakening their non-affliction damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C3: Megaton Sculpture][n][r]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , Skill.classes   = [Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 15
                apply 4 [ Weaken All Flat 5]
          , p Self $ vary "C1: Bird Bomb" "C3: Megaton Sculpture"
          ]
        }
      , Skill.new
        { Skill.name      = "C3: Megaton Sculpture"
        , Skill.desc      = "Deidara drops a large explosive on the enemy team, dealing 20 damage to them and weakening their non-affliction damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C1: Bird Bomb]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , Skill.classes   = [Chakra, Ranged, Nonstacking]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ p Enemies do
                damage 20
                apply 4 [ Weaken All Flat 5]
          , p Self $ vary "C1: Bird Bomb" baseVariant
          ]
        }
      , Skill.new
        { Skill.name      = "C2: Dragon Missile"
        , Skill.desc      = "Deidara's dragon fires a long-range bomb at an opponent, dealing 30 damage and weakening their non-affliction damage by 5 for 4 turns. Does not stack."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Nonstacking]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
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
          [ p Self do
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
          [ p Enemy $ trap (-2) (OnAction NonMental) do
                removeTrap "C2: Minefield"
                damage 10
                apply 4 [Weaken All Flat 5]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "C4: Karura"
        , Skill.desc      = "A cloud of microscopic bombs enter an enemy's bloodstream and repeatedly detonate, dealing 10 affliction damage to the target each turn for the rest of the game and weakening their non-affliction damage by 5. Once used, this skill becomes [C0: Ultimate Art][b][n][n]."
        , Skill.classes   = [Bane, Chakra, Ranged, Uncounterable, Unremovable, Unreflectable]
        , Skill.cost      = [Blood, Nin]
        , Skill.effects   =
          [ p Enemy $ apply 0 [Afflict 10, Weaken All Flat 5]
          , p Self  $ vary "C4: Karura" "C0: Ultimate Art"
          ]
        }
      , Skill.new
        { Skill.name      = "C0: Ultimate Art"
        , Skill.desc      = "Deidara fills his veins with explosives and becomes art. If he his health is at or below 40, he deals 35 affliction damage to all enemies and dies."
        , Skill.classes   = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Blood, Nin, Nin]
        , Skill.effects   =
          [ p Enemies do
                hp <- user health
                when (hp <= 40) $ afflict 35
          , p Self do
                hp <- user health
                when (hp <= 40) killHard
          ]
        }
      ]
    , [ invuln "Clay Clone" "Deidara" [Chakra] ]
    ] []
  , Character
    "Hidan"
    "An S-Rank rogue ninja from the Hidden Hotspring Village, Hidan joined Akatsuki to learn the secrets of its members. He belongs to a cult that worships Jashin, a bloodthirsty and murderous god who blesses him with immortality. With no need to fear death, he binds his soul to his enemies and tortures himself endlessly."
    [ [ Skill.new
        { Skill.name      = "Jashin Sigil"
        , Skill.require   = HasI (-1) "Jashin Sigil"
        , Skill.desc      = "Hidan prepares for his ritual by drawing an insignia on the ground in blood. Once used, this skill becomes [First Blood][r]."
        , Skill.classes   = [Physical, Single, Unremovable, Uncounterable, Unreflectable]
        , Skill.effects   =
          [ p Self do
                tag 0
                vary "Jashin Sigil" "First Blood"
          ]
        }
      , Skill.new
        { Skill.name      = "First Blood"
        , Skill.desc      = "Searching for a victim to join him in his ritual of death, Hidan deals 5 damage to an opponent and marks them for 2 turns. For 2 turns, this skill becomes [Blood Curse Ritual][g]."
        , Skill.classes   = [Physical, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 5
                tag 2
                self $ vary' 2 "Jashin Sigil" "Blood Curse"
          ]
        }
      , Skill.new
        { Skill.name      = "Blood Curse Ritual"
        , Skill.desc      = "Hidan begins his ritual by drinking the blood of [First Blood]'s target, instantly using [Prayer] and then linking himself to them for 3 turns. While active, harmful skills used on Hidan and the target are also reflected to each other and this skill becomes [Death Blow][t][g]. Hidan ignores status effects from enemies except chakra cost changes, although his target does not. Damage that Hidan deals to himself while Linked to a living target heals him instead."
        , Skill.require   = HasU "First Blood"
        , Skill.classes   = [Soulbound, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ p Self do
                stacks <- userStacks "jashin"
                apply' "Prayer" (1 + stacks) [Endure]
                hide' "jashin" 0 []
          ,  p Enemies do
                userSlot   <- user slot
                targetSlot <- target slot
                apply' "Blood Curse" 3 [Share userSlot]
                trap 3 OnDeath $ self $ remove "bloodlink"
                self do
                    hide' "bloodlink" 3 []
                    bomb' "Blood Curse" 3 [Enrage, Share targetSlot]
                        [ p Done do
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
          [ p Self do
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
          [ p Self do
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
        , Skill.classes   = [Mental, Single, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Self do
                stacks <- userStacks "jashin"
                apply (1 + stacks) [Endure]
                hide' "jashin" 0 []
          ]
        , Skill.changes   = costPer "jashin" [Rand]
        }
      ]
    , [ invuln "Block" "Hidan" [Physical] ]
    ] []
  , Character
    "Zetsu"
    "After Madara turned the Gedo statue's mutated victims into an army of servants, he chose one to lead them. Imbuing the White Zetsu entity with materialized will in the form of Black Zetsu, he created a hybrid being who became an official member of Akatsuki. White Zetsu and Black Zetsu have different approaches to combat, but both are able to take control of an enemy's abilities."
    [ [ Skill.new
        { Skill.name      = "White Zetsu"
        , Skill.desc      = "Zetsu's white half takes over, canceling [Black Zetsu]. While active, Zetsu gains 5 permanent destructible defense each turn. Once used, this skill becomes [Black Zetu]."
        , Skill.classes   = [Chakra, Single]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Self do
                cancelChannel "Black Zetsu"
                setFace 0
                vary "White Zetsu" "Black Zetsu"
                vary "Black Zetsu" "White Army"
                vary "Doppelgänger / Body Coating" "Doppelgänger"
          ]
        , Skill.effects   =
          [ p Self $ defend 0 5 ]
        }
      , Skill.new
        { Skill.name      = "Black Zetsu"
        , Skill.desc      = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains 1 random chakra every other turn. Once used, this skill becomes [White Zetsu]."
        , Skill.classes   = [Chakra, Single]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Self do
                cancelChannel "White Zetsu"
                setFace 0
                vary "White Zetsu" baseVariant
                vary "Black Zetsu" "Underground Roots"
                vary "Doppelgänger / Body Coating" "Body Coating"
          ]
        , Skill.effects   =
          [ p Self $ unlessM (userHas "chakra") do
                gain [Rand]
                hide' "chakra" 1 []
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Black Zetsu"
        , Skill.desc      = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains 1 random chakra every other turn. Once used, this skill becomes [Underground Roots][b][r]. As White Zetsu, this skill becomes [White Army][g]."
        , Skill.classes   = [Chakra, Single]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Self do
                cancelChannel "White Zetsu"
                setFace 0
                vary "White Zetsu" baseVariant
                vary "Black Zetsu" "Underground Roots"
                vary "Doppelgänger / Body Coating" "Body Coating"
          ]
        , Skill.effects   =
          [ p Self $ unlessM (userHas "chakra") do
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
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ p Enemy do
                damage 20
                apply 1 [Weaken All Percent 50]
          ]
        }
      , Skill.new
        { Skill.name      = "White Army"
        , Skill.desc      = "Zetsu creates numerous clones of himself which deal 5 damage to all enemies for 5 turns. As Black Zetsu, this skill becomes [Underground Roots][b][r]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen]
        , Skill.channel   = Ongoing 5
        , Skill.effects   =
          [ p Enemies $ damage 5 ]
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
        , Skill.desc      = "Zetsu melts and flows over an enemy, taking control of their body. The next skill they use will target allies instead of enemies and enemies instead of allies. Does not stack. As White Zetsu, this skill becomes [Doppelgänger][t][r]."
        , Skill.classes   = [Mental, Melee, Single, Invisible, Unreflectable]
        , Skill.cost      = [Blood, Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy $ apply 0 [Swap All] ]
        }
      , Skill.new
        { Skill.name      = "Doppelgänger"
        , Skill.desc      = "Zetsu seizes an enemy and alters his chakra to match their own, dealing 20 damage, absorbing 1 random chakra, and stunning their non-mental skills for 1 turn. The last skill they used replaces this skill for 1 turn. Zetsu's copy of their skill has no chakra cost and ends when this skill reverts. As Black Zetsu, this skill becomes [Body Coating][b][g]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                absorb 1
                copyLast 1 2
                apply 1 [Stun NonMental]
                damage 20
          ]
        }
      ]
    , [ invuln "Hide" "Zetsu" [Physical] ]
    ] []
  , Character
    "Hiruko Sasori"
    "The Akatsuki member Sasori is rarely seen outside of Hiruko, a heavily armored shell that allows him to perform puppetry without the usual weakness of leaving himself exposed. Its poisonous stinger and hidden traps make it a threat that cannot be ignored."
    [ [ Skill.new
        { Skill.name      = "Scorpion Tail Constriction"
        , Skill.desc      = "Sasori shifts Hiruko into offensive mode and seizes an enemy, dealing 10 damage and stunning their non-mental skills for 1 turn. The target receives 10 additional damage from skills for the rest of the turn. Once used, this skill becomes [Scorpion Tail Strike][t][r]."
        , Skill.classes   = [Bane, Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 10
                apply (-1) [Stun NonMental, Bleed All Flat 10 ]
          , p Self $ vary "Scorpion Tail Constriction" "Scorpion Tail Strike"
          ]
        }
      , Skill.new
        { Skill.name      = "Scorpion Tail Strike"
        , Skill.desc      = "Hiruko's poison-drenched tail stabs at an enemy, dealing 20 damage and 20 affliction damage. For 2 turns, the target's skills cost 2 additional random chakra."
        , Skill.classes   = [Bane, Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 20
                afflict 20
                apply 1 [Exhaust All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hidden Toxic Needles"
        , Skill.desc      = "Sasori shifts Hiruko into a defensive stance and takes aim at his enemies for 1 turn. If the enemy team uses any skills, they will receive 15 affliction damage for 2 turns. At the end of the turn, Sasori gains 20 permanent destructible defense."
        , Skill.classes   = [Bane, Physical, Ranged, Single, InvisibleTraps]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self $ delay 1 $ defend 0 20
          , p Self $ tag (-1)
          , p Enemies do
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
        , Skill.channel   = Control 2
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Self do
                defend 1 10
                heal 10
                apply 1 [Ignore $ Only Expose]
          ]
        }
      ]
    , [ invuln "Tail Block" "Sasori" [Physical] ]
    ] []
  , Character
    "Kazekage Puppeteer Sasori"
    "Sasori's most prized human puppet is the body of the Third Kazekage, which allows him to wield its magnetic abilities. As Sasori's last resort, this puppet favors all-out attack. Its enemies must deal with it quickly before its iron sand takes over the battlefield."
    [ [ Skill.new
        { Skill.name      = "Kazekage Puppet Summoning"
        , Skill.desc      = "Sasori summons his most prized puppet, gaining 15 destructible defense and enabling his other skills. Once used, this skill becomes [Iron Sand: World Order][b][n]. Each turn, Sasori gains a stack of Iron Sand."
        , Skill.classes   = [Physical]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Self $ defend 0 15 ]
        , Skill.effects   =
          [ p Self $ apply' "Iron Sand" 0 [] ]
        }
      , Skill.new
        { Skill.name      = "Iron Sand: World Order"
        , Skill.desc      = "Using the third Kazekage's magnetic abilities, Sasori shapes his Iron Sand into a massive tangle of branching iron spikes that looms overhead. As it comes crashing down on the battlefield, it deals 10 piercing damage to all enemies and 5 additional damage per stack of Iron Sand."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemies do
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
        , Skill.channel   = Action 2
        , Skill.cooldown  = 3
        , Skill.start     =
          [ p Self do
                defend 2 20
                onBreak'
          ]
        , Skill.effects   =
          [ p Enemy do
                damage 10
                afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Thousand Arms"
        , Skill.desc      = "Countless concealed arms lash out from Sasori's Kazekage puppet and flail wildly for 1 turn, pinning down anyone they catch. Enemies who do not use harmful skills next turn will be pinned for 1 turn, unable to reduce damage or become invulnerable. While active, this skill becomes [Poison Gas][r][r]."
        , Skill.require   = HasI 1 "Iron Sand"
        , Skill.classes   = [Physical, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.channel   = Control 1
        , Skill.effects   =
          [ p Enemies $ trap (-1) OnHarm $ apply' "Pinned" (-1) [Expose]
          , p Self    $ vary "Thousand Arms" "Poison Gas"
          ]
        }
      , Skill.new
        { Skill.name      = "Poison Gas"
        , Skill.desc      = "Sasori emits a cloud of poisonous gas, dealing 15 affliction damage to all enemies. Next turn, enemy cooldowns are increased by 1 and enemy chakra costs are increased by 1 random. Lasts 2 turns on targets pinned by [Thousand Arms]."
        , Skill.classes   = [Bane, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ p Enemies do
                afflict 15
                bonus <- 1 `bonusIf` targetHas "Pinned"
                apply (1 + bonus) [Snare 1, Exhaust All]
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Sasori" [Chakra] ]
    ] []
  , Character
    "True Form Sasori"
    "Having invented and perfected the art of human puppetry, Sasori accomplished its ultimate act: transforming himself into a living puppet. His immortal core now resides in an unnaturally youthful simulacrum filled to the brim with tools of slaughter, each of which he switches out for another as soon as he uses it."
    [ [ Skill.new
        { Skill.name      = "Poisonous Chain Skewer"
        , Skill.desc      = "Sasori hooks an enemy with the poison-soaked steel ropes inside his body and pulls himself to them, dealing 5 affliction damage for 3 turns. Next turn, the target can only target Sasori or themselves. Once used, this skill becomes [Impale][t]."
        , Skill.classes   = [Bane, Ranged, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Enemy do
                apply 3 [Afflict 5]
                userSlot <- user slot
                apply 1 [Taunt userSlot]
          , p Self do
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
          [ p Self do
                vary "Poisonous Chain Skewer" baseVariant
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
          ,  p Enemy do
                  pierce 15
                  apply 2 [Afflict 5]
                  whenM (targetHas "Poisonous Chain Skewer") $
                      bomb' "Complex Toxin" 2 []
                            [ p Expire $ apply 1 [Stun All] ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Flamethrower Jets"
        , Skill.desc      = "Using fuel stored in a sealing scroll, Sasori shoots flames at an enemy for 3 turns, dealing 10 affliction damage each turn. While active, Sasori is invulnerable to all other enemies and ignores status effects from enemies except chakra cost changes. If Sasori uses any skill, [Flamethrower Jets] is canceled. After use, this skill becomes [Cutting Water Jets][n]."
        , Skill.classes   = [Ranged, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.channel   = Action 3
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy do
                afflict 10
                tag 1
                userSlot <- user slot
                self $ apply' "Flame Blast" 1 [Duel userSlot]
          , p Self do
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
          [ p Enemy do
                bonus <- 10 `bonusIf` targetHas "Flamethrower Jets"
                pierce (20 + bonus)
          , p Self do
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
        , Skill.classes   = [Physical, Single]
        , Skill.cost      = [Tai, Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.effects   =
          [ p Self do
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
                vary "Performance of a Hundred Puppets"
                     "Barrage of a Hundred Puppets"
                defend 0 50
                onBreak $ self $
                    vary "Performance of a Hundred Puppets" baseVariant
          , p XAllies $ defend 0 25
          ]
        }
      , Skill.new
        { Skill.name      = "Barrage of a Hundred Puppets"
        , Skill.desc      = "Sasori commands his puppet army to attack an enemy, dealing 30 damage and applying [Complex Toxin] to the target, which stuns them after 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 30
                bomb' "Complex Toxin" 2 [] [ p Expire $ apply 1 [Stun All] ]
          , p Self do
                cancelChannel "Flamethrower Jets"
                everyone do
                    remove "Flame Blast"
                    remove "Flamethrower Jets"
          ]
        }
      ]
    , [ invuln "Heart Switch" "Sasori" [Physical] ]
    ] []
  , Character
    "Animal Path Pain"
    "Having taken over the body of a ninja from the Hidden Rain Village named Ajisai, Pain now acts through it as one of his Six Paths. Animal Path's specialization is summoning giant creatures that continue to fight for her even if she is immobilized."
    [ [ Skill.new
        { Skill.name      = "Summoning: Giant Centipede"
        , Skill.desc      = "Pain summons a huge centipede behind an enemy to ambush them. It deals 15 damage to them for 2 turns, and if the target does not use a skill during that time, they will be stunned for 1 turn. Once used, this skill becomes [Summoning: Giant Crustacean][r][r]."
        , Skill.classes   = [Physical, Melee, Summon]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.channel   = Ongoing 2
        , Skill.start     =
          [ p Enemy do
                trap 2 (OnAction All) $ remove "Summoming: Giant Centipede"
                bomb 2 []
                    [ p Expire $ apply' "Giant Centipede Stun" 1 [Stun All] ]
          ,  p Self $ vary "Summoning: Giant Centipede"
                           "Summoning: Giant Crustacean"
          ]
        , Skill.effects   = [ p Enemy $ afflict 15 ]
        }
      , Skill.new
        { Skill.name      = "Summoning: Giant Crustacean"
        , Skill.desc      = "Pain summons a huge foaming lobster that sprays spittle over the battlefield. For 2 turns, all enemies take 10 affliction damage and their cooldowns are increased by 1 turn. While active, the lobster provides 10 points of damage reduction to Pain and her team. Once used, this skill becomes [Summoning: Giant Centipede][n]."
        , Skill.classes   = [Chakra, Ranged, Summon]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.channel   = Ongoing 2
        , Skill.start     =
          [ p Self $ vary "Summoning: Giant Centipede" baseVariant ]
        , Skill.effects   =
          [ p Enemies do
                afflict 10
                apply 1 [Exhaust All]
          , p Allies $ apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Giant Panda"
        , Skill.desc      = "Pain summons a huge panda that defends herself or an ally, providing 20 permanent destructible defense and making them invulnerable for 2 turns."
        , Skill.classes   = [Physical, Summon]
        , Skill.cost      = [Nin, Blood]
        , Skill.cooldown  = 4
        , Skill.channel   = Ongoing 2
        , Skill.start     =
          [ p Ally do
                defend 0 20
                apply 1 [Invulnerable All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Giant Multi-Headed Dog"
        , Skill.desc      = "Pain summons a huge Cerberus hound that deals 10 piercing damage to all enemies for 2 turns. The first enemy to use a harmful skill on Pain or her allies will extend the effect of this skill on them by 2 turns. Cannot be used while active."
        , Skill.require   = HasI (-1) "Summoning: Giant Multi-Headed Dog"
        , Skill.classes   = [Physical, Melee, Summon, Bypassing, Unreflectable]
        , Skill.cost      = [Blood, Rand ]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Enemies $ tag 2
          , p Allies $ apply 0 [Parry Uncounterable $ Play do
                unlessM (targetHas "already") do
                    prolong 2 "Summoning: Giant Multi-Headed Dog"
                    flag' "already"
                allies $ delay (-1) $
                    remove "Summoning: Giant Multi-Headed Dog"]
          ]
        , Skill.effects   =
          [ p Enemies $ whenM (targetHas "Summoning: Giant Multi-Headed Dog") do
                pierce 10
                self $ flag' "keep going"
          , p Self $ unlessM (userHas "keep going") $
                cancelChannel "Summoning: Giant Multi-Headed Dog"
          ]
        }
      ]
    , [ invuln "Summoning: Giant Chameleon" "Pain" [Physical, Summon] ]
    ] []
  , Character
    "Preta Path Pain"
    "Having taken over the body of a farmer from the Hidden Grass Village, Pain now acts through it as one of his Six Paths. Preta Path's specialization is absorbing chakra and nullifying ninjutsu abilities."
    [ [ Skill.new
        { Skill.name      = "Chakra Shield"
        , Skill.desc      = "Pain creates a protective barrier around himself and his allies which reflects the next skill used on each."
        , Skill.classes   = [Chakra, Ranged, Invisible, Single, Unreflectable]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Allies $ apply 0 [Reflect] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Preta Drain"
        , Skill.desc      = "Pain absorbs an enemy's chakra, dealing 25 damage and absorbing 1 random chakra."
        , Skill.classes   = [Melee, Chakra]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
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
          [ p Enemy $ apply 1 [Stun Bloodline, Stun Ninjutsu] ]
        }
      ]
      , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ] []
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
          [ p Self do
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
          [ p Ally do
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
          [ p Enemy do
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
          [ p Enemy do
                bonus <- 20 `bonusIf` targetHas "Choke Hold"
                leech (20 + bonus) $ self . addDefense "Summoning: King of Pain"
          ]
        }
      ]
    , [ invuln "Block" "Pain" [Physical] ]
    ] []
  , Character
    "Human Path Pain"
    "Having taken over the body of a ninja from the Hidden Waterfall Village, Pain now acts through it as one of his Six Paths. Human Path's specialty is drawing the souls of his enemies from their bodies to reveal their secrets and drain their lifeforce."
    [ [ Skill.new
        { Skill.name      = "Mind Invasion"
        , Skill.desc      = "Pain invades the mind of an enemy, dealing 15 damage. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ p Enemy do
                damage 15
                apply 1 [Reveal]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Spirit Absorption"
        , Skill.desc      = "Pain draws out the lifeforce of an enemy affected by [Mind Invasion], stealing 20 health and absorbing 1 random chakra. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.require   = HasU "Mind Invasion"
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                absorb 1
                apply 1 [ Reveal]
                leech 20 $ self . heal
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Soul Rip"
        , Skill.desc      = "Pain pulls out the soul of an enemy affected by [Mind Invasion], stealing 30 health. If their health reaches 30 or lower, they die; if not, he absorbs 1 random chakra from them and stuns them for 1 turn. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , Skill.require   = HasU "Mind Invasion"
        , Skill.classes   = [Mental, Melee, Unreflectable]
        , Skill.cost      = [Gen, Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                apply 1 [Stun All, Reveal]
                leech 30 $ self . heal
                hp <- target health
                if hp <= 30 then kill else absorb 1
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ] []
  , Character
    "Asura Path Pain"
    "Having taken over the body of a wandering puppeteer, Pain now acts through it as one of his Six Paths. Asura Path's body is heavily augmented with ballistic and mechanical weaponry."
    [ [ Skill.new
        { Skill.name      = "Metal Blade"
        , Skill.desc      = "Pain attacks an enemy with a blade that unfolds from his body, dealing 15 piercing damage. The target takes 10 affliction damage each turn until one of their allies uses a skill on them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
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
        , Skill.channel   = Action 2
        , Skill.start     =
          [ p Enemy $ purge
          , p Self  $ vary' 0 "Missile Salvo" "Head Cannon"
          ]
        , Skill.effects   = [ p Enemy $ damage 10 ]
        }
      , Skill.new
        { Skill.name      = "Head Cannon"
        , Skill.desc      = "Pain's head opens up to reveal a cannon, which explodes and deals 20 piercing damage to all enemies. Once used, this skill becomes [Missile Salvo][r][r]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemies $ pierce 20
          , p Self    $ vary "Missile Salvo" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Guided Missile"
        , Skill.desc      = "Pain fires a slow-moving but devastating missile at a target. Over the next four turns, the cost of this skill is 1 chakra that cycles through the different types of chakra. Each turn, it has a different effect on the target. Using the skill again resets it."
        , Skill.classes   = [Physical, Ranged, Bypassing, Invisible]
        , Skill.channel   = Ongoing 4
        , Skill.start     =
          [ p Self  $ hide' "missile" 1 []
          , p Enemy $ tag 4
          ]
        , Skill.effects   =
          [ p Self $ varyNext "Guided Missile" ]
        }
      , Skill.new
        { Skill.name      = "Bloodline Missile"
        , Skill.desc      = "Deals 25 damage to the target of [Guided Missile] and 25 damage to a random enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ p Enemies $ whenM (targetHas "Guided Missile") $ damage 25
          , p REnemy  $ damage 25
          , p Self do
                cancelChannel "Guided Missile"
                everyone $ remove "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Genjutsu Missile"
        , Skill.desc      = "Deals 20 damage to the target of [Guided Missile] and prevents them from reducing damage or becoming invulnerable for 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ p Enemies $ whenM (targetHas "Guided Missile") do
                damage 25
                apply 2 [Expose]
          , p Self do
              cancelChannel "Guided Missile"
              everyone $ remove "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Ninjutsu Missile"
        , Skill.desc      = "Deals 25 damage to the target of [Guided Missile] and stuns them for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ p Enemies $ whenM (targetHas "Guided Missile") do
                damage 25
                apply 1 [Stun All]
          , p Self do
                cancelChannel "Guided Missile"
                everyone $ remove "Guided Missile"
          ]
        }
      , Skill.new
        { Skill.name      = "Taijutsu Missile"
        , Skill.desc      = "Deals 30 piercing damage to the target of [Guided Missile]."
        , Skill.classes   = [Physical, Ranged, Bypassing]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ p Enemies $ whenM (targetHas "Guided Missile") $ pierce 30
          ,  p Self do
                cancelChannel "Guided Missile"
                everyone $ remove "Guided Missile"
          ]
        }
      ]
    , [ invuln "Flee" "Pain" [Physical] ]
    ] []
  , Character
    "Deva Path Pain"
    "Having taken over the body of Yahiko, his deceased best friend, Pain now acts through it as one of his Six Paths. To honor Yahiko's memory, Pain uses the Deva Path as the leader of the Six Paths and his main body when interacting with others. Deva Path's specialization is gravity manipulation, which he uses to impair and imprison his enemies."
    [ [ Skill.new
        { Skill.name      = "Almighty Push"
        , Skill.desc      = "Pain targets himself or an ally. The first harmful skill used on them next turn will be countered, and the person countered will receive 20 damage. Once used, this skill alternates between [Universal Pull] and [Almighty Push] each turn. "
        , Skill.classes   = [Chakra, Ranged, Invisible, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.channel   = Passive
        , Skill.start     =
          [ p Ally $ apply 1 [Parry All $ Play $ damage 20]
          , p Self $ tag' "Tidal Force" 1
          ]
        , Skill.effects   =
          [ p Self do
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
        , Skill.desc      = "Pain targets himself or an ally. The first harmful skill used on them next turn will be countered, and the person countered will receive 20 damage. This skill will become [Universal Pull] next turn."
        , Skill.classes   = [Chakra, Ranged, Invisible, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ p Ally $ apply 1 [Parry All $ Play $ damage 20]
          , p Self $ tag' "Tidal Force" 1
          ]
        }
      , Skill.new
        { Skill.name      = "Universal Pull"
        , Skill.desc      = "Pain manipulates gravity to pull an enemy toward him, ending their Action and Control skills in progress. Next turn, the target can only target Pain or themselves. If [Almighty Push] was used last turn, its effect is applied to Pain. This skill will become [Almighty Push] next turn."
        , Skill.classes   = [Chakra, Ranged, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ p Enemy do
                interrupt $ const True
                userSlot <- user slot
                apply 1 [Taunt userSlot]
          , p Self $ whenM (userHas "Tidal Force") $
                apply' "Almighty Push" 1 [Parry All $ Play $ damage 20]
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
          [ p Enemy do
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
          [ p Enemy $
                barrierDoes 3 damage (apply 1 [Invulnerable All, Seal]) 80
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Pain" [Mental] ]
    ] []
  , Character
    "Nagato"
    "Nagato leads the Akatsuki as the six-bodied Pain. His true body has remained safely hidden for years, acting through the Gedo statue. Though vulnerable without his Paths to defend him, Nagato's rinnegan makes him a formidable opponent."
    [ [ Skill.new
        { Skill.name      = "Summoning: Gedo Statue"
        , Skill.desc      = "Nagato summons the empty vessel of the ten-tailed beast, which provides 10 points of damage reduction to him for 3 turns. While active, Nagato can use his other skills and this skill becomes [Control][r]."
        , Skill.classes   = [Mental, Summon, Unremovable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 4
        , Skill.channel   = Control (-4)
        , Skill.start     =
          [ p Self do
                remove "gedo"
                remove "dragon"
          ]
        , Skill.effects   =
          [ p Self do
                vary' 1 "Summoning: Gedo Statue" "Control"
                vary' 1 "Phantom Dragon" "Phantom Dragon"
                dragonStacks <- userStacks "dragon"
                addStacks' 1 "Control" dragonStacks
                gedoStacks   <- userStacks "gedo"
                apply 1 [Reduce All Flat (10 + 5 * gedoStacks)]
          ]
        , Skill.interrupt =
          [ p Self do
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
          [ p Self do
                prolongChannel 2 "Summoning: Gedo Statue"
                hide' "dragon" 0 []
                stacks <- userStacks "gedo"
                when (stacks < 3) $ hide' "gedo" 0 []
          ]
        , Skill.changes   = changeWith "Phantom Dragon" $ setCost []
        }
      ]
    , [ Skill.new
        { Skill.name      = "Phantom Dragon"
        , Skill.desc      = "Nagato summons a dragon to attack an enemy for 20 piercing damage. Costs 1 genjutsu chakra during [Summoning: Gedo Statue]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.effects   =
          [ p Enemy $ pierce 20
          , p Self  $ tag 1
          ]
        }
      , Skill.new
        { Skill.name      = "Phantom Dragon"
        , Skill.desc      = "Nagato summons a dragon to attack an enemy for 20 piercing damage. During [Summoning: Gedo Statue], costs 1 genjutsu chakra and deals 5 additional damage per stack of [Control]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ p Enemy do
                stacks <- userStacks "dragon"
                pierce (20 + 5 * stacks)
          , p Self $ tag 1
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Rinne Rebirth"
        , Skill.desc      = "Nagato draws on the strength of the Outer Path to infuse life into himself and his allies. During each of the next 3 turns, Nagato restores 15 health to his team, resets their cooldowns, and gains 1 random chakra. Requires [Summoning: Gedo Statue]."
        , Skill.require   = HasI 1 "Summoning: Gedo Statue"
        , Skill.classes   = [Mental]
        , Skill.cost      = [Blood, Gen, Nin]
        , Skill.cooldown  = 6
        , Skill.channel   = Control 3
        , Skill.effects   =
          [ p Allies do
                heal 15
                resetAll
          , p Self $ gain [Rand]
          ]
        }
      ]
    , [ invuln "Rinnegan Foresight" "Nagato" [Mental] ]
    ] []
  , Character
    "Tobi"
    "A peculiar new member of the Akatsuki, Tobi claims to be Madara Uchiha even though Madara has been dead for many years. Using his Izanagi, he can rewind his state to an earlier point and even come back from the dead."
    [ [ Skill.new
        { Skill.name      = "Sharingan"
        , Skill.desc      = "Tobi analyzes the battlefield to gain the upper hand. The next time a harmful skill is used on him, it will be countered and this skill will become [Kamui][g][r] for 2 turns. Cannot be used while active."
        , Skill.classes   = [Mental, Invisible, Single]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ p Self $ apply 0 [Parry All $ Play do
                self $ tag' "Kamui" 2
                vary' (-2) "Sharingan" "Kamui"]
          ]
        }
      , Skill.new
        { Skill.name      = "Kamui"
        , Skill.desc      = "Tobi banishes a target to his pocket dimension for 3 turns, preventing them from affecting or being affected by anyone else. If used on an ally, cures all harmful effects on them. If used on an enemy, deals 20 piercing damage and prevents them from reducing damage or becoming invulnerable. Ends if Tobi uses [Kamui Strike] on someone else."
        , Skill.classes   = [Chakra, Ranged, Single, Unreflectable]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p XAlly do
                cureAll
                userSlot <- user slot
                apply 3 [Duel userSlot]
          , p Enemy do
                pierce 20
                userSlot <- user slot
                apply 3 [Duel userSlot, Expose]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kamui Strike"
        , Skill.desc      = "Tobi teleports behind an enemy and deals 20 piercing damage to them. Deals 20 additional damage if the target is affected by [Kamui]."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Gen]
        , Skill.effects   =
          [ p Enemy do
                has <- targetHas "Kamui"
                if has then
                    pierce 40
                else do
                    everyone $ remove "Kamui"
                    pierce 20
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Izanagi"
        , Skill.desc      = "Tobi sacrifices one of his eyes to take control of reality on a local scale. 2 turns from now, he will be restored to his current state."
        , Skill.classes   = [Mental, Invisible]
        , Skill.cost      = [Blood, Blood]
        , Skill.charges   = 2
        , Skill.effects   =
          [ p Self $ snapshot 2 ]
        }
      ]
    , [ invuln "Phase" "Tobi" [Chakra] ]
    ] []
  ]
