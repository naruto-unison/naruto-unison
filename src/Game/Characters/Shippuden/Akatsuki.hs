{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden.Akatsuki (akatsukiCsS) where

import StandardLibrary
import Game.Functions
import Game.Game
import Game.Structure

akatsukiCsS :: [Character]
akatsukiCsS = 
  [ Character
    "Kisame Hoshigaki"
    "An Akatsuki member and one of the Seven Swordsmen of the Mist, Kisame is an S-Rank rogue ninja who hunts and captures tailed beasts. His water techniques and legendary sword Samehada flood his enemies."
    [ [ newSkill
        { label   = "Thousand Hungry Sharks"
        , desc    = "A school of sharks erupts around Kisame. He gains ten stacks of [Hundred Hungry Sharks]. Each turn, the sharks deal 5 piercing damage to all enemies, spending one stack per enemy hit. The first enemy to use a skill on Kisame will be marked, causing the sharks to ignore other enemies until the target dies. Deals 5 additional damage to each enemy during [Exploding Water Shockwave]. Once used, this skill becomes [Man-Eating Sharks][n]."
        , classes = [Chakra, Ranged, Single, Unreflectable, Resource]
        , cost    = χ [Nin]
        , channel = Ongoing 0
        , start   = [(Self, addStacks "Hundred Hungry Sharks" 10
                          • trapFrom' 0 (OnHarmed All) §
                            ( enemyTeam § hide' "ignored" 0 []
                            • remove "ignored" • tag 0 
                            • trap' 0 OnDeath . everyone § remove "ignored"
                            • self § removeTrap "Thousand Hungry Sharks"
                            ))]
        , effects = [ (Enemies, ifnotU "ignored" 
                              $ withChan "Exploding Water Shockwave" 5 pierce 5
                              • self § removeStack "Hundred Hungry Sharks") 
                    , (Self,    ifnotI "Hundred Hungry Sharks" 
                              $ cancelChannel "Thousand Hungry Sharks"
                              • everyone § remove "ignored" 
                                         ° remove "Thousand Hungry Sharks")
                    ]
        }
      , newSkill
        { label   = "Man-Eating Sharks"
        , desc    = "Spends all stacks of [Hundred Hungry Sharks] to deal 5 piercing damage per stack to an enemy. Costs 1 random chakra during [Exploding Water Shockwave]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin]
        , effects = [ (Enemy, perI "Hundred Hungry Sharks" 5 pierce 0) 
                    , (Self,  cancelChannel "Thousand Hungry Sharks"
                            • remove "Hundred Hungry Sharks"
                            • everyone § remove "ignored" 
                                       ° remove "Thousand Hungry Sharks")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Exploding Water Shockwave"
        , desc    = "Kisame creates a giant orb of water that fills the entire battlefield and merges with his legendary sword, becoming a shark for 3 turns. While active, enemy cooldowns are increased by 1 and this skill becomes [Shark Dance][t]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand, Rand]
        , cd      = 4
        , channel = Ongoing 3
        , start   = [(Self, setFace 3 
                          • vary "Exploding Water Shockwave" "Shark Dance")]
        , effects = [(Enemies, apply 1 [Snare 1])]
        }
      , newSkill
        { label   = "Shark Dance"
        , desc    = "Deals 20 damage to an enemy and steals a random chakra."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [(Enemy, steal 1 • damage 20)]
        }
      ]
    , [ newSkill
        { label   = "Super Shark Bomb"
        , desc    = "Kisame traps an enemy for 1 turn. At the end of their turn, the target takes 30 damage. If they use a harmful chakra or mental skill while active, they will be countered and receive 20 additional damage."
        , classes = [Chakra, Ranged, Bypassing, Invisible]
        , cost    = χ [Blood, Nin]
        , cd      = 4
        , effects = [ (Enemy, trap 1 (OnCounter Chakra) flag
                            • trap 1 (OnCounter Mental) flag
                            • delay (-1) 
                              § withU "Super Shark Bomb" 20 damage 30)
                    ]
        }
      ]
    , invuln "Scale Shield" "Kisame" [Physical]
    ] []
  , Character
    "Deidara"
    "An S-Rank rogue ninja from the Hidden Stone Village, Deidara has begrudgingly joined Akatsuki after losing in a bet to Itachi. As a former member of the Explosion Corps, he posesses the unusual ability to turn clay into explosives by infusing it with lightning chakra. Most of his reckless decisions can be attributed to his pride and his love of art, which usually outweighs any other priorities."
    [ [ newSkill
        { label   = "C1: Bird Bomb"
        , desc    = "Deidara hurls a clay bird at an enemy that explodes into shrapnel on impact, dealing 15 damage to the target and weakening their non-affliction damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C3: Megaton Sculpture][n][r]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , classes = [Chakra, Ranged, Nonstacking]
        , cost    = χ [Rand]
        , effects = [(Enemy, damage 15 • apply 4 [ Weaken All 5])
                    , (Self,  vary "C1: Bird Bomb" "C3: Megaton Sculpture")
                    ]
        }
      , newSkill
        { label   = "C3: Megaton Sculpture"
        , desc    = "Deidara drops a large explosive on the enemy team, dealing 20 damage to them and weakening their non-affliction damage by 5 for 4 turns. Does not stack. Once used, this skill becomes [C1: Bird Bomb]. During [C2: Clay Dragon], this skill becomes [C2: Dragon Missile][n][r]."
        , classes = [Chakra, Ranged, Nonstacking]
        , cost    = χ [Nin, Rand]
        , effects = [(Enemies, damage 20 • apply 4 [ Weaken All 5])
                    , (Self,    vary "C1: Bird Bomb" "")
                    ]
        }
      , newSkill
        { label   = "C2: Dragon Missile"
        , desc    = "Deidara's dragon fires a long-range bomb at an opponent, dealing 30 damage and weakening their non-affliction damage by 5 for 4 turns. Does not stack."
        , classes = [Chakra, Ranged, Bypassing, Nonstacking]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, damage 30 • apply 4 [Weaken All 5])]
        }
      ]
    , [ newSkill
        { label   = "C2: Clay Dragon"
        , desc    = "Deidara sculpts a dragon out of clay and takes off, gaining 35 destructible defense for 3 turns. While active, this skill becomes [C2: Minefield][r]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Rand]
        , cd      = 4
        , effects = [(Self, defend 3 35 
                          • vary' 3 "C1: Bird Bomb" "C2: Dragon Missile" 
                          • vary' 3 "C2: Clay Dragon" "C2: Minefield")]
        }
      , newSkill
        { label   = "C2: Minefield"
        , desc    = "Deidara scatters mines that burrow into the ground around an enemy. The next time they use a non-mental skill within 2 turns, they will take 10 damage and their non-affliction damage will be weakened by 5 for 4 turns. Does not stack."
        , classes = [Chakra, Ranged, InvisibleTraps, Nonstacking]
        , cost    = χ [Rand]
        , cd      = 2 
        , effects = [(Enemy, trap (-2) (OnAction NonMental)
                             § removeTrap "C2: Minefield"
                             ° damage 10
                             ° apply 4 [Weaken All 5])]
        }
      ]
    , [ newSkill
        { label   = "C4: Karura"
        , desc    = "Deidara releases a cloud of microscopic bombs that enter an enemy's bloodstream and repeatedly detonate, dealing 10 affliction damage to the target each turn for the rest of the game and weakening their non-affliction damage by 5. Once used, this skill becomes [C0: Ultimate Art][b][n][n]."
        , classes = [Bane, Chakra, Ranged, Uncounterable, Unremovable, Unreflectable]
        , cost    = χ [Blood, Nin]
        , effects = [ (Enemy, apply 0 [Afflict 10, Weaken All 5]) 
                    , (Self,  vary "C4: Karura" "C0: Ultimate Art")
                    ]
        }
      , newSkill
        { label   = "C0: Ultimate Art"
        , desc    = "Deidara fills his veins with explosives and becomes art. If he his health is at or below 40, he deals 35 affliction damage to all enemies and dies."
        , classes = [Chakra, Ranged, Bypassing, Uncounterable, Unreflectable]
        , cost    = χ [Blood, Nin, Nin]
        , effects = [ (Enemies, ifHealthI 0 40 § afflict 35) 
                    , (Self,    ifHealthI 0 40 § setHealth 0)
                    ]
        }
      ]
    , invuln "Clay Clone" "Deidara" [Chakra]
    ] []
  , Character
    "Hidan"
    "An S-Rank rogue ninja from the Hidden Hotspring Village, Hidan joined Akatsuki to learn the secrets of its members. He belongs to a cult that worships Jashin, a bloodthirsty and murderous god who blesses him with immortality. With no need to fear death, he binds his soul to his enemies and tortures himself endlessly."
    [ [ newSkill
        { label   = "Jashin Sigil"
        , require = HasI (-1) "Jashin Sigil"
        , desc    = "Hidan prepares for his ritual by drawing an insignia on the ground in blood. Once used, this skill becomes [First Blood][r]."
        , classes = [Physical, Single, Unremovable, Uncounterable, Unreflectable]
        , effects = [(Self, tag 0 • vary "Jashin Sigil" "First Blood")]
        }
      , newSkill
        { label   = "First Blood"
        , desc    = "Searching for a victim to join him in his ritual of death, Hidan deals 5 damage to an opponent and marks them for 2 turns. For 2 turns, this skill becomes [Blood Curse Ritual][g]."
        , classes = [Physical, Unreflectable, Unremovable]
        , cost    = χ [Rand]
        , effects = [(Enemy, damage 5 • tag 2 
                           • self § vary' 2 "Jashin Sigil" "Blood Curse")]
        }
      , newSkill
        { label   = "Blood Curse Ritual"
        , desc    = "Hidan begins his ritual by drinking the blood of [First Blood]'s target, instantly using [Prayer] and then linking himself to them for 3 turns. While active, harmful skills used on Hidan and the target are also reflected to each other and this skill becomes [Death Blow][t][g]. Hidan ignores harmful non-damage effects other than chakra cost changes, but any effects are still reflected to the Linked target. Damage that Hidan deals to himself while Linked to a living target heals him instead."
        , require = HasU "First Blood"
        , classes = [Soulbound, Uncounterable, Unreflectable, Unremovable]
        , cost    = χ [Gen]
        , effects = [ (Self, perI "jashin" 1 (applyDur' "Prayer" [Endure]) 1 
                           • hide' "jashin" 0 []) 
                    , (Enemies, apply' "Blood Curse" 3 [Reapply] 
                              • trap 3 OnDeath . self § remove "bloodlink"
                              • self 
                                § hide' "bloodlink" 3 []
                                ° bomb' "Blood Curse" 3 [Enrage, Reapply] 
                                  [(Done, remove "Jashin Sigil" 
                                        • remove "bloodlink"
                                        • vary "Jashin Sigil" "")])
                    ]
        }
      , newSkill
        { label   = "Death Blow"
        , desc    = "Hidan impales himself through his chest, dealing 50 piercing damage to himself."
        , classes = [Physical, Melee]
        , cost    = χ [Gen, Tai]
        , effects = [ (Self, ifnotI "bloodlink" § sacrifice 0 50
                           • ifI "bloodlink" 
                             § heal 50
                             ° enemyTeam (ifU "Blood Curse" § pierce 50)
                             ) ]
        }
      ]
    , [ newSkill
        { label   = "Self-Mutilation"
        , desc    = "Hidan tears a gash in his stomach with his scythe, dealing 35 piercing damage to himself and stunning himself for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , cd      = 1
        , effects = [(Self, apply 1 [Stun All]
                          • ifnotI "bloodlink" § sacrifice 0 35
                          • ifI "bloodlink" 
                          § heal 50 ° enemyTeam (ifU "Blood Curse" § pierce 35))]
        }
      ]
    , [ newSkill
        { label   = "Prayer"
        , desc    = "Silently praying to Lord Jashin, Hidan prevents his health from dropping below 1 for 1 turn. Each time this skill is used, it costs an additional random chakra and its effect lasts an additional turn. Cannot be used while active."
        , classes = [Mental, Single, Uncounterable, Unreflectable, Unremovable]
        , cost    = χ [Rand]
        , effects = [(Self, perI "jashin" 1 (applyDur [Endure]) 1 
                           • hide' "jashin" 0 []) 
                    ]
        , changes = costPer "jashin" [Rand]
        }
      ]
    , invuln "Block" "Hidan" [Physical]
    ] []
  , Character
    "Zetsu"
    "After Madara turned the Gedo statue's mutated victims into an army of servants, he chose one to lead them. Imbuing the White Zetsu entity with materialized will in the form of Black Zetsu, he created a hybrid being who became an official member of Akatsuki. White Zetsu and Black Zetsu have different approaches to combat, but both are able to take control of an enemy's abilities."
    [ [ newSkill
        { label   = "White Zetsu"
        , desc    = "Zetsu's white half takes over, canceling [Black Zetsu]. While active, Zetsu gains 5 permanent destructible defense each turn. Once used, this skill becomes [Black Zetu]."
        , classes = [Chakra]
        , channel = Ongoing 0
        , start   = [(Self, cancelChannel "Black Zetsu" • setFace 0
                          • vary "White Zetsu" "Black Zetsu"
                          • vary "Black Zetsu" "White Army"
                          • vary "Doppelgänger / Body Coating" "Doppelgänger")]
        , effects = [(Self, defend 0 5)]
        }
      , newSkill
        { label   = "Black Zetsu"
        , desc    = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains a random chakra every other turn. Once used, this skill becomes [White Zetsu]."
        , classes = [Chakra]
        , channel = Ongoing 0
        , start   = [(Self, cancelChannel "White Zetsu" • setFace 0
                          • vary "White Zetsu" ""
                          • vary "Black Zetsu" "Underground Roots"
                          • vary "Doppelgänger / Body Coating" "Body Coating")]
        , effects = [(Self, ifnotI "chakra" § gain [ Rand]
                          • ifI "chakra" § flag' "was chakra"
                          • remove "chakra"
                          • ifI "was chakra" § hide' "chakra" 1 [])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Black Zetsu"
        , desc    = "Zetsu's black half takes over, canceling [White Zetsu]. While active, Zetsu gains a random chakra every other turn. Once used, this skill becomes [Underground Roots][b][r]. As White Zetsu, this skill becomes [White Army][g]."
        , classes = [Chakra]
        , channel = Ongoing 0
        , start   = [(Self, cancelChannel "White Zetsu" • setFace 0
                          • vary "White Zetsu" ""
                          • vary "Black Zetsu" "Underground Roots"
                          • vary "Doppelgänger / Body Coating" "Body Coating")]
        , effects = [(Self, ifnotI "chakra" § gain [ Rand]
                           • ifI "chakra" § flag' "was chakra"
                           • remove "chakra"
                           • ifI "was chakra" § hide' "chakra" 1 [])
                    ]
        }
      , newSkill
        { label   = "Underground Roots"
        , desc    = "Tree roots emerge from the ground and wrap around an enemy, dealing 20 damage for 2 turns. While active, the target's damage is weakened by half. As White Zetsu, this skill becomes [White Army][g]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Blood, Rand]
        , cd      = 2
        , channel = Action 2
        , effects = [(Enemy, damage 20 • apply 1 [Scale All 0.5])]
        }
      , newSkill
        { label   = "White Army"
        , desc    = "Zetsu creates numerous clones of himself which deal 5 damage to all enemies for 5 turns. As Black Zetsu, this skill becomes [Underground Roots][b][r]."
        , classes = [Physical, Melee]
        , cost    = χ [Gen]
        , channel = Ongoing 5
        , effects = [(Enemies, damage 5)]
        }
      ]
    , [ newSkill
        { label   = "Doppelgänger / Body Coating"
        , desc    = "Zetsu seizes an enemy and makes use of their abilities. As White Zetsu, this skill deals 20 damage, steals a random chakra, stuns their non-mental skill for 1 turn, and replaces itself with the last skill they used for 1 turn. As Black Zetsu, this skill causes the target's next reflectable non-unique skill to target allies instead of enemies and enemies instead of allies."
        , require = Unusable
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        }
      , newSkill
        { label   = "Body Coating"
        , desc    = "Zetsu melts and flows over an enemy, taking control of their body. The next skill they use will target allies instead of enemies and enemies instead of allies. Does not stack. As White Zetsu, this skill becomes [Doppelgänger][t][r]."
        , classes = [Mental, Melee, Single, Invisible, Unreflectable]
        , cost    = χ [Blood, Gen]
        , cd      = 3
        , effects = [(Enemy, apply 0 [Swap All])]
        }
      , newSkill
        { label   = "Doppelgänger"
        , desc    = "Zetsu seizes an enemy and alters his chakra to match their own, dealing 20 damage, stealing a random chakra, and stunning their non-mental skills for 1 turn. The last skill they used replaces this skill for 1 turn. Zetsu's copy of their skill has no chakra cost and ends when this skill reverts. As Black Zetsu, this skill becomes [Body Coating][b][g]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , cd      = 1
        , effects = [ (Enemy, steal 1 • copyLast 1 2 
                            • apply 1 [Stun NonMental]
                            • damage 20)
                    ]
        }
      ]
    , invuln "Hide" "Zetsu" [Physical]
    ] []
  , Character
    "Hiruko Sasori"
    "The Akatsuki member Sasori is rarely seen outside of Hiruko, a heavily armored shell that allows him to perform puppetry without the usual weakness of leaving himself exposed. Its poisonous stinger and hidden traps make it a threat that cannot be ignored."
    [ [ newSkill
        { label   = "Scorpion Tail Constriction"
        , desc    = "Sasori shifts Hiruko into offensive mode and seizes an enemy, dealing 10 damage and stunning their non-mental skills for 1 turn. The target receives 10 additional damage from skills for the rest of the turn. Once used, this skill becomes [Scorpion Tail Strike][t][r]."
        , classes = [Bane, Physical, Ranged]
        , cost    = χ [Rand]
        , effects = [ (Enemy, damage 10
                            • apply (-1) [Stun NonMental, Bleed All 10])
                    , (Self,  vary "Scorpion Tail Constriction" 
                                   "Scorpion Tail Strike")
                    ]
        }
      , newSkill
        { label   = "Scorpion Tail Strike"
        , desc    = "Hiruko's poison-drenched tail stabs at an enemy, dealing 20 damage and 20 affliction damage. For 2 turns, the target's skills cost 2 additional random chakra."
        , classes = [Bane, Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [ (Enemy, damage 20 • afflict 20 
                            • apply 1 [Exhaust All ])]
        }
      ]
    , [ newSkill
        { label   = "Hidden Toxic Needles"
        , desc    = "Sasori shifts Hiruko into a defensive stance and takes aim at his enemies for 1 turn. If the enemy team uses any skills, they will receive 15 affliction damage for 2 turns. At the end of the turn, Sasori gains 20 permanent destructible defense."
        , classes = [Bane, Physical, Ranged, Single, InvisibleTraps]
        , cost    = χ [Rand, Rand]
        , cd      = 3
        , effects = [ (Self, delay 1 § defend 0 20) 
                    , (Self, tag (-1))
                    , (Enemies, delay (-1) 
                                § ifI "needles at the ready" 
                                  § apply 2 [Afflict 15]
                              • trap (-1) (OnAction All) . self 
                                § hide' "needles at the ready" 1 [])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Defensive Puppeteering"
        , desc    = "Hiruko swings his tail about to knock back enemies, gaining 10 destructible defense and restoring 10 health for 2 turns. While active, Sasori ignores effects that prevent him from reducing damage or becoming invulnerable."
        , classes = [Physical]
        , cost    = χ [Rand, Rand]
        , channel = Control 2
        , cd      = 3
        , effects = [ (Self, defend 1 10 • heal 10 
                           • apply 1 [Ignore $ const Expose])]
        }
      ]
    , invuln "Tail Block" "Sasori" [Physical]
    ] []
  , Character
    "Kazekage Puppeteer Sasori"
    "Sasori's most prized human puppet is the body of the Third Kazekage, which allows him to wield its magnetic abilities. As Sasori's last resort, this puppet favors all-out attack. Its enemies must deal with it quickly before its iron sand takes over the battlefield."
    [ [ newSkill
        { label   = "Kazekage Puppet Summoning" 
        , desc    = "Sasori summons his most prized puppet, gaining 15 destructible defense and enabling his other skills. Once used, this skill becomes [Iron Sand: World Order][b][n]. Each turn, Sasori gains a stack of Iron Sand."
        , classes = [Physical]
        , channel = Ongoing 0
        , start   = [(Self, defend 0 15)]
        , effects = [(Self, apply' "Iron Sand" 0 [])]
        }
      , newSkill
        { label   = "Iron Sand: World Order"
        , desc    = "Using the third Kazekage's magnetic abilities, Sasori shapes his Iron Sand into a massive tangle of branching iron spikes that looms overhead. As it comes crashing down on the battlefield, it deals 10 piercing damage to all enemies and 5 additional damage per stack of Iron Sand."
        , classes = [Physical]
        , cost    = χ [Blood, Nin]
        , cd      = 3
        , effects = [(Enemies, perI "Iron Sand" 5 pierce 10)]
        }
      ]
    , [ newSkill
        { label   = "Poison Blade Assault"
        , desc    = "Sasori directs the Kazekage puppet to single out an enemy and gains 20 destructible defense for 2 turns. While Sasori has destructible defense from this skill, he deals 10 damage and 10 affliction damage to the target."
        , require = HasI 1 "Iron Sand"
        , classes = [Bane, Physical, Melee]
        , cost    = χ [Rand, Rand]
        , channel = Action 2
        , cd      = 3
        , start   = [(Self, defend 2 20 • onBreak')]
        , effects = [(Enemy, damage 10 • afflict 10)]
        }
      ]
    , [ newSkill
        { label   = "Thousand Arms"
        , desc    = "Countless concealed arms lash out from Sasori's Kazekage puppet and flail wildly for 1 turn, pinning down anyone they catch. Enemies who do not use harmful skills next turn will be pinned for 1 turn, unable to reduce damage or become invulnerable. While active, this skill becomes [Poison Gas][r][r]."
        , require = HasI 1 "Iron Sand"
        , classes = [Physical, Melee, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 3
        , channel = Control 1
        , effects = [ (Enemies, trap (-1) OnHarm 
                              § apply' "Pinned" (-1) [Expose])
                    , (Self, vary "Thousand Arms" "Poison Gas")
                    ]
        }
      , newSkill
        { label   = "Poison Gas"
        , desc    = "Sasori emits a cloud of poisonous gas, dealing 15 affliction damage to all enemies. Next turn, enemy cooldowns are increased by 1 and enemy chakra costs are increased by 1 random. Lasts 2 turns on targets pinned by [Thousand Arms]."
        , classes = [Bane, Ranged]
        , cost    = χ [Rand, Rand]
        , effects = [ (Enemies, afflict 15
                              • ifnotU "Pinned" 
                                § apply 1 [Snare 1, Exhaust All]
                              • ifU    "Pinned" 
                                § apply 2 [Snare 1, Exhaust All])
                    ]
        }
      ]
    , invuln "Chakra Barrier" "Sasori" [Chakra]
    ] []
  , let unjet = cancelChannel "Flamethrower Jets" 
              • everyone § remove "Flame Blast" ° remove "Flamethrower Jets"
    in Character
    "True Form Sasori"
    "Having invented and perfected the art of human puppetry, Sasori accomplished its ultimate act: transforming himself into a living puppet. His immortal core now resides in an unnaturally youthful simulacrum filled to the brim with tools of slaughter, each of which he switches out for another as soon as he uses it."
    [ [ newSkill
        { label   = "Poisonous Chain Skewer"
        , desc    = "Sasori hooks an enemy with the poison-soaked steel ropes inside his body and pulls himself to them, dealing 5 affliction damage for 3 turns. Next turn, the target can only target Sasori or themselves. Once used, this skill becomes [Impale][t]."
        , classes = [Bane, Ranged, Unreflectable]
        , cost    = χ [Rand]
        , effects = [ (Enemy, apply 3 [Afflict 5] • apply 1 [Taunt]) 
                    , (Self, unjet • vary "Poisonous Chain Skewer" "Impale")
                    ]
        }
      , newSkill
        { label   = "Impale"
        , desc    = "Sasori stabs an enemy with a poison-soaked blade, dealing 15 piercing damage immediately and 5 affliction damage for 2 turns. If the target is affected by [Poisonous Chain Skewer], they become affected by [Complex Toxin], which stuns them after 2 turns. Once used, this skill becomes [Poisonous Chain Skewer]."
        , classes = [Bane, Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Self,  unjet • vary "Poisonous Chain Skewer" "")
                    , (Enemy, pierce 15 • apply 2 [Afflict 5] 
                            • ifU "Poisonous Chain Skewer" 
                              § bomb' "Complex Toxin" 2 [] 
                                [(Expire, apply 1 [Stun All])])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Flamethrower Jets"
        , desc    = "Using fuel stored in a sealing scroll, Sasori shoots flames at an enemy for 3 turns, dealing 10 affliction damage each turn. While active, Sasori ignores non-damage effects from the target other than chakra cost changes and is invulnerable to other enemies. If Sasori uses any skill, [Flamethrower Jets] is canceled. After use, this skill becomes [Cutting Water Jets][n]."
        , classes = [Ranged, Unreflectable]
        , cost    = χ [Nin, Rand]
        , channel = Action 3
        , cd      = 3
        , effects = [ (Enemy, afflict 10 • tag 1 
                            • self § apply' "Flame Blast" 1 [Duel])
                    , (Self,  apply 1 [Enrage] 
                            • vary "Flamethrower Jets" "Cutting Water Jets")
                    ]
        }
      , newSkill
        { label   = "Cutting Water Jets"
        , desc    = "Sasori shoots a high-pressure jet of water at an enemy, dealing 20 piercing damage. Deals 10 additional damage if the target is affected by [Flamethrower Jets]. Ends [Flamethrower Jets]. Once used, this skill becomes [Flamethrower Jets]."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin]
        , effects = [ (Enemy, withU "Flamethrower Jets" 10 pierce 20) 
                    , (Self,  unjet • vary "Flamethrower Jets" "")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Performance of a Hundred Puppets"
        , desc    = "Proving his reputation as the greatest puppeteer in history, Sasori takes control of 100 puppets, each acting as pure extensions of his will. Sasori gains 50 permanent destructible defense and grants 25 permanent destructible defense to his allies. As long as Sasori has destructible defense from this skill, this skill becomes [Barrage of a Hundred Puppets][r][r]."
        , classes = [Physical, Single]
        , cost    = χ [Tai, Rand, Rand]
        , cd      = 5
        , effects = [ (Self,    unjet 
                              • vary "Performance of a Hundred Puppets"
                                     "Barrage of a Hundred Puppets"
                              • defend 0 50 
                              • onBreak 
                                § vary "Performance of a Hundred Puppets" "")
                    , (XAllies, defend 0 25) 
                    ]
        }
      , newSkill
        { label   = "Barrage of a Hundred Puppets"
        , desc    = "Sasori commands his puppet army to attack an enemy, dealing 30 damage and applying [Complex Toxin] to the target, which stuns them after 2 turns."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand, Rand]
        , effects = [ (Enemy, damage 30 
                            • bomb' "Complex Toxin" 2 [] 
                                    [(Expire, apply 1 [Stun All])]) 
                    , (Self,  unjet)
                    ]
        }
      ]
    , invuln "Heart Switch" "Sasori" [Physical]
    ] []
  , Character
    "Animal Path Pain"
    "Having taken over the body of a ninja from the Hidden Rain Village named Ajisai, Pain now acts through it as one of his Six Paths. Animal Path's specialization is summoning giant creatures that continue to fight for her even if she is immobilized."
    [ [ newSkill
        { label   = "Summoning: Giant Centipede"
        , desc    = "Pain summons a huge centipede behind an enemy to ambush them. It deals 15 damage to them for 2 turns, and if the target does not use a new skill during that time, they will be stunned for 1 turn. Once used, this skill becomes [Summoning: Giant Crustacean][r][r]."
        , classes = [Physical, Melee, Summon]
        , cost    = χ [Nin]
        , cd      = 2
        , channel = Ongoing 2
        , start   = [ (Enemy, trap 2 (OnAction All) 
                              § remove "Summoming: Giant Centipede"
                            • bomb 2 []
                              [(Expire, apply' "Giant Centipede Stun" 1 
                                        [Stun All])]
                      )
                    , (Self,  vary "Summoning: Giant Centipede" 
                                   "Summoning: Giant Crustacean")
                    ]
        , effects = [(Enemy, afflict 15)]
        }
      , newSkill
        { label   = "Summoning: Giant Crustacean"
        , desc    = "Pain summons a huge foaming lobster that sprays spittle over the battlefield. For 2 turns, all enemies take 10 affliction damage and their cooldowns are increased by 1 turn. While active, the lobster provides 10 points of damage reduction to Pain and her team. Once used, this skill becomes [Summoning: Giant Centipede][n]."
        , classes = [Chakra, Ranged, Summon]
        , cost    = χ [Rand, Rand]
        , cd      = 2
        , channel = Ongoing 2
        , start   = [(Self, vary "Summoning: Giant Centipede" "")] 
        , effects = [(Enemies, afflict 10 • apply 1 [ Exhaust All])
                    , (Allies,  apply 1 [Reduce All 10])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Giant Panda"
        , desc    = "Pain summons a huge panda that defends herself or an ally, providing 20 permanent destructible defense and making them invulnerable for 2 turns."
        , classes = [Physical, Summon]
        , cost    = χ [Nin, Blood]
        , cd      = 4
        , channel = Ongoing 2
        , start   = [(Ally, defend 0 20 • apply 1 [Immune All])]
        }
      ]
    , [ newSkill 
        { label   = "Summoning: Giant Multi-Headed Dog"
        , desc    = "Pain summons a huge Cerberus hound that deals 10 piercing damage to all enemies for 2 turns. The first enemy to use a harmful skill on Pain or her allies will extend the effect of this skill on them by 2 turns. Cannot be used while active."
        , require = HasI (-1) "Summoning: Giant Multi-Headed Dog"
        , classes = [Physical, Melee, Summon, Bypassing, Unreflectable]
        , cost    = χ [Blood, Rand ]
        , channel = Ongoing 0
        , start   = [ (Enemies, tag 2)
                    , (Allies,  apply 0 [Parry Uncounterable 
                              $ ifnotU "already" 
                                § prolong 2 "Summoning: Giant Multi-Headed Dog"
                              • flag' "already"
                              • alliedTeam . delay (-1) 
                                § remove "Summoning: Giant Multi-Headed Dog"])
                    ]
        , effects = [ (Enemies, ifU "Summoning: Giant Multi-Headed Dog" 
                                § pierce 10
                                ° self (flag' "keep going"))
                    , (Self, ifnotI "keep going" 
                             § cancelChannel 
                               "Summoning: Giant Multi-Headed Dog")
                    ]
        }
      ]
    , invuln "Summoning: Giant Chameleon" "Pain" [Physical, Summon]
    ] []
  , Character
    "Preta Path Pain"
    "Having taken over the body of a farmer from the Hidden Grass Village, Pain now acts through it as one of his Six Paths. Preta Path's specialization is absorbing chakra and nullifying ninjutsu abilities."
    [ [ newSkill
        { label   = "Chakra Shield"
        , desc    = "Pain creates a protective barrier around himself and his allies which reflects the next skill used on each."
        , classes = [Chakra, Ranged, Invisible, Single, Unreflectable]
        , cost    = χ [Nin, Rand]
        , cd      = 4
        , effects = [(Allies, apply 0 [Reflect])]
        }
      ]
    , [ newSkill
        { label   = "Preta Drain"
        , desc    = "Pain absorbs an enemy's chakra, dealing 25 damage and stealing a random chakra."
        , classes = [Melee, Chakra]
        , cost    = χ [Tai, Rand]
        , cd      = 1
        , effects = [(Enemy, steal 1 • damage 25)]
        }
      ]
    , [ newSkill
        { label   = "Ninjutsu Absorption"
        , desc    = "Pain nullifies an enemy's chakra, preventing them from using skills that cost bloodline or ninjutsu chakra for 1 turn."
        , classes = [Chakra, Melee]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, apply 1 [Stun Bloodline, Stun Ninjutsu])]
        }
      ]
      , invuln "Rinnegan Foresight" "Pain" [Mental]
    ] []
  , Character
    "Naraka Path Pain"
    "Having taken over the body of a priest, Pain now acts through it as one of his Six Paths. Naraka Path's specialty is summoning and controlling the King of Hell, which shields and supports his allies by draining the strength of those it considers unworthy."
    [ [ newSkill
        { label   = "Summoning: King of Hell"
        , desc    = "Pain calls upon a timeless being tied to the power of the rinnegan which exists beyond life and death. The King of Hell provides 20 permanent destructible defense to Pain. While Pain has destructible defense from the King of Hell, this skill becomes [Energy Transfer][g]."
        , classes = [Mental, Summon]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Self, vary "Summoning: King of Hell" "Energy Transfer"
                          • defend 0 20 
                          • onBreak § vary "Summoning: King of Hell" "")]
        }
      , newSkill
        { label   = "Energy Transfer"
        , desc    = "Pain restores health to himself or an ally equal to his remaining defense from [Summoning: King of Hell]."
        , classes = [Mental]
        , cost    = χ [Gen]
        , cd      = 1
        , effects = [(Ally, perDef "Summoning: King of Hell" heal 1)]
        }
      ]
    , [ newSkill
        { label   = "Choke Hold"
        , desc    = "Pain seizes an enemy by the throat, dealing 20 damage to them and stunning their non-mental skills for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [(Enemy, damage 20 • apply 1 [Stun NonMental])]
        }
      ]
    , [ newSkill
        { label   = "Judgment"
        , desc    = "Judging an enemy unworthy, the King of Hell absorbs 20 of their health. If Pain has destructible defense from [Summoning: King of Hell], the absorbed health is added to its destructible defense. Absorbs 20 additional health if the target is affected by [Choke Hold]."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Rand]
        , cd      = 1
        , effects = [ (Enemy, ifnotU "Choke Hold" § leech 20 
                              § self . addDefense "Summoning: King of Hell"
                            • ifU "Choke Hold"    § leech 40 
                              § self . addDefense "Summoning: King of Hell") 
                    ]
        }
      ]
    , invuln "Block" "Pain" [Physical]
    ] []
  , Character
    "Human Path Pain"
    "Having taken over the body of a ninja from the Hidden Waterfall Village, Pain now acts through it as one of his Six Paths. Human Path's specialty is drawing the souls of his enemies from their bodies to reveal their secrets and drain their lifeforce."
    [ [ newSkill
        { label   = "Mind Invasion"
        , desc    = "Pain invades the mind of an enemy, dealing 15 damage. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , classes = [Mental, Melee, Unreflectable]
        , cost    = χ [Rand]
        , effects = [(Enemy, damage 15 • apply 1 [ Reveal])]
        }
      ]
    , [ newSkill
        { label   = "Spirit Absorption"
        , desc    = "Pain draws out the lifeforce of an enemy affected by [Mind Invasion], stealing 20 health and 1 chakra. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , require = HasU "Mind Invasion"
        , classes = [Mental, Melee, Unreflectable]
        , cost    = χ [Gen, Rand]
        , cd      = 2
        , effects = [(Enemy, steal 1 • apply 1 [ Reveal] 
                            • leech 20 § self . heal)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Soul Rip"
        , desc    = "Pain pulls out the soul of an enemy affected by [Mind Invasion], stealing 30 health. If their health reaches 30 or lower, they die; if not, he steals a random chakra from them and stuns them for 1 turn. Reveals invisible effects from the target and the target's cooldowns for 1 turn."
        , require = HasU "Mind Invasion"
        , classes = [Mental, Melee, Unreflectable]
        , cost    = χ [Gen, Tai]
        , cd      = 2
        , effects = [(Enemy, steal 1 • apply 1 [ Stun All, Reveal] 
                            • leech 30 § self . heal
                            • ifHealthU 0 30 kill)
                    ]
        }
      ]
    , invuln "Rinnegan Foresight" "Pain" [Mental]
    ] []
  , let missile kind = vary' 1 "Guided Missile" $ kind ++ " Missile"
    in Character
    "Asura Path Pain"
    "Having taken over the body of a wandering puppeteer, Pain now acts through it as one of his Six Paths. Asura Path's body is heavily augmented with ballistic and mechanical weaponry."
    [ [ newSkill 
        { label   = "Metal Blade"
        , desc    = "Pain attacks an enemy with a blade that unfolds from his body, dealing 15 piercing damage. The target takes 10 affliction damage each turn until one of their allies uses a skill on them."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , cd      = 2
        , effects = [(Enemy, pierce 15 • apply 0 [Afflict 10]
                           • trap 0 OnHelped § remove "Metal Blade")]
        }
      ] 
    , [ newSkill
        { label   = "Missile Salvo"
        , desc    = "Pain launches a cluster of missiles at an enemy, dealing 10 damage to them for 2 turns and removing the effects of helpful skills from them. Once used, this skill becomes [Head Cannon][r][r]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand, Rand]
        , cd      = 3
        , channel = Action 2
        , start   = [ (Enemy, purge) 
                    , (Self,  vary' 0 "Missile Salvo" "Head Cannon")
                    ]
        , effects = [(Enemy, damage 10)]
        }
      , newSkill
        { label   = "Head Cannon"
        , desc    = "Pain's head opens up to reveal a cannon, which explodes and deals 20 piercing damage to all enemies. Once used, this skill becomes [Missile Salvo][r][r]."
        , classes = [Physical, Ranged]
        , cost    = χ [Rand, Rand]
        , cd      = 1
        , effects = [ (Enemies, pierce 20) 
                    , (Self,    vary "Missile Salvo" "")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Guided Missile"
        , desc    = "Pain fires a slow-moving but devastating missile at a target. Over the next four turns, the cost of this skill is 1 chakra that cycles through the different types of chakra. Each turn, it has a different effect on the target. Using the skill again resets it."
        , classes = [Physical, Ranged, Bypassing, Invisible]
        , channel = Ongoing 4
        , start   = [ (Self, hide' "missile" 1 []) 
                    , (Enemy, tag 4)
                    ]
        , effects = [(Self, ifI "missile"   § flag' "was missile"
                                            ° missile "Bloodline"
                          • ifI "bloodline" § flag' "was bloodline" 
                                            ° missile "Genjutsu"
                          • ifI "genjutsu"  § flag' "was genjutsu" 
                                            ° missile "Ninjutsu"
                          • ifI "ninjutsu"  § flag' "was ninjutsu" 
                                            ° missile "Taijutsu"
                          • remove "missile"
                          • remove "Bloodline" • remove "Genjutsu"
                          • remove "Ninjutsu"  • remove "Taijutsu"
                          • ifI "was missile"   § hide' "bloodline" 1 []
                          • ifI "was bloodline" § hide' "genjutsu"  1 []
                          • ifI "was genjutsu"  § hide' "ninjutsu"  1 []
                          • ifI "was ninjutsu"  § hide' "taijutsu"  1 [])]
        }
      , newSkill
        { label   = "Bloodline Missile"
        , desc    = "Deals 25 damage to the target of [Guided Missile] and 25 damage to a random enemy."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood]
        , effects = [ (Enemies, ifU "Guided Missile" § damage 25) 
                    , (REnemy,  damage 25)
                    , (Self,    cancelChannel "Guided Missile" 
                              • everyone § remove "Guided Missile")
                    ]
        }
      , newSkill
        { label   = "Genjutsu Missile"
        , desc    = "Deals 20 damage to the target of [Guided Missile] and prevents them from reducing damage or becoming invulnerable for 2 turns."
        , classes = [Physical, Ranged]
        , cost    = χ [Gen]
        , effects = [ (Enemies, ifU "Guided Missile" 
                              § damage 25 ° apply 2 [Expose])
                    , (Self,    cancelChannel "Guided Missile"
                              • everyone § remove "Guided Missile")
                    ]
        }
      , newSkill
        { label   = "Ninjutsu Missile"
        , desc    = "Deals 25 damage to the target of [Guided Missile] and stuns them for 1 turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin]
        , effects = [ (Enemies, ifU "Guided Missile" 
                              § damage 25 ° apply 1 [Stun All])
                    , (Self,    cancelChannel "Guided Missile"
                              • everyone § remove "Guided Missile")
                    ]
        }
      , newSkill
        { label   = "Taijutsu Missile"
        , desc    = "Deals 30 piercing damage to the target of [Guided Missile]."
        , classes = [Physical, Ranged, Bypassing]
        , cost    = χ [Tai]
        , effects = [ (Enemies, ifU "Guided Missile" § pierce 30)
                    , (Self,    cancelChannel "Guided Missile"
                              • everyone § remove "Guided Missile")
                    ]
        }
      ]
    , invuln "Flee" "Pain" [Physical]
    ] []
  , Character
    "Deva Path Pain"
    "Having taken over the body of Yahiko, his deceased best friend, Pain now acts through it as one of his Six Paths. To honor Yahiko's memory, Pain uses the Deva Path as the leader of the Six Paths and his main body when interacting with others. Deva Path's specialization is gravity manipulation, which he uses to impair and imprison his enemies."
    [ [ newSkill
        { label   = "Almighty Push"
        , desc    = "Pain targets himself or an ally. The first harmful skill used on them next turn will be countered, and the person countered will receive 20 damage. Once used, this skill alternates between [Universal Pull] and [Almighty Push] each turn. "
        , classes = [Chakra, Ranged, Invisible, Unreflectable]
        , cost    = χ [Gen]
        , channel = Passive
        , start   = [ (Ally, apply 1 [Parry All $ damage 20]) 
                    , (Self, tag' "Tidal Force" 1)
                    ]
        , effects = [(Self, ifnotI "pull" 
                            § vary "Almighty Push" "Universal Pull"
                          • ifI    "pull" 
                            § vary "Almighty Push" "Almighty Push" 
                            ° flag' "pulled"
                          • remove "pull"
                          • ifnotI "pulled" § hide' "pull" 1 [])]
        }
      , newSkill
        { label   = "Almighty Push"
        , desc    = "Pain targets himself or an ally. The first harmful skill used on them next turn will be countered, and the person countered will receive 20 damage. This skill will become [Universal Pull] next turn."
        , classes = [Chakra, Ranged, Invisible, Unreflectable]
        , cost    = χ [Gen]
        , effects = [ (Ally, apply 1 [Parry All $ damage 20]) 
                    , (Self, tag' "Tidal Force" 1)
                    ]
        }
      , newSkill
        { label   = "Universal Pull"
        , desc    = "Pain manipulates gravity to pull an enemy toward him, ending their Action and Control skills in progress. Next turn, the target can only target Pain or themselves. If [Almighty Push] was used last turn, its effect is applied to Pain. This skill will become [Almighty Push] next turn."
        , classes = [Chakra, Ranged, Unreflectable]
        , cost    = χ [Gen]
        , effects = [ (Enemy, interrupt • apply 1 [Taunt])
                    , (Self,  ifI "Tidal Force"
                            § apply' "Almighty Push" 1 [Parry All $ damage 20])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Chakra Receiver"
        , desc    = "Pain pierces an enemy with a black rod that attunes them to his chakra, dealing 15 piercing damage and applying 15 permanent destructible barrier. Starting 1 turn from now, while the target has destructible barrier from this skill, they are stunned every other turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [(Enemy, pierce 15
                           • bar 0 identity' 
                             ( ifU "receive" § apply 1 [Stun All] ° flag' "recd" 
                             • remove "receive"
                             • ifnotU "recd" § hide' "receive" 1 []
                             ) 10)]
        }
      ]
    , [ newSkill
        { label   = "Planetary Devastation"
        , desc    = "Pain creates a gravitational anchor that pulls in an enemy and accumulates a rough sphere of rock and debris around them, applying 80 destructible barrier for 3 turns. While the target has destructible barrier from this skill, they are immune to effects from allies and invulnerable. At the end of the 3 turns, the target will receive damage equal to the remaining destructible barrier from this skill."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Gen, Tai]
        , cd      = 3
        , effects = [(Enemy, bar 3 damage (apply 1 [Immune All, Seal]) 80)]
        }
      ]
    , invuln "Rinnegan Foresight" "Pain" [Mental]
    ] []
  , Character
    "Nagato"
    "Nagato leads the Akatsuki as the six-bodied Pain. His true body has remained safely hidden for years, acting through the Gedo statue. Though vulnerable without his Paths to defend him, Nagato's rinnegan makes him a formidable opponent."
    [ [ newSkill
        { label   = "Summoning: Gedo Statue"
        , desc    = "Nagato summons the empty vessel of the ten-tailed beast, which provides 10 points of damage reduction to him for 3 turns. While active, Nagato can use his other skills and this skill becomes [Control][r]."
        , classes = [Mental, Summon, Unremovable]
        , cost    = χ [Blood]
        , cd      = 4
        , channel = Control (-4)
        , start   = [(Self, remove "gedo" • remove "dragon")]
        , effects = [(Self, vary' 1 "Summoning: Gedo Statue" "Control"
                          • vary' 1 "Phantom Dragon" "Phantom Dragon"
                          • perI "dragon" 1 (addStacks' 1 "Control") 0
                          • perI "gedo" 5 (applyX 1 $ Reduce All) 10)]
        , disrupt = [(Self, remove "Summoning: Gedo Statue" • remove "Control")]
        }
      , newSkill
        { label   = "Control"
        , desc    = "Nagato attempts to maintain control over the Gedo statue for a little longer, prolonging [Summoning: Gedo Statue] for 2 additional turns. Until it ends, [Summoning: Gedo Statue] provides 5 additional points of damage reduction up to a maximum of 25 and [Phantom Dragon] deals 5 additional damage. This skill has no chakra cost if [Phantom Dragon] was used last turn."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Self, prolongChannel 2 "Summoning: Gedo Statue"
                          • hide' "dragon" 0 []
                          • ifnotStacks "gedo" 3 § hide' "gedo" 0 [])]
        , changes = changeWith "Phantom Dragon" $ setCost []
        }
      ] 
    , [ newSkill
        { label   = "Phantom Dragon"
        , desc    = "Nagato summons a dragon to attack an enemy for 20 piercing damage. Costs 1 genjutsu chakra during [Summoning: Gedo Statue]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen, Rand]
        , effects = [ (Enemy, pierce 20) 
                    , (Self,  tag 1)
                    ]
        }
      , newSkill
        { label   = "Phantom Dragon"
        , desc    = "Nagato summons a dragon to attack an enemy for 20 piercing damage. During [Summoning: Gedo Statue], costs 1 genjutsu chakra and deals 5 additional damage per stack of [Control]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen]
        , effects = [ (Enemy, perI "dragon" 5 pierce 20) 
                    , (Self,  tag 1)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Rinne Rebirth"
        , desc    = "Nagato draws on the strength of the Outer Path to infuse life into himself and his allies. During each of the next 3 turns, Nagato restores 15 health to his team, resets their cooldowns, and gains a random chakra. Requires [Summoning: Gedo Statue]."
        , require = HasI 1 "Summoning: Gedo Statue"
        , classes = [Mental]
        , cost    = χ [Blood, Gen, Nin]
        , cd      = 6
        , channel = Control 3
        , effects = [ (Allies, heal 15 • resetAll) 
                    , (Self,   gain [Rand])
                    ]
        }
      ]
    , invuln "Rinnegan Foresight" "Nagato" [Mental]
    ] []
  , Character
    "Tobi"
    "A peculiar new member of the Akatsuki, Tobi claims to be Madara Uchiha even though Madara has been dead for many years. Using his Izanagi, he can rewind his state to an earlier point and even come back from the dead."
    [ [ newSkill
        { label   = "Sharingan"
        , desc    = "Tobi analyzes the battlefield to gain the upper hand. The next time a harmful skill is used on him, it will be countered and this skill will become [Kamui][g][r] for 2 turns. Cannot be used while active."
        , classes = [Mental, Invisible, Single]
        , cost    = χ [Blood]
        , cd      = 4
        , effects = [(Self, apply 0 [Parry All 
                            $ self § tag' "Kamui" 2 
                            • vary' (-2) "Sharingan" "Kamui"])]
        }
      , newSkill
        { label   = "Kamui"
        , desc    = "Tobi banishes a target to his pocket dimension for 3 turns, preventing them from affecting or being affected by anyone else. If used on an ally, cures all harmful effects on them. If used on an enemy, deals 20 piercing damage and prevents them from reducing damage or becoming invulnerable. Ends if Tobi uses [Kamui Strike] on someone else."
        , classes = [Chakra, Ranged, Single, Unreflectable]
        , cost    = χ [Gen, Rand]
        , cd      = 1
        , effects = [ (XAlly, cureAll • apply 3 [Duel, Isolate]) 
                    , (Enemy, pierce 20 • apply 3 [Duel, Isolate, Expose])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Kamui Strike"
        , desc    = "Tobi teleports behind an enemy and deals 20 piercing damage to them. Deals 20 additional damage if the target is affected by [Kamui]."
        , classes = [Chakra, Melee]
        , cost    = χ [Gen]
        , effects = [(Enemy, ifnotU "Kamui" . everyone § remove "Kamui"
                           • withU "Kamui" 20 pierce 20)]
        }
      ]
    , [ newSkill
        { label   = "Izanagi"
        , desc    = "Tobi sacrifices one of his eyes to take control of reality on a local scale. 2 turns from now, he will be restored to his current state."
        , classes = [Mental, Invisible]
        , cost    = χ [Blood, Blood]
        , charges = 2
        , effects = [(Self, snapshot 2)]
        }
      ]
    , invuln "Phase" "Tobi" [Chakra]
    ] []
  ]
