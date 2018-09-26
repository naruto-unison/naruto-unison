{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden.Adults (adultCsS) where

import StandardLibrary
import Game.Functions
import Game.Game
import Game.Structure

adultCsS :: [Character]
adultCsS =
  [ Character
    "Kakashi Hatake"
    "For most of his life, Kakashi has avoided using Kamui—his Sharingan's ultimate ability—unless absolutely necessary, as the effort of using it was almost too much for his body and mind to handle. Those days are over. With years of practice and refinement behind him, Kakashi can now rely on Kamui's dimensional warping to torture his enemies and make his allies intangible."
    [ [ newSkill
        { label   = "Lightning Beast Fang"
        , desc    = "Kakashi creates a lightning hound out of his Lightning Blade, which deals 25 piercing damage to an enemy. If the target is damaged, they will be stunned for 1 turn. During the next turn, this skill becomes [Lightning Blade Finisher][n][r]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, trap' 1 (OnDamaged All) § apply 1 [Stun All]
                            • pierce 25)
                    , (Self,  vary' 1 "Lightning Beast Fang" 
                                      "Lightning Blade Finisher")
                    ]
        }
      , newSkill
        { label   = "Lightning Blade Finisher"
        , desc    = "Deals 35 piercing damage to an enemy. Deals 15 additional damage if the target is affected by [Lightning Beast Fang]."
        , classes = [Chakra, Melee]
        , cost    = χ [Nin, Rand]
        , effects = [(Enemy, withU "Lightning Beast Fang" 15 pierce 35)]
        }
      ]
    , [ newSkill
        { label   = "Kamui"
        , desc    = "If used on an enemy, deals 45 piercing damage to them, increases their cooldowns by 1 turn, and increases the costs of their skills by 1 random chakra. If used on an ally, cures them of enemy effects and makes them invulnerable for 1 turn."
        , classes = [Chakra, Ranged, Bypassing]
        , cost    = χ [Blood, Gen]
        , effects = [ (Enemy, pierce 45) 
                    , (XAlly, cureAll • apply 1 [Immune All])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Team Tactics"
        , desc    = "For 3 turns, the cooldowns of Kakashi's allies are decreased by 1. While active, the first enemy skill used will replace this skill for 1 turn. Kakashi's copy of the skill has no chakra cost and ends when this skill reverts."
        , classes = [Mental, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 4
        , effects = [ (XAllies, apply 3 [Snare (-1)]) 
                    , (Enemies, apply' "Team Tactics " 3 [Copy 1 All 2 True])
                    ]
        }
      ]
    , invuln "Shadow Clone" "Kakashi" [Chakra]
    ] []
  , Character
    "Asuma Sarutobi"
    "Having somehow managed to avoid lung cancer, Asuma remains the leader of Team 10. Using techniques he learned from the Fire Temple, he hinders his opponents and instantly executes weak enemies."
    [ [ newSkill
        { label   = "Thousand Hand Strike"
        , desc    = "Asuma summons Kannon, the Fire Temple's patron spirit, which provides him with 40 permanent destructible defense and deals 25 damage to an enemy. Next turn, this skill becomes [Kannon Strike][r]. When [Kannon Strike] ends, this skill is disabled for 1 turn."
        , require = HasI (-1) "Overheating"
        , classes = [Physical, Melee, Summon]
        , cost    = χ [Blood, Rand]
        , effects = [ (Enemy, damage 25)
                    , (Self,  defend 0 40 
                            • vary' 1 "Thousand Hand Strike" "Kannon Strike") 
                    ]
        }
      , newSkill
        { label   = "Kannon Strike"
        , desc    = "Deals 20 damage to an enemy. This skill remains [Kannon Strike] for another turn."
        , classes = [Physical, Melee, Nonstacking]
        , cost    = χ [Rand]
        , effects = [ (Enemy, damage 20)
                    , (Self, tag' "Overheating" 2 
                           • vary' 1 "Thousand Hand Strike" "Kannon Strike")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Burning Ash"
        , desc    = "Asuma continually exhales a cloud of combustible ash upon his enemies, increasing the cooldowns of their skills by 1 turn. While active, this skill becomes [Burning Ash: Ignite][b]."
        , classes = [Bane, Ranged, Unreflectable]
        , cost    = χ [Gen, Rand]
        , channel = Action 0
        , start   = [(Self, vary "Burning Ash" "Burning Ash: Ignite")] 
        , effects = [(Enemies, apply 0 [Snare 1])]
        }
      , newSkill
        { label   = "Burning Ash: Ignite"
        , desc    = "Asuma strikes a piece of flint between his teeth, producing a spark that sets fire to his piles of ash and burns them away. The fire deals 10 affliction damage to each enemy per stack of [Burning Ash] on them."
        , classes = [Ranged, Bypassing, Uncounterable, Unreflectable]
        , cost    = χ [Blood]
        , effects = [ (Enemies,  perU "Burning Ash" 10 afflict 0) 
                    , (Self,     cancelChannel "Burning Ash" 
                               • everyone § remove "Burning Ash")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Decapitate"
        , desc    = "Bypassing invulnerability, Asuma mercilessly slaughters an enemy whose health is at or below 25."
        , classes = [Physical, Melee, Bypassing, Uncounterable, Unreflectable]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, ifHealthU 0 25 kill)]
        }
      ]
    , invuln "Dodge" "Asuma" [Physical]
    ] []
  {-
  , Character
    "Zaji"
    "A chūnin from the Hidden Leaf Village, Zaji loves to boast about his strength and combat prowess. He doesn't actually have either, but he's a decent sensor. By warning his team of incoming attacks, he can protect them from both light and heavy damage."
    [ [ newSkill
        { label   = "Chakra Sense"
        , desc    = "Zaji extends his senses over the battlefield and detects incoming attacks. For 2 turns, attacks that deal 25 baseline damage or lower will not injure Zaji or his allies."
        , classes = [Mental]
        , cost    = χ [Rand]
        , cd      = 2
        , effects = [(Allies, apply 2 [Threshold 25])]
        }
      ]
    ] []
  -}
{-
  , Character
    "Might Guy"
    "Over the past few years, Guy has learned restraint. By gradually opening his Gates in sequence, he avoids the risk of burning out before the battle is won."
    [ [ newSkill
        { label   = "Nunchaku"
        , desc    = "Using his signature Twin Fangs weapons, Guy deals 10 damage to an enemy for 3 turns. While active, if an enemy uses a harmful physical skill on him, he will deal 10 damage to them. Deals 5 additional damage on the first turn per stack of [Single Gate Release]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , channel = Action 3
        , start   = [ (Enemy, perI "Single Gate Release" 5 damage 10) ]
        , effects = [ (Self,  trapFrom 1 (OnHarmed Physical) $ damage 10)
                    , (Enemy, ifnotI "first" § damage 10)
                    ]
        }
      ] 
    , [ newSkill
        { label   = "Fiery Kick"
        , desc    = "Guy slams his leg into an enemy, dealing 35 damage and weakening their damage by 20 for 1 turn. Deals 5 additional damage per stack of [Single Gate Release]."
        , classes = [Physical, Melee]
        , cost    = χ [Blood, Tai]
        , effects = [(Enemy, perI "Single Gate Release" 5 damage 35 
                           • apply 1 [Weaken All 20])]
        }
      ]
    , [ newSkill
        { label   = "Single Gate Release"
        , desc    = "Guy opens one of his internal Gates, losing 5 health and gaining 5 points of permanent damage reduction. "
        , classes = [Mental, Unremovable]
        , effects = [(Self, sacrifice 0 5 • apply 0 [Reduce All 5])]
        }
      ]
    , invuln "Block" "Guy" [Physical]
    ] []-}
  , Character
    "Tsunade"
    "Tsunade has become the fifth Hokage. Knowing the Hidden Leaf Village's fate depends on her, she holds nothing back. Even if one of her allies is on the verge of dying, she can keep them alive long enough for her healing to get them back on their feet."
    [ [ newSkill
        { label   = "Heaven Spear Kick"
        , desc    = "Tsunade spears an enemy with her foot, dealing 20 piercing damage to them. If an ally is affected by [Healing Wave], their health cannot drop below 1 next turn. Spends a Seal if available to deal 20 additional damage and demolish the target's destructible defense and Tsunade's destructible barrier."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy, ifI "Strength of One Hundred Seal" demolish
                            • withI "Strength of One Hundred Seal" 20 pierce 20)
                    , (Allies, ifU "Healing Wave" § apply 1 [Endure])
                    , (Self,   remove "Strength of One Hundred Seal"
                             • vary "Strength of One Hundred Seal" "")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Healing Wave"
        , desc    = "Tsunade pours chakra into an ally, restoring 30 health to them immediately and 10 health each turn for 2 turns. Spends a Seal if available to restore 10 additional health immediately and last 3 turns."
        , classes = [Chakra, Unremovable]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [ (XAlly, withI "Strength of One Hundred Seal" 10 heal 20
                            • ifnotI "Strength of One Hundred Seal" 
                              § apply (-2) [Heal 10]
                            • ifI "Strength of One Hundred Seal"
                              § apply (-3) [Heal 10])
                    , (Self,  remove "Strength of One Hundred Seal"
                            • vary "Strength of One Hundred Seal" "")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Strength of One Hundred Seal"
        , desc    = "Tsunade activates her chakra-storing Seal, restoring 25 health and empowering her next skill. Spends a Seal if available to instead restore 50 health to Tsunade and grant 2 random chakra."
        , classes = [Chakra]
        , cost    = χ [Rand]
        , cd      = 3
        , effects = [(Self, heal 25 • tag 0 
                          • vary "Strength of One Hundred Seal" 
                                 "Strength of One Hundred Seal")]
        }
      , newSkill
        { label   = "Strength of One Hundred Seal"
        , desc    = "Tsunade activates her chakra-storing Seal, restoring 25 health and empowering her next skill. Spends a Seal if available to instead restore 50 health to Tsunade and grant 2 random chakra."
        , classes = [Chakra]
        , cost    = χ [Rand]
        , cd      = 3
        , effects = [(Self, heal 50 • gain [Rand, Rand] 
                          • vary "Strength of One Hundred Seal" ""
                          • remove "Strength of One Hundred Seal")]
        }
      ]
    , invuln "Block" "Tsunade" [Physical]
    ] []
  , Character
    "Ōnoki"
    "The third Tsuchikage of the Hidden Rock Village, Onoki is the oldest and most stubborn Kage. His remarkable ability to control matter on an atomic scale rapidly grows in strength until it can wipe out a foe in a single attack."
    [ [ newSkill
        { label   = "Earth Golem"
        , desc    = "Ōnoki pulls up a golem of rock from the ground, which provides 10 permanent destructible defense to his team and deals 10 damage to all enemies."
        , classes = [Chakra, Physical, Melee]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [ (Allies, defend 0 10) 
                    , (Enemies, damage 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Lightened Boulder"
        , desc    = "Ōnoki negates the gravity of an ally, providing 10 points of damage reduction to them for 2 turns. While active, the target cannot be countered or reflected."
        , classes = [Physical, Melee]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(XAlly, apply 2 [Reduce All 10, AntiCounter])]
        }
      ]
    , [ newSkill
        { label   = "Atomic Dismantling"
        , desc    = "Ōnoki shatters the atomic bonds within an enemy, dealing 20 piercing damage and permanently increasing the damage of this skill by 10."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin]
        , effects = [ (Enemy, perI "Atomic Dismantling" 10 pierce 20) 
                    , (Self,  addStack)
                    ]
        }
      ]
    , invuln "Flight" "Ōnoki" [Chakra]
    ] []
  , Character
    "Mei Terumi"
    "The third Mizukage of the Hidden Mist Village, Mei works tirelessly to help her village overcome its dark history and become a place of kindness and prosperity. Her corrosive attacks eat away at the defenses of her "
    [ [ newSkill
        { label   = "Solid Fog"
        , desc    = "Mei exhales a cloud of acid mist, dealing 15 affliction damage to an enemy for 3 turns."
        , classes = [Chakra, Ranged]
        , cost    = χ [Blood]
        , cd      = 3
        , effects = [(Enemy, afflict 15)]
        }
      ]
    , [ newSkill
        { label   = "Water Bomb"
        , desc    = "Mei floods the battlefield, dealing 20 piercing damage to all enemies and preventing them from reducing damage or becoming invulnerable for 1 turn."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Rand]
        , cd      = 1
        , effects = [(Enemies, pierce 20 • apply 1 [Expose])]
        }
      ]
    , [ newSkill
        { label   = "Lava Monster"
        , desc    = "Mei spits a stream of hot lava, dealing 10 affliction damage to all enemies and removing 20 destructible defense from them for 3 turns."
        , classes = [Chakra, Ranged]
        , cost    = χ [Blood, Rand]
        , cd      = 3
        , channel = Action 3
        , effects = [(Enemies, demolish' 20 • afflict 10)]
        }
      ]
    , invuln "Flee" "Mei" [Physical]
    ] []
  ]
