{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden.Organizations (organizationCsS) where

import StandardLibrary
import Game.Functions
import Game.Game
import Game.Structure

organizationCsS :: [Character]
organizationCsS = 
  [ Character
    "Aoba Yamashiro"
    "A jōnin from the Hidden Leaf Village assigned to hunt down Akatsuki members, Aoba maintains a reserved and impassive demeanor in public that quickly disappears among friends. He uses his significant genjutsu prowess to summon numerous crows and spread his consciousness among them, controlling all of their actions simultaneously."
    [ [ newSkill
        { label   = "Scattering Crow Swarm"
        , desc    = "Aoba summons a flock of self-duplicating crows that swarm the enemy team for 4 turns, dealing 5 damage each turn and providing 5 points of damage reduction to Aoba and his allies."
        , classes = [Mental, Ranged, Summon]
        , cost    = χ [Gen]
        , channel = Ongoing 4
        , start   = [(Self, enemyTeam § apply (-4) []
                          • alliedTeam § apply (-4) [Reduce All 5])]
        , effects = [(Enemies, damage 5)]
        }
      ]
    , [ newSkill
        { label   = "Revenge of the Murder"
        , desc    = "If the target ally's health reaches 0 within 3 turns, their health will be set to 5, their skills will all be replaced by [Converging Murder], and they will become completely immune to all skills. At the end of their next turn, they will die."
        , classes = [Mental, Ranged, Invisible, Uncounterable, Unreflectable, Unremovable]
        , cost    = χ [Rand]
        , effects = [(XAlly, trap 3 OnRes 
                           $ resetAll • setHealth 5 • teach 1 Deep 2 • setFace 1
                           • bomb (-1) [Immune All, Seal, Enrage, Ignore Stun] 
                             [(Done, kill')])]
        }
      ]
    , [ newSkill
        { label   = "Converging Murder"
        , desc    = "Aoba directs all of his crows at an enemy, dealing 45 damage to them. Deals 5 additional damage for each stack of [Scattering Crow Swarm] on the target."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Gen]
        , cd      = 1
        , effects = [(Enemy, perU "Scattering Crow Swarm" 5 damage 45)]
        } 
      ]
    , invuln "Crow Barrier" "Aoba" [Chakra]
    ] []
  , Character
    "Ibiki Morino"
    "A sadistic jōnin who specializes in extracting information, Ibiki commands the Hidden Leaf Village's Torture and Interrogation Force. Pain is his preferred method of communication, and his preferred approach to battle is ensuring all options available to his enemies will lead to their defeat."
    [ [ newSkill
        { label   = "Biding His Time"
        , desc    = "Provides 10 points of permanent damage reduction to Ibiki. Each time a damaging skill is used on Ibiki, he will gain a stack of [Payback]. Once used, this skill becomes [Payback][r]."
        , classes = [Mental, Melee]
        , cost    = χ [Rand]
        , effects = [ (Self, apply 0 [Reduce All 10] 
                           • vary "Biding His Time" "Payback"
                           • trap 0 (OnDamaged All) § addStacks "Payback" 1) ]
        }
     , newSkill
        { label   = "Payback"
        , desc    = "Deals 15 damage to an enemy. Spends all stacks of [Payback] to deal 5 additional damage per stack."
        , classes = [Mental, Melee]
        , cost    = χ [Rand]
        , effects = [ (Enemy, perI "Payback" 5 damage 15) 
                    , (Self, remove "Payback" • vary "Biding His Time" "")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Iron Maiden"
        , desc    = "Ibiki traps an enemy in a spike-filled iron coffin shaped like a cat. For 3 turns, each time the target uses a harmfull skill, they will receive 25 piercing damage. Ibiki gains 30 permanent destructible defense."
        , classes = [Physical, Melee, Summon]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, trap 3 OnHarm § pierce 25) 
                    , (Self,  defend 0 30)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Torture Chamber"
        , desc    = "Ibiki traps an enemy in a cage of chains and gears. For 3 turns, each time the target does not use a skill, they will receive 25 piercing damage. Ibiki gains 30 permanent destructible defense."
        , classes = [Physical, Melee, Summon]
        , cost    = χ [Nin, Rand]
        , effects = [ (Enemy, trap 3 OnNoAction § pierce 25) 
                    , (Self,  defend 0 30)
                    ]
        }
      ]
    , invuln "Dodge" "Ibiki" [Physical]
    ] []
  , Character
    "Sai"
    "An operative of the Hidden Leaf Village's elite Root division, Sai is quietly expressive and artistic. He uses a set of brushes with chakra-infused ink to give life to his illustrations, which usually take the form of powerful black-and-white beasts." 
    [ [ newSkill
        { label   = "Super Beast Scroll: Lions"
        , desc    = "Sai draws a pack of lions that attack an enemy, dealing 30 damage to them and providing 20 destructible defense to Sai for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Gen, Rand]
        , effects = [ (Enemy, damage 30) 
                    , (Self,  defend 1 20)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Super Beast Scroll: Snake"
        , desc    = "Sai draws a snake that paralyzes an enemy with its bite, stunning their physical and chakra skills for 1 turn and preventing them from reducing damage or becoming invulnerable. During [Ink Mist], this skill becomes [Super Beast Scroll: Bird][g]."
        , classes = [Physical, Melee, Bypassing]
        , cost    = χ [Gen]
        , effects = [(Enemy, apply 1 [Stun Physical, Stun Chakra, Expose])]
        }
      , newSkill
        { label   = "Super Beast Scroll: Bird"
        , desc    = "Sai draws a bird in the air, which deals 25 damage to an enemy and stuns them for 1 turn."
        , classes = [Physical, Melee, Bypassing]
        , cost    = χ [Gen]
        , effects = [(Enemy, damage 25 • apply 1 [Stun All])]
        }
      ]
    , [ newSkill
        { label   = "Ink Mist"
        , desc    = "Streams of ink coil in the air around Sai and his allies for 3 turns, obscuring them from enemies and allowing Sai to draw three-dimensionally. If someone on Sai's team is stunned, they become invulnerable for 1 turn. If their chakra is removed or stolen, they gain a random chakra. If an ally receives new non-affliction damage, Sai's damage increases by 10 for 1 turn."
        , classes = [Mental, Multi, Bypassing]
        , cost    = χ [Rand, Rand]
        , effects = [ (Enemies, trap 3 OnChakra . self § gain [Rand])
                    , (Allies, trap 3 (OnStunned Multi) § apply 1 [Immune All] 
                             • trap 3 (OnDamaged NonAffliction) . self 
                               § apply 1 [Strengthen All 10])
                    , (Self,   vary' 3 "Super Beast Scroll: Snake" 
                                       "Super Beast Scroll: Bird")
                    ]
        }
      ]
    , invuln "Ink Clone" "Sai" [Chakra]
    ] []
    -- TODO "Ignore all effects that improve their skills"
--  , Character
--    "Yamato"
--    "An operative of the Hidden Leaf Village's elite Root division, Yamato carries the first Hokage's wood-manipulation abilities due to having been injected with his cells. 
-- TODO Izumo and Kotetsu swap health
  , Character
      "Fū Yamanaka"
      "An operative of the Hidden Leaf Village's elite Root division, Fū is emotionless and ruthlessly straightforward. His only drive is unswerving loyalty to Danzō. His combination of long-distance Yamanaka genjutsu and his personal form of taijutsu makes him a formidable threat in any situation, but his trump card is the ability to swap his consciousness into the body of an opponent and make use of all their skills."
      [ [ newSkill
          { label   = "Tantō Slash"
          , desc    = "Fū slashes an enemy with his tantō, dealing 25 damage. Deals 15 additional damage if the target is affected by [Mind Transfer]."
          , classes = [Physical, Melee]
          , cost    = χ [Tai]
          , effects = [(Enemy, withU "Mind Transfer" 15 damage 25)]
          }
        ]
      , [ newSkill
          { label   = "Mind Transfer"
          , desc    = "Fū takes over an enemy's mind and steals all removable beneficial effects on them. The target's destructible defense is transferred to Fū, and Fū's destructible barrier is transferred to the target. While active, Fū detects all invisible effects and enemy cooldowns."
          , classes = [Chakra]
          , cost    = χ [Gen]
          , cd      = 3
          , channel = Control 3
          , start   = [(Enemy, commandeer)]
          , effects = [(Enemy, tag 1 • enemyTeam § hide' "revealed" 1 [Reveal])]
          }
        ]
      , [ newSkill
          { label   = "Mind Transfer Puppet Curse"
          , desc    = "Fū defends himself or an ally with a puppet trap. For 2 turns, the first enemy that uses a new harmful physical or chakra skill on the target will be countered. If an enemy is countered, Fū's skills are replaced by their skills for 4 turns and their skills are replaced by [Puppet Curse: Attack] for 4 turns. Effects from Fū's usage of their skills are canceled when Fū's skills revert."
          , classes = [Mental, InvisibleTraps, Unreflectable]
          , cost    = χ [Gen]
          , cd      = 3
          , effects = let f = copyAll 4 • tag (-4) 
                            • teach 4 Shallow 4 • teachOne 4 3 Deep 5
                            • self § resetAll ° bomb (-4) [] [(Done, resetAll)]
                      in [(Ally, applyWith [Invisible] 2 
                                 [Parry Physical f, Parry Chakra f])]
          }
        ]
      , invuln "Dodge" "Fū" [Physical]
      , [ newSkill
          { label   = "Puppet Curse: Attack"
          , desc    = "Trapped in a puppet, little can be done but flail about and hope to hit someone! Deals 15 damage to an enemy."
          , classes = [Physical, Melee]
          , cost    = χ [Rand]
          , effects = [(Enemy, damage 15)]
          }
        ]
      , [ newSkill
          { label   = "Puppet Curse: Defend"
          , desc    = "Trapped in a puppet, little can be done but flail about and hope to block an attack! The puppet becomes invulnerable for 1 turn."
          , classes = [Physical, Melee]
          , cost    = χ [Rand]
          , cd      = 4
          , effects = [(Self, apply 1 [Immune All])]
          }
        ]
      ] []
  , Character
    "Danzō Shimura"
    "The founder and leader of the Hidden Leaf Village's elite Root division, Danzō has had a hand in almost every important global event since he came to power. His numerous implanted Sharingans allow him to repeatedly cheat death."
    [ [ newSkill
        { label   = "Izanagi"
        , desc    = "Danzō gains 10 Sharingan and loses 1 each turn. If his health reaches 0, he regains 10 health per Sharingan and loses all Sharingan. When he has no Sharingan remaining, this skill becomes [Reverse Tetragram Sealing][r][r][r]."
        , classes = [Mental]
        , cost    = χ [Blood]
        , channel = Ongoing 0
        , start   = [(Self, addStacks "Sharingan" 10 
                          • vary "Izanagi" "Izanagi")]
        , effects = [(Self, ifnotI "paused" 
                            $ removeStack "Sharingan"
                            • ifnotI "Sharingan"
                              § cancelChannel "Izanagi"
                              ° vary "Izanagi" "Reverse Tetragram Sealing"
                              ° trap' 1 OnRes 
                                ( cancelChannel "Izanagi"
                                • vary "Izanagi" "Reverse Tetragram Sealing"
                                • perI "Sharingan" 10 setHealth 0
                                • remove "Sharingan"
                                ))]
        }
      , newSkill
        { label   = "Izanagi"
        , desc    = "Pauses the effect of [Izanagi] for 1 turn."
        , classes = [Mental]
        , cost    = χ [Blood]
        , effects = [(Self, flag' "paused")]
        }
      , newSkill
        { label   = "Reverse Tetragram Sealing"
        , desc    = "Kills Danzō and make all enemies immune to effects from each other for 3 turns. In 3 turns, enemies who are not invulnerable are killed."
        , classes = [Mental, Bypassing, Unremovable]
        , cost    = χ [Rand, Rand, Rand]
        , effects = [ (Enemies, delay (-3) kill)
                    , (Self,    enemyTeam § apply (-3) [Seal] • kill)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Vacuum Wave"
        , desc    = "Danzō exhales slicing blades of air at an enemy, dealing 20 piercing damage. Deals 15 additional damage if the target is affected by [Kotoamatsukami]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [(Enemy, withU "Kotoamatsukami" 15 pierce 20)]
        }
      ]
    , [ newSkill
        { label   = "Kotoamatsukami"
        , desc    = "Danzō infiltrates the mind of an enemy. The next time they use a skill, its effects will be nullified, they will be stunned for 2 turns, and this skill will be replaced by the skill they used for 2 turns. Danzō's copy of their skill has no chakra cost or cooldown and ends when this skill reverts."
        , classes = [ Mental, Ranged, Invisible, Uncounterable, Unreflectable]
        , cost    = χ [Blood, Gen, Gen]
        , cd      = 9
        , effects = [(Enemy, apply 2 [Copy 2 All 2 True]
                           • trap 2 (OnCounter Uncounterable) 
                             § remove "Kotoamatsukami"
                             ° apply' "Kotoamatsukami Stun" 2 [Stun All])]
        }
      ]
    , invuln "Summoning: Baku" "Danzō" [Chakra, Summon]
    ] []
  ]
