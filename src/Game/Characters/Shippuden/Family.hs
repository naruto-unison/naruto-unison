{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden.Family (familyCsS) where

import StandardLibrary

import Game.Functions
import Game.Game
import Game.Structure

familyCsS :: [Character]
familyCsS =
  [ Character
    "Chiyo"
    "A widely-respected puppeteer and former leader of the Hidden Sand Village's Puppet Brigade, Elder Chiyo has a lifetime of combat experience. Her numerous puppets sow chaos among her enemies and shield her from harm, allowing her to use her other skills with impunity. If one of her allies is close to death, she can sacrifice her own life to restore theirs."
    [ [ newSkill
        { label   = "Assault Blade"
        , desc    = "Chiyo lashes out with hovering kunai, dealing 15 piercing damage to a targeted enemy and a random enemy. During [Ten Puppets Collection], this skill becomes [Three Treasure Suction Crush][r][r]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [ (Enemy,  pierce 15) 
                    , (REnemy, pierce 15)
                    ]
        }
      , newSkill
        { label   = "Three Treasure Suction Crush"
        , desc    = "Chiyo deploys three puppets in a triangle formation. The puppets create a vacuum hurricane which sucks in an enemy, dealing 30 damage to them and stunning their non-mental skills for 1 turn. Deals affliction damage if the target is affected by [Lion Sealing]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand, Rand]
        , cd      = 1
        , effects = [(Enemy, apply 1 [Stun NonMental]
                           • ifU "Lion Roar Sealing"    § afflict 30
                           • ifnotU "Lion Roar Sealing" § damage 30)]
        }
      ]
    , [ newSkill
        { label   = "Ten Puppets Collection"
        , desc    = "Chiyo commands a brigade of puppets, gaining 50 permanent destructible defense. Each turn that Chiyo has destructible defense from this skill, her puppets deal 10 damage to a random enemy. While active, this skill becomes [Lion Roar Sealing][b]."
        , classes = [Chakra, Ranged, Single]
        , cost    = χ [Rand, Rand]
        , cd      = 5
        , channel = Action 0
        , start   = [(Self, defend 0 50 
                          • vary "Assault Blade" "Three Treasure Suction Crush"
                          • vary "Ten Puppets Collection" "Lion Roar Sealing"
                          • onBreak § cancelChannel "Ten Puppets Collection")]
        , effects = [(REnemy, damage 10)]
        }
      , newSkill
        { label   = "Lion Roar Sealing"
        , desc    = "Chiyo uses an advanced sealing technique on an enemy, making them immune to effects from allies and preventing them from reducing damage or becoming invulnerable for 2 turns."
        , classes = [Chakra, Ranged]
        , cost    = χ [Blood]
        , cd      = 3
        , effects = [(Enemy, apply 2 [Expose, Seal])]
        }
      ]
    , [ newSkill
        { label   = "Self-Sacrifice Reanimation"
        , desc    = "Chiyo prepares to use her forbidden healing technique on an ally. The next time their health reaches 0, their health is fully restored, they are cured of harmful effects, and Chiyo's health is reduced to 1."
        , classes = [Chakra, Invisible]
        , cost    = χ [Blood, Nin]
        , charges = 1
        , effects = [(XAlly, trap 0 OnRes 
                             § cureAll ° setHealth 100 ° self (setHealth 1))]
        }
      ]
    , invuln "Chakra Barrier" "Chiyo" [Chakra]
    ] []
  , Character
    "Inoichi Yamanaka"
    "A jōnin from the Hidden Leaf Village and Ino's father, Inoichi can solve practically any dilemma with his analytical perspective. Under his watchful gaze, every move made by his enemies only adds to his strength."
    [ [ newSkill
        { label   = "Psycho Mind Transmission"
        , desc    = "Inoichi invades the mind of an enemy, dealing 20 damage to them for 2 turns. While active, the target cannot use counter or reflect skills."
        , classes = [Mental, Melee, Uncounterable, Unreflectable]
        , cost    = χ [Nin]
        , cd      = 1
        , channel = Control 2
        , effects = [(Enemy, damage 20 • apply 1 [Uncounter])]
        }
      ]
    , [ newSkill
        { label   = "Sensory Radar"
        , desc    = "Inoichi steps back and focuses on the tide of battle. Each time an enemy uses a harmful skill, Inoichi will recover 10 health and gain a stack of [Sensory Radar]. While active, this skill becomes [Sensory Radar: Collate][r]."
        , classes = [Mental, Ranged]
        , cost    = χ [Nin]
        , effects = [ (Self, vary "Sensory Radar" "Sensory Radar: Collate")
                    , (Enemies, trap 0 OnHarm . self § heal 10 ° addStack)
                    ]
        }
      , newSkill
        { label   = "Sensory Radar: Collate"
        , desc    = "Inoichi compiles all the information he has gathered and puts it to use. For every stack of [Sensory Radar], he gains a random chakra. Ends [Sensory Radar]."
        , classes = [Mental, Ranged]
        , cost    = χ [Rand]
        , effects = [ (Enemies, removeTrap "Sensory Radar")
                    , (Self,    vary "Sensory Radar" ""
                              • perI "Sensory Radar" 0 
                                (gain . flip replicate Rand) 1
                              • remove "Sensory Radar")
                    ]
        }
      ]
    , [ newSkill
        { label   = "Mental Invasion"
        , desc    = "Inoichi preys on an enemy's weaknesses. For 4 turns, all invulnerability skills used by the target will have their duration reduced by 1 turn. While active, anyone who uses a harmful mental skill on the target will become invulnerable for 1 turn."
        , classes = [Mental, Ranged]
        , cost    = χ [ Rand ]
        , cd      = 4
        , effects = [(Enemy, apply 4 [Throttle Immune 1]
                           • trapFrom 4 (OnHarmed Mental) 
                             § apply 1 [Immune All])]
        }
      ]
    , invuln "Mobile Barrier" "Inoichi" [Mental]
    ] []
  ]
