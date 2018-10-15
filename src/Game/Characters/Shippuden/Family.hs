{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden.Family (familyCsS) where

import Game.Functions
import Game.Game
import Game.Structure

familyCsS :: [Group -> Character]
familyCsS =
  [ Character
    "Chiyo"
    "A widely-respected puppeteer and former leader of the Hidden Sand Village's Puppet Brigade, Elder Chiyo has a lifetime of combat experience. Her numerous puppets sow chaos among her enemies and shield her from harm, allowing her to use her other skills with impunity. If one of her allies is close to death, she can sacrifice her own life to restore theirs."
    [ [ newSkill
        { label   = "Assault Blade"
        , desc    = "Hovering kunai deal 15 piercing damage to a targeted enemy and a random enemy. During [Ten Puppets Collection], this skill becomes [Three Treasure Suction Crush][r][r]."
        , classes = [Chakra, Ranged]
        , cost    = χ [Rand]
        , cd      = 1
        , effects = [ (Enemy,  pierce 15) 
                    , (REnemy, pierce 15)
                    ]
        }
      , newSkill
        { label   = "Three Treasure Suction Crush"
        , desc    = "Three puppets in a triangle formation create a vacuum hurricane which sucks in an enemy, dealing 30 damage to them and stunning their non-mental skills for 1 turn. Deals affliction damage if the target is affected by [Lion Sealing]."
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
                          • onBreak')]
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
  ]
