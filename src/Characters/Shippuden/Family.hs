{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Characters.Shippuden.Family (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Chiyo"
    "A widely-respected puppeteer and former leader of the Hidden Sand Village's Puppet Brigade, Elder Chiyo has a lifetime of combat experience. Her numerous puppets sow chaos among her enemies and shield her from harm, allowing her to use her other skills with impunity. If one of her allies is close to death, she can sacrifice her own life to restore theirs."
    [ [ Skill.new
        { Skill.name      = "Assault Blade"
        , Skill.desc      = "Hovering kunai deal 15 piercing damage to a targeted enemy and a random enemy. During [Ten Puppets Collection], this skill becomes [Three Treasure Suction Crush][r][r]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy  $ pierce 15
          , p REnemy $ pierce 15
          ]
        }
      , Skill.new
        { Skill.name      = "Three Treasure Suction Crush"
        , Skill.desc      = "Three puppets in a triangle formation create a vacuum hurricane which sucks in an enemy, dealing 30 damage to them and stunning their non-mental skills for 1 turn. Deals affliction damage if the target is affected by [Lion Sealing]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                apply 1 [Stun NonMental]
                has <- targetHas "Lion Roar Sealing"
                if has then afflict 30 else damage 30
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ten Puppets Collection"
        , Skill.desc      = "Chiyo commands a brigade of puppets, gaining 50 permanent destructible defense. Each turn that Chiyo has destructible defense from this skill, her puppets deal 10 damage to a random enemy. While active, this skill becomes [Lion Roar Sealing][b]."
        , Skill.classes   = [Chakra, Ranged, Single]
        , Skill.cost      = k [Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.channel   = Action 0
        , Skill.start     =
          [ p Self do
                defend 0 50
                vary "Assault Blade" "Three Treasure Suction Crush"
                vary "Ten Puppets Collection" "Lion Roar Sealing"
                onBreak'
          ]
        , Skill.effects   = [ p REnemy $ damage 10 ]
        }
      , Skill.new
        { Skill.name      = "Lion Roar Sealing"
        , Skill.desc      = "Chiyo uses an advanced sealing technique on an enemy, making them immune to effects from allies and preventing them from reducing damage or becoming invulnerable for 2 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ p Enemy $ apply 2 [Expose, Seal] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Self-Sacrifice Reanimation"
        , Skill.desc      = "Chiyo prepares to use her forbidden healing technique on an ally. The next time their health reaches 0, their health is fully restored, they are cured of harmful effects, and Chiyo's health is reduced to 1."
        , Skill.classes   = [Chakra, Invisible]
        , Skill.cost      = k [Blood, Nin]
        , Skill.charges   = 1
        , Skill.effects   =
          [ p XAlly $ trap 0 OnRes do
                cureAll
                setHealth 100
                self $ setHealth 1
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Chiyo" [Chakra] ]
    ] []
  ]
