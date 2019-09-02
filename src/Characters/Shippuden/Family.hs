{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Family (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Tsume Inuzuka"
    "A jōnin from the Hidden Leaf Village and mother to Kiba, Tsume shares his wild temperament, impatience, and odd sense of humor. Kuromaru, her animal companion, keeps her enemies at bay and strikes back at any who dare to attack her."
    [ [ Skill.new
        { Skill.name      = "Call Kuromaru"
        , Skill.desc      = "Kuromaru guards Tsume from her enemies for 4 turns, providing her with 10 points of damage reduction and dealing 10 damage to enemies who use harmful skills on her. While active, this skill becomes [Fierce Bite][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 4
        , Skill.channel   = Ongoing 4
        , Skill.start     =
          [ To Self $ vary "Call Kuromaru" "Fierce Bite" ]
        , Skill.effects   =
          [ To Self do
                apply 1 [Reduce All Flat 10]
                trapFrom 1 (OnHarmed All) $ damage 10
          ]
        }
      , Skill.new
        { Skill.name      = "Fierce Bite"
        , Skill.desc      = "Kuromaru pounces on an enemy, dealing 25 damage. If the target dies during the same turn, Tsume will become unkilllable for 2 turns, during which her damage will be increased by 10 and she will ignore stuns."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.effects   =
          [ To Enemy do
                trap' (-1) OnDeath $ self $
                    apply 2 [Strengthen All Flat 10, Endure, Ignore $ Any Stun]
                damage 25
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tunneling Fang"
        , Skill.desc      = "Spinning like a buzzsaw, Tsume deals 15 piercing damage to an enemy for 2 turns. Deals 5 additional damage during [Call Kuromaru]. While active, all stun skills used by the target will have their duration reduced by 2 turns."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand]
        , Skill.channel   = Action 2
        , Skill.effects   =
          [ To Enemy do
                bonus <- 5 `bonusIf` userHas "Call Kuromaru"
                pierce (15 + bonus)
                apply 1 [Throttle 2 $ Any Stun]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Light Bomb"
        , Skill.desc      = "Tsume blinds her enemies with a flash-bang, making her team invulnerable for 1 turn."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand]
        , Skill.charges   = 3
        , Skill.effects   =
          [ To Allies $ apply 1 [Invulnerable All] ]
        }
      ]
    , [ invuln "Dodge" "Tsume" [Physical] ]
    ] []
  , Character
    "Chiyo"
    "A widely-respected puppeteer and former leader of the Hidden Sand Village's Puppet Brigade, Elder Chiyo has a lifetime of combat experience. Her numerous puppets sow chaos among her enemies and shield her from harm, allowing her to use her other skills with impunity. If one of her allies is close to death, she can sacrifice her own life to restore theirs."
    [ [ Skill.new
        { Skill.name      = "Assault Blade"
        , Skill.desc      = "Hovering kunai deal 15 piercing damage to a targeted enemy and a random enemy. During [Ten Puppets Collection], this skill becomes [Three Treasure Suction Crush][r][r]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy  $ pierce 15
          , To REnemy $ pierce 15
          ]
        }
      , Skill.new
        { Skill.name      = "Three Treasure Suction Crush"
        , Skill.desc      = "Three puppets in a triangle formation create a vacuum hurricane which sucks in an enemy, dealing 30 damage to them and stunning their non-mental skills for 1 turn. Deals affliction damage if the target is affected by [Lion Sealing]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
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
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 5
        , Skill.channel   = Action 0
        , Skill.start     =
          [ To Self do
                defend 0 50
                vary "Assault Blade" "Three Treasure Suction Crush"
                vary "Ten Puppets Collection" "Lion Roar Sealing"
                onBreak'
          ]
        , Skill.effects   = [ To REnemy $ damage 10 ]
        }
      , Skill.new
        { Skill.name      = "Lion Roar Sealing"
        , Skill.desc      = "Chiyo uses an advanced sealing technique on an enemy, making them immune to effects from allies and preventing them from reducing damage or becoming invulnerable for 2 turns."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemy $ apply 2 [Expose, Seal] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Self-Sacrifice Reanimation"
        , Skill.desc      = "Chiyo prepares to use her forbidden healing technique on an ally. The next time their health reaches 0, their health is fully restored, they are cured of harmful effects, and Chiyo's health is reduced to 1."
        , Skill.classes   = [Chakra, Invisible]
        , Skill.cost      = [Blood, Nin]
        , Skill.charges   = 1
        , Skill.effects   =
          [ To XAlly $ trap 0 OnRes do
                cureAll
                setHealth 100
                self $ setHealth 1
          ]
        }
      ]
    , [ invuln "Chakra Barrier" "Chiyo" [Chakra] ]
    ] []
  ]
