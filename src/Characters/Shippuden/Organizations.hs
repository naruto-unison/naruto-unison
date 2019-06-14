{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Characters.Shippuden.Organizations (cs) where

import Characters.Base

import qualified Model.Skill as Skill

cs :: [Category -> Character]
cs =
  [ Character
    "Sai"
    "An operative of the Hidden Leaf Village's elite Root division, Sai is quietly expressive and artistic. He uses a set of brushes with chakra-infused ink to give life to his illustrations, which usually take the form of powerful black-and-white beasts."
    [ [ Skill.new
        { Skill.name      = "Super Beast Scroll: Lions"
        , Skill.desc      = "Sai draws a pack of lions that attack an enemy, dealing 30 damage to them and providing 20 destructible defense to Sai for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = k [Gen, Rand]
        , Skill.effects   = [ p Enemy $ damage 30
                            , p Self  $ defend 1 20
                            ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Beast Scroll: Snake"
        , Skill.desc      = "Sai draws a snake that paralyzes an enemy with its bite, stunning their physical and chakra skills for 1 turn and preventing them from reducing damage or becoming invulnerable. During [Ink Mist], this skill becomes [Super Beast Scroll: Bird][g]."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = k [Gen]
        , Skill.effects   =
          [ p Enemy $ apply 1 [Stun Physical, Stun Chakra, Expose] ]
        }
      , Skill.new
        { Skill.name      = "Super Beast Scroll: Bird"
        , Skill.desc      = "Sai draws a bird in the air, which deals 25 damage to an enemy and stuns them for 1 turn."
        , Skill.classes   = [Physical, Melee, Bypassing]
        , Skill.cost      = k [Gen]
        , Skill.effects   =
          [ p Enemy do
                damage 25
                apply 1 [Stun All]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ink Mist"
        , Skill.desc      = "Streams of ink coil in the air around Sai and his allies for 3 turns, obscuring them from enemies and allowing Sai to draw three-dimensionally. If someone on Sai's team is stunned, they become invulnerable for 1 turn. If their chakra is removed or stolen, they gain 1 random chakra. If an ally receives new non-affliction damage, Sai's damage increases by 10 for 1 turn."
        , Skill.classes   = [Mental, Bypassing]
        , Skill.cost      = k [Rand, Rand]
        , Skill.effects   =
          [ p Enemies $ trap 3 OnChakra $ self $ gain [Rand]
          , p Allies do
                trap 3 OnStunned $ apply 1 [Invulnerable All]
                trap 3 (OnDamaged NonAffliction) $
                    self $ apply 1 [Strengthen All Flat 10 ]
          , p Self $  vary' 3 "Super Beast Scroll: Snake"
                              "Super Beast Scroll: Bird"
          ]
        }
      ]
    , [ invuln "Ink Clone" "Sai" [Chakra] ]
    ] []
    -- TODO "Ignore all effects that improve their skills"
--  , Character
--    "Yamato"
--    "An operative of the Hidden Leaf Village's elite Root division, Yamato carries the first Hokage's wood-manipulation abilities due to having been injected with his cells.
-- TODO Izumo and Kotetsu swap health
  , Character
      "Fū Yamanaka"
      "An operative of the Hidden Leaf Village's elite Root division, Fū is emotionless and ruthlessly straightforward. His only drive is unswerving loyalty to Danzō. His combination of long-distance Yamanaka genjutsu and his personal form of taijutsu makes him a formidable threat in any situation, but his trump card is the ability to swap his consciousness into the body of an opponent and make use of all their skills."
      [ [ Skill.new
          { Skill.name      = "Tantō Slash"
          , Skill.desc      = "Fū slashes an enemy with his tantō, dealing 25 damage. Deals 15 additional damage if the target is affected by [Mind Transfer]."
          , Skill.classes   = [Physical, Melee]
          , Skill.cost      = k [Tai]
          , Skill.effects   =
            [ p Enemy do
                  bonus <- 15 `bonusIf` targetHas "Mind Transfer"
                  damage (25 + bonus)
            ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Mind Transfer"
          , Skill.desc      = "Fū takes over an enemy's mind and steals all removable beneficial effects on them. The target's destructible defense is transferred to Fū, and Fū's destructible barrier is transferred to the target. While active, Fū detects all invisible effects and enemy cooldowns."
          , Skill.classes   = [Chakra]
          , Skill.cost      = k [Gen]
          , Skill.cooldown  = 3
          , Skill.channel   = Control 3
          , Skill.start     = [ p Enemy $ commandeer ]
          , Skill.effects   =
            [ p Enemy do
                  tag 1
                  enemies $ hide' "revealed" 1 [Reveal]
            ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Mind Transfer Puppet Curse"
          , Skill.desc      = "Fū defends himself or an ally with a puppet trap. For 2 turns, the first enemy who uses a new harmful physical or chakra skill on the target will be countered. If an enemy is countered, Fū's skills are replaced by their skills for 4 turns and their skills are replaced by [Puppet Curse: Attack] for 4 turns. Effects from Fū's usage of their skills are canceled when Fū's skills revert."
          , Skill.classes   = [Mental, InvisibleTraps, Unreflectable]
          , Skill.cost      = k [Gen]
          , Skill.cooldown  = 3
          , Skill.effects   =
            let f = Play do
                        copyAll 4
                        tag (-4)
                        teach 4 Shallow 4
                        teachOne 4 3 Deep 5
                        self do
                            resetAll
                            bomb (-4) [] [ p Done resetAll ] in
            [ p Ally $
                  applyWith [Invisible] 2 [Parry Physical f, Parry Chakra f]
            ]
          }
        ]
      , [ invuln "Dodge" "Fū" [Physical] ]
      , [ Skill.new
          { Skill.name      = "Puppet Curse: Attack"
          , Skill.desc      = "Trapped in a puppet, little can be done but flail about and hope to hit someone! Deals 15 damage to an enemy."
          , Skill.classes   = [Physical, Melee]
          , Skill.cost      = k [Rand]
          , Skill.effects   =
            [ p Enemy $ damage 15 ]
          }
        ]
      , [ Skill.new
          { Skill.name      = "Puppet Curse: Defend"
          , Skill.desc      = "Trapped in a puppet, little can be done but flail about and hope to block an attack! The puppet becomes invulnerable for 1 turn."
          , Skill.classes   = [Physical, Melee]
          , Skill.cost      = k [Rand]
          , Skill.cooldown  = 4
          , Skill.effects   =
            [ p Self $ apply 1 [Invulnerable All] ]
          }
        ]
      ] []
  , Character
    "Torune Aburame"
    "An operative of the Hidden Leaf Village's elite Root division, Torune was born with rare venom-resistant antibodies that allow him to carry the Aburame clan's most dangerous species of beetle. The venom beetles cover his skin like armor, protecting him and infesting anyone who dares to touch him."
    [ [ Skill.new
        { Skill.name      = "Nano-Sized Venom Beetles"
        , Skill.desc      = "Torune applies a Venom Beetle to an enemy, dealing 5 affliction damage for 5 turns, and gains 15 permanent destructible defense. Whoever destroys Torune's destructible defense from this skill will have a Venom Beetle applied to them. While Torune has destructible defense from this skill, this skill costs 1 random chakra but does not provide any destructible defense."
        , Skill.classes   = [Bane, Melee]
        , Skill.cost      = k [Blood]
        , Skill.cooldown  = 0
        , Skill.effects   =
          [ p Enemy $ apply' "Venom Beetle" 5 [Afflict 5]
          , p Self do
                defend 0 15
                onBreak $ addStacks "Venom Beetle" 1
          ]
        , Skill.changes   = changeWith "Nano-Sized Venom Beetles" $
                            setCost [Rand] `also` targetOnly [Enemy]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Jar of Poison"
        , Skill.desc      = "Torune applies a Venom Beetle to all enemies, dealing 5 affliction damage to each for 5 turns, and gains 30 permanent destructible defense. Whoever destroys Torune's destructible defense from this skill will have a Venom Beetle applied to them. While Torune has destructible defense from this skill, this skill costs 1 random chakra but does not provide any destructible defense."
        , Skill.classes   = [Bane, Melee]
        , Skill.cost      = k [Blood, Blood]
        , Skill.cooldown  = 0
        , Skill.effects   =
          [ p Enemies $ apply' "Venom Beetle" 5 [Afflict 5]
          , p Self do
                defend 0 30
                onBreak $ addStacks "Venom Beetle" 1
          ]
        , Skill.changes   = changeWith "Jar of Poison" $
                            setCost [Rand, Rand] `also` targetOnly [Enemies]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Venom Explosion"
        , Skill.desc      = "Torune detonates all Venom Beetles on an enemy, depleting 1 random chakra for each Venom Beetle destroyed. "
        , Skill.require   = HasU "Venom Beetle"
        , Skill.classes   = [Bane, Melee]
        , Skill.cost      = k [Blood, Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ p Enemy do
                stacks <- targetStacks "Venom Beetle"
                deplete stacks
          ]
        }
      ]
    , [ invuln "Dodge" "Torune" [Physical] ]
    ] []
  , Character
    "Danzō Shimura"
    "The founder and leader of the Hidden Leaf Village's elite Root division, Danzō has had a hand in almost every important global event since he came to power. His numerous implanted Sharingans allow him to repeatedly cheat death."
    [ [ Skill.new
        { Skill.name      = "Izanagi"
        , Skill.desc      = "Danzō gains 10 Sharingan and loses 1 each turn. If his health reaches 0, he regains 10 health per Sharingan and loses all Sharingan. When he has no Sharingan remaining, this skill becomes [Reverse Tetragram Sealing][r][r][r]."
        , Skill.classes   = [Mental, Resource]
        , Skill.cost      = k [Blood]
        , Skill.channel   = Ongoing 0
        , Skill.start     =
          [ p Self do
                addStacks "Sharingan" 10
                vary "Izanagi" "Izanagi"
          ]
        , Skill.effects   =
          [ p Self $ unlessM (userHas "paused") do
                removeStack "Sharingan"
                unlessM (userHas "Sharingan") do
                    cancelChannel "Izanagi"
                    vary "Izanagi" "Reverse Tetragram Sealing"
                    trap' 1 OnRes do
                        cancelChannel "Izanagi"
                        vary "Izanagi" "Reverse Tetragram Sealing"
                        stacks <- userStacks "Sharingan"
                        setHealth (10 * stacks)
                        remove "Sharingan"
          ]
        }
      , Skill.new
        { Skill.name      = "Izanagi"
        , Skill.desc      = "Pauses the effect of [Izanagi] for 1 turn."
        , Skill.classes   = [Mental]
        , Skill.cost      = k [Blood]
        , Skill.effects   = [ p Self $ flag' "paused"]
        }
      , Skill.new
        { Skill.name      = "Reverse Tetragram Sealing"
        , Skill.desc      = "Kills Danzō and makes all enemies immune to effects from each other for 3 turns. In 3 turns, enemies who are not invulnerable die."
        , Skill.classes   = [Mental, Bypassing, Unremovable]
        , Skill.cost      = k [Rand, Rand, Rand]
        , Skill.effects   =
          [ p Enemies $ delay (-3) kill
          , p Self do
                enemies $ apply (-3) [Seal]
                kill
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Vacuum Wave"
        , Skill.desc      = "Danzō exhales slicing blades of air at an enemy, dealing 20 piercing damage. Deals 15 additional damage if the target is affected by [Kotoamatsukami]."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = k [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ p Enemy do
                bonus <- 15 `bonusIf` userHas "Kotoamatsukami"
                pierce (20 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Kotoamatsukami"
        , Skill.desc      = "Danzō infiltrates the mind of an enemy. The next time they use a skill, its effects will be nullified, they will be stunned for 2 turns, and this skill will be replaced by the skill they used for 2 turns. Danzō's copy of their skill has no chakra cost or cooldown and ends when this skill reverts."
        , Skill.classes   = [ Mental, Ranged, Invisible, Uncounterable, Unreflectable]
        , Skill.cost      = k [Blood, Gen, Gen]
        , Skill.cooldown  = 9
        , Skill.effects   =
          [ p Enemy do
                apply 2 [Replace 2 All 2 True]
                trap 2 (OnCounter Uncounterable) do
                    remove "Kotoamatsukami"
                    apply' "Kotoamatsukami Stun" 2 [Stun All]
          ]
        }
      ]
    , [ invuln "Summoning: Baku" "Danzō" [Chakra, Summon] ]
    ] []
  ]
