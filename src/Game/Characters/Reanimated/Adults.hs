{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Reanimated.Adults (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Category -> Character]
characters =
  [ Character
    "Pakura"
    "Reanimated by Kabuto, Pakura was a jōnin diplomat known as the Hero of the Hidden Sand until she was betrayed and ambushed by the Hidden Mist Village. Her unique scorch style combines fire and wind elements to create heat orbs that mummify her enemies."
    [ [ Skill.new
        { Skill.name      = "Scorch Style"
        , Skill.desc      = "Miniature suns orbit Pakura and attack an enemy, dealing 20 affliction damage. For 2 turns, enemies who use skills on Pakura will take 10 affliction damage."
        , Skill.classes   = [Chakra, Ranged, Bane]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ afflict 20
          , To Self do
                trapFrom 2 (OnHarmed All) $ afflict 10
                tag 2
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Searing Combat"
        , Skill.desc      = "Pakura ignites an enemy, dealing 20 affliction damage to them for 2 turns. Cannot be used on an enemy already affected by this skill."
        , Skill.require   = HasU 0 "Searing Combat"
        , Skill.classes   = [Physical, Melee, Bane]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy $ apply 2 [Afflict 20] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Steam Kill"
        , Skill.desc      = "One of Pakura's orbs contacts an enemy and evaporates the water inside their body, mummifying them and dealing 40 affliction damage. Requires [Scorch Style]."
        , Skill.require   = HasI 1 "Scorch Style"
        , Skill.classes   = [Chakra, Melee, Bane]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Enemy $ afflict 40 ]
        }
      ]
    , [ invuln "Block" "Pakura" [Physical] ]
    ]
    75
  , Character
    "Gari"
    "Reanimated by Kabuto, Gari was a jōnin member of the Hidden Stone Village's Demolitions Unit. Augmented with explosive force, his taijutsu attacks overwhelm enemies who meet them head-on."
    [ [ Skill.new
        { Skill.name      = "Exploding Palm"
        , Skill.desc      = "Gari strikes an enemy and sets off an explosion at the moment of contact. The next time they use a skill on Gari or his allies, they will take 20 piercing damage. This skill stacks."
        , Skill.classes   = [Bypassing, Physical, Ranged, Invisible]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                addStack
                trap 0 OnHarm do
                    stacks <- targetStacks "Exploding Palm"
                    pierce (20 * stacks)
                    removeTrap "Exploding Palm"
                    remove "Exploding Palm"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Ground Pound"
        , Skill.desc      = "An explosive shockwave destabilizes the ground around Gari. If an enemy uses a skill on Gari next turn, they will take 25 damage, and Gari will gain 2 turns of 25% damage reduction and restore 15 health each turn."
        , Skill.classes   = [Chakra, Melee, Invisible]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self $ trapFrom 1 (OnHarmed All) do
                damage 25
                self $ apply 2 [Reduce All Percent 25, Heal 15]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Landmine Fist"
        , Skill.desc      = "Making direct contact with an enemy, Gari generates an explosion inside them that deals 35 piercing damage."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 35 ]
        }
      ]
    , [ invuln "Dodge" "Gari" [Physical] ]
    ]
    75
  , Character
    "Ginkaku"
    "The word \"Silver\" tattooed on his shoulder marks Ginkaku as one half of the Silver and Gold Brothers, the most notorious criminals in the history of the Hidden Cloud Village. Ginkaku and Kinkaku use tailed-beast chakra to wield the revered arsenal of their ancestor, the Sage of the Six Paths. When they fight together, they make a terrifying pair."
    [ [ Skill.new
        { Skill.name      = "Seven Stars Blade"
        , Skill.desc      = "Using a legendary sword that records a person's most frequently used word, Ginkaku slashes at an enemy's soul, dealing 25 piercing damage and extracting a Spirit Word from them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                pierce 25
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amber Purification Jar"
        , Skill.desc      = "Ginkaku captures an enemy inside the Sage of the Sixth Path's sealing jar, stunning their physical and melee skills for 1 turn and extracting a Spirit Word from them."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Stun Physical, Stun Melee]
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Benihisago"
        , Skill.desc      = "Ginkaku draws an enemy's soul into a crimson gourd, dealing 10 affliction damage and 5 additional damage for each of the target's Spirit Words. This also extracts a Spirit Word from the target and increases the damage of Kinkaku's [Scroll of Fire] on the target by 5."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- target $ numAnyStacks "Spirit Word"
                afflict (10 + 5 * stacks)
                addStack' "Scroll of Fire"
          ]
        }
      ]
    , [ invuln "Parry" "Ginkaku" [Physical] ]
    ]
    75
  , Character
    "Kinkaku"
    "The word \"Gold\" tattooed on his shoulder marks Kinkaku as one half of the Silver and Gold Brothers, the most notorious criminals in the history of the Hidden Cloud Village. Ginkaku and Kinkaku use tailed-beast chakra to wield the revered arsenal of their ancestor, the Sage of the Six Paths. When they fight together, they make a terrifying pair."
    [ [ Skill.new
        { Skill.name      = "Leaf Fan"
        , Skill.desc      = "Using a legendary fan that can generate any of the five elements, Kinkaku deals 25 affliction damage to an enemy, extracts a Spirit Word from them, and gains 50% damage reduction for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy do
                afflict 25
                addStack' "Spirit Word"
          , To Self $ apply 1 [Reduce All Percent 50]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Gold Rope"
        , Skill.desc      = "Kinkaku binds an enemy with the Sage of the Six Path's soul-stealing rope. The next time they use a skill on Kinkaku or his allies, they will take 35 damage and a Spirit Word will be extracted from them."
        , Skill.classes   = [Chakra, Melee]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 0 OnHarm do
                damage 35
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Scroll of Fire"
        , Skill.desc      = "A coil of flame erupts from the Bashōsen Leaf Fan, dealing 20 damage to all enemies and extracting a Spirit Word from each of them."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                stacks <- target $ numAnyStacks "Scroll of Fire"
                damage (20 + 5 * stacks)
                addStack' "Spirit Word"
          ]
        }
      ]
    , [ invuln "Parry" "Kinkaku" [Physical] ]
    ]
    75
  , Character
    "Toroi"
    "Reanimated by Kabuto, Toroi was a jōnin from the Hidden Cloud Village with the rare ability to manipulate magnetic fields. His weapons are magnetized, and as their magnetic field accumulates on targets, they become harder and harder to avoid."
    [ [ Skill.new
        { Skill.name      = "Demon Wind Shuriken"
        , Skill.desc      = "Toroi deals 20 damage to an enemy with a giant shuriken and defends himself with several others. For 2 turns, enemies who use skills on Toroi will become permanently unable to be healed or cured."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self $ trapFrom 2 (OnHarmed All) $ apply 0 [Plague]
          , To Enemy do
                stacksA <- targetStacks "Conserving Bee Twin Blades"
                stacksB <- targetStacks "Magnetic Current"
                damage (20 + 5 * stacksA + 5 * stacksB)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Magnetic Field"
        , Skill.desc      = "Toroi generates a field of magnetism around himself that provides 50% damage reduction for 3 turns. While active, this skill becomes [Conserving Bee Twin Blades]."
        , Skill.classes   = [Physical]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                apply 3 [Reduce All Percent 50]
                vary' 3 "Magnetic Field" "Conserving Bee Twin Blades"
          ]
        }
      , Skill.new
        { Skill.name      = "Conserving Bee Twin Blades"
        , Skill.desc      = "Toroi hurls a magnetized shuriken at an enemy, dealing 10 piercing damage and increasing the damage of [Demon Wind Shuriken] and [Magnetic Current] to the target by 5."
        , Skill.classes   = [Physical, Ranged]
        , Skill.effects   =
          [ To Enemy do
                stacks <- targetStacks "Magnetic Current"
                pierce (10 + 5 * stacks)
                addStack
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Magnetic Current"
        , Skill.desc      = "Toroi energizes the field with magnetism, dealing 10 piercing damage to all enemies and increasing the damage of [Demon Wind Shuriken] and [Conserving Bee Twin Blades] to the targets by 5."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                stacks <- targetStacks "Conserving Bee Twin Blades"
                pierce (10 + 5 * stacks)
                addStack
          ]
        }
      ]
    , [ invuln "Shuriken Threads" "Toroi" [Physical] ]
    ]
    75
  , Character
    "Fukai"
    "Reanimated by Kabuto, Fukai was the previous jinchūriki of Gyūki, the eight-tailed beast. Also known as Blue B, Fukai was unable to control Gyūki, which led to his downfall and the deaths of his comrades."
    [ [ Skill.new
        { Skill.name      = "Chakra Arms"
        , Skill.desc      = "Manifesting limbs of tailed-beast chakra, Fukai deals 10 affliction damage to all enemies and increases the damage of [Tailed Beast Bomb Barrage] and [Lariat] to the targets by 5."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemies do
                afflict 10
                addStack
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tailed Beast Bomb Barrage"
        , Skill.desc      = "With a continuous attack of tailed-beast bombs, Fukai deals 30 damage to an enemy. All damage he receives—including piercing and affliction—is permanently reduced by 10%."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self $ apply 0 [Reduce Affliction Percent 10]
          , To Enemy do
                stacks <- targetStacks "Chakra Arms"
                damage (30 + 5 * stacks)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Lariat"
        , Skill.desc      = "Rushing an enemy, Fukai deals 15 percing damage to them and stuns their chakra and ranged skills for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- targetStacks "Chakra Arms"
                pierce (15 + 5 * stacks)
                apply 1 [Stun Chakra, Stun Ranged]
          ]
        }
      ]
    , [ invuln "Dodge" "Fukai" [Physical] ]
    ]
    75
  , Character
    "Chiyo"
    "Reanimated by Kabuto, Chiyo was the leader of the Hidden Sand Village's Puppet Brigade. Her expertise with chakra threads allows her to control numerous puppets at once."
    [ [ Skill.new
        { Skill.name      = "Ten Puppets Collection"
        , Skill.desc      = "Commanding a brigade of puppets, Chiyo stuns an enemy's physical and ranged skills for 1 turn and deals 20 damage to a random enemy for 2 turns."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.start     =
          [ To Enemy $ apply 1 [Stun Physical, Stun Ranged] ]
        , Skill.effects   =
          [ To REnemy $ damage 20 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Possum"
        , Skill.desc      = "Chiyo feigns unconsciousness to lure an enemy into a false sense of security. If the target uses a skill on Chiyo or her allies, they will be countered and take 20 damage, and their physical skills will be stunned for 1 turn."
        , Skill.classes   = [Physical, Melee, Invisible]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ trap 0 OnHarm do
                damage 20
                apply 1 [Stun Physical]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Army of Illusions"
        , Skill.desc      = "Chiyo takes control of multiple bodies and attacks an enemy, dealing 20 damage and becoming invulnerable to melee skills for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy $ damage 20
          , To Self  $ apply 1 [Invulnerable Melee]
          ]
        }
      ]
    , [ invuln "Puppet Distraction" "Chiyo" [Physical] ]
    ]
    75
  , Character
    "Kimimaro"
    "Reanimated by Kabuto, Kimimaro was a member of the Sound Five until he was claimed by illness. Loyal to Orochimaru, Kimimaro now follows Kabuto, who carries Orochimaru's chakra and shares similar ambitions."
    [ [ Skill.new
        { Skill.name      = "Clematis Dance"
        , Skill.desc      = "Kimimaro attacks the enemy team with long, sharp bone spears, dealing 20 damage and killing them if their health reaches 5 or lower."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                damage 20
                hp <- target health
                when (hp < 5) kill
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Macabre Bone Pulse"
        , Skill.desc      = "Kimimmaro warps his skeleton into blades and attacks an enemy, dealing 45 piercing damage."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Blood, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 45 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Digital Shrapnel"
        , Skill.desc      = "A volley of bullets shoot forth from Kimimaro's fingertips, providing him with 50% damage reduction for 1 turn. Next turn, enemies who use skills will take 20 damage."
        , Skill.classes   = [Physical, Ranged, Invisible]
        , Skill.cooldown  = 2
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Enemies $ trap 1 (OnAction All) $ damage 20
          , To Self $ apply 1 [Reduce All Percent 50]
          ]
        }
      ]
    , [ invuln "Block" "Kimimaro" [Physical] ]
    ]
    75
  , Character
    "Chūkichi"
    "Reanimated by Kabuto, Chūkichi was a jōnin renowned as the Hidden Mist Village's most talented sensor. Due to his refusal to serve Akatsuki, Chūkichi had his personality completely erased by Kabuto, transforming him into a mere killing machine."
    [ [ Skill.new
        { Skill.name      = "Hidden Frost"
        , Skill.desc      = "A cloud of frost conceals Chūkichi, rendering him effectively invisible. For 2 turns, he is invulnerable to ranged skills."
        , Skill.classes   = [Chakra]
        , Skill.cooldown  = 3
        , Skill.cost      = [Nin]
        , Skill.effects   =
          [ To Self $ apply 2 [Invulnerable Ranged] ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Psychic Jamming"
        , Skill.desc      = "Chūkichi telepathically disrupts the minds of all enemies, increasing the costs of their skills by 1 arbitrary chakra for 1 turn. During [Hidden Frost], this skill costs 1 arbitrary chakra."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies $ apply 1 [Exhaust All] ]
        , Skill.changes   =
            changeWith "Hidden Frost" \x -> x { Skill.cost = [Rand] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Silent Killing"
        , Skill.desc      = "Chūkichi ambushes an enemy, preventing them from reducing damage or becoming invulnerable for 1 turn and dealing 30 piercing damage. During [Hidden Frost], this skill deals 10 additional damage and costs 1 taijutsu chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                apply 1 [Expose]
                bonus <- 10 `bonusIf` userHas "Hidden Frost"
                pierce (30 + bonus)
          ]
        , Skill.changes   =
            changeWith "Hidden Frost" \x -> x { Skill.cost = [Rand] }
        }
      ]
    , [ invuln "Hide" "Chūkichi" [Mental] ]
    ]
    75
  ]
