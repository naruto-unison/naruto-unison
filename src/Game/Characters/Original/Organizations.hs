{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Original.Organizations (characters) where

import Game.Characters.Base

import qualified Game.Model.Skill as Skill

characters :: [Int -> Category -> Character]
characters =
  [ Character
    "Izumo and Kotetsu"
    "A pair of chūnin from the Hidden Leaf Village assigned to hunt down members of Akatsuki, Izumo and Kotetsu are close friends and effective partners. Although their strength may be somewhat lacking as individuals, they have a significant advantage of their own: there are two of them."
    [ [ Skill.new
        { Skill.name      = "Mace Crush"
        , Skill.desc      = "Kotetsu slams an enemy with his mace, dealing 30 damage. Deals 10 additional damage to an enemy affected by [Syrup Trap]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` targetHas "Syrup Trap"
                damage (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Syrup Trap"
        , Skill.desc      = "Izumo spits out a field of sticky syrup that impedes the enemy team. For 2 turns, enemies that use chakra skills will have their chakra skills stunned for 1 turn, and enemies that use physical skills will have their physical skills stunned for 1 turn."
        , Skill.classes   = [Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                trap 2 (OnAction Chakra)   $ apply 1 [Stun Chakra]
                trap 2 (OnAction Physical) $ apply 1 [Stun Physical]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Devastate"
        , Skill.desc      = "Izumo flanks an enemy from the left, making them vulnerable to Kotetsu's [Annihilate] for 3 turns. If the target is affected by [Annihilate], Izumo deals 65 damage to them. If Izumo uses [Tag Team], this skill becomes [Annihilate][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                tag 3
                whenM (targetHas "Annihilate") $ damage 65
          ]
        }
      , Skill.new
        { Skill.name      = "Annihilate"
        , Skill.desc      = "Kotetsu flanks an enemy from the right, making them vulnerable to Izumo's [Devastate] for 3 turns. If the target is affected by [Devastate], Kotetsu deals 65 damage to them. If Kotetsu uses [Tag Team], this skill becomes [Devastate][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemy do
                tag 3
                whenM (targetHas "Devastate") $ damage 65
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tag Team"
        , Skill.desc      = "Izumo tags out, swapping his health with Kotetsu's. Once used, if Kotetsu dies, Izumo will immediately take over. "
        , Skill.classes   = [Nonstacking, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.require   = HasI (-1) "solo"
        , Skill.effects   =
          [ To Self do
                userHealth <- user health
                tagHealth <- userStacks "Kotetsu's Health"
                setHealth if tagHealth == 0 then 100 else tagHealth
                remove "Kotetsu's Health"
                addStacks "Izumo's Health" userHealth
                vary "Devastate" "Annihilate"
                vary "Tag Team" "Tag Team"
                trap' 0 OnRes do
                    tagHealth' <- userStacks "Izumo's Health"
                    setHealth tagHealth'
                    remove "Izumo's Health"
                    hide' "solo" 0 []
                    vary "Devastate" baseVariant
                    vary "Tag Team" baseVariant
          ]
        }
      , Skill.new
        { Skill.name      = "Tag Team"
        , Skill.desc      = "Kotetsu tags out, swapping his health with Izumo's. Once used, if Izumo dies, Kotetsu will immediately take over. "
        , Skill.classes   = [Nonstacking, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.require   = HasI (-1) "solo"
        , Skill.effects   =
          [ To Self do
                userHealth <- user health
                tagHealth <- userStacks "Izumo's Health"
                setHealth if tagHealth == 0 then 100 else tagHealth
                remove "Izumo's Health"
                addStacks "Kotetsu's Health" userHealth
                vary "Devastate" baseVariant
                vary "Tag Team" baseVariant
                trap' 0 OnRes do
                    tagHealth' <- userStacks "Kotetsu's Health"
                    setHealth tagHealth'
                    remove "Kotetsu's Health"
                    hide' "solo" 0 []
                    vary "Devastate" "Annihilate"
                    vary "Tag Team" "Tag Team"
          ]
        }
      ]
    ]
  , Character
    "Aoba Yamashiro"
    "A jōnin from the Hidden Leaf Village assigned to hunt down Akatsuki members, Aoba maintains a reserved and impassive demeanor in public that quickly disappears among friends. He uses his significant genjutsu prowess to summon numerous crows and spread his consciousness among them, controlling all of their actions simultaneously."
    [ [ Skill.new
        { Skill.name      = "Scattering Crow Swarm"
        , Skill.desc      = "A flock of self-duplicating crows swarms the enemy team for 4 turns, dealing 5 damage each turn and providing 5 points of damage reduction to Aoba and his allies."
        , Skill.classes   = [Mental, Ranged, Resource]
        , Skill.cost      = [Gen]
        , Skill.dur       = Ongoing 4
        , Skill.start     =
          [ To Self do
              enemies $ apply 4 []
              allies  $ apply 4 [Reduce All Flat 5]
          ]
        , Skill.effects   =
          [ To Enemies $ whenM (not <$> targetHas "swarmed") do
                stacks <- targetStacks "Scattering Crow Swarm"
                damage (5 * stacks)
                flag' "swarmed"
          ]
        , Skill.interrupt =
          [ To Self do
                everyone $ remove "Scattering Crow Swarm"
                cancelChannel "Scattering Crow Swarm" -- just in case
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Revenge of the Murder"
        , Skill.desc      = "If the target ally's health reaches 0 within 3 turns, their health will be set to 5, all their skills will be replaced by [Converging Murder][g][g], and they will become completely invulnerable to all skills. At the end of their next turn, they will die."
        , Skill.classes   = [Mental, Ranged, Invisible, Uncounterable, Unreflectable, Unremovable]
        , Skill.cost      = [Rand]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To XAlly $ trap 3 OnRes do
                resetAll
                setHealth 5
                teach 1 Deep 2
                setFace
                bomb (-1) [Invulnerable All, Alone, Seal, Enrage, Focus]
                          [ To Done killHard ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Converging Murder"
        , Skill.desc      = "Aoba directs all of his crows at an enemy, dealing 45 damage to them. Deals 5 additional damage for each stack of [Scattering Crow Swarm] on the target."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- target $ numAnyStacks "Scattering Crow Swarm"
                damage (45 + 5 * stacks)
          ]
        }
      ]
    , [ invuln "Crow Barrier" "Aoba" [Chakra] ]
    ]
  , Character
    "Ibiki Morino"
    "A sadistic jōnin who specializes in extracting information, Ibiki commands the Hidden Leaf Village's Torture and Interrogation Force. Pain is his preferred method of communication, and his preferred approach to battle is ensuring all options available to his enemies will lead to their defeat."
    [ [ Skill.new
        { Skill.name      = "Biding Time"
        , Skill.desc      = "Provides 10 points of permanent damage reduction to Ibiki. Each time an enemy uses a skill that deals damage to Ibiki, he will gain a stack of [Payback]. Once used, this skill becomes [Payback][r]."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Self do
                apply 0 [Reduce All Flat 10]
                vary "Biding Time" "Payback"
                trap 0 (OnDamaged All) $ addStacks "Payback" 1
          ]
        }
     , Skill.new
        { Skill.name      = "Payback"
        , Skill.desc      = "Deals 15 damage to an enemy. Spends all stacks of [Payback] to deal 5 additional damage per stack."
        , Skill.classes   = [Mental, Melee]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Payback"
                damage (15 + 5 * stacks)
          , To Self do
                remove "Payback"
                vary "Biding Time" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Iron Maiden"
        , Skill.desc      = "A spike-filled iron coffin shaped like a cat imprisons an enemy. For 3 turns, each time the target uses a skill on Ibiki or his allies, they will take 25 piercing damage. Ibiki gains 30 permanent destructible defense."
        , Skill.classes   = [Summon, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ trap 3 OnHarm $ pierce 25
          , To Self  $ defend 0 30
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Torture Chamber"
        , Skill.desc      = "A cage of chains and gears surrounds an enemy. For 3 turns, each time the target does not use a skill, they will take 25 piercing damage. Ibiki gains 30 permanent destructible defense."
        , Skill.classes   = [Summon, Melee]
        , Skill.cost      = [Nin, Rand]
        , Skill.effects   =
          [ To Enemy $ trap 3 OnNoAction $ pierce 25
          , To Self  $ defend 0 30
          ]
        }
      ]
    , [ invuln "Dodge" "Ibiki" [Physical] ]
    ]
  , Character
    "Yūgao Uzuki"
    "An operative of the Hidden Leaf Village, Yūgao is dedicated and thorough. Having grown close to Hayate, Yūgao combines his expert sword techniques with her sealing abilities."
    [ [ Skill.new
        { Skill.name      = "Moonlight Night"
        , Skill.desc      = "Light flashes off Yūgao's sword as she spins it in a circle, surrounding herself with disorienting afterimages, then strikes at an enemy to deal 50 damage."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Gen, Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                stacks <- userStacks "Moon Haze"
                damage (50 + 25 * stacks)
          , To Self  $ remove "Moon Haze"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Moon Haze"
        , Skill.desc      = "The light of the moon empowers Yūgao, providing 20 destructible defense for 1 turn and adding 25 damage to her next [Moonlight Night]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Self do
                defend 1 20
                addStack
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sealing Technique"
        , Skill.desc      = "Yūgao places a powerful and thorough seal on an enemy. For 2 turns, they do not benefit from damage reduction, destructible defense, invulnerability, counters, or reflects."
        , Skill.classes   = [Bypassing, Uncounterable, Unreflectable]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 2 [Expose, Uncounter, Undefend] ]
        }
      ]
    , [ invuln "Parry" "Yūgao" [Physical] ]
    ]
  , Character
    "Demon Brothers"
    "A pair of rogue chūnin from the Hidden Mist Village, the Demon Brothers are Zabuza's professional assassins. Armed with chain weapons, Gōzu and Meizu gang up on an enemy, disable them, and dispose of them in short order."
    [ [ Skill.new
        { Skill.name      = "Chain Wrap"
        , Skill.desc      = "Gōzu throws his chains around an enemy, stunning their non-mental skills for 1 turn. The following turn, this skill becomes [Chain Shred][t]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun NonMental]
          , To Self  $ vary' 1 "Chain Wrap" "Chain Shred"
          ]
        }
      , Skill.new
        { Skill.name      = "Chain Shred"
        , Skill.desc      = "Meizu tears his chains through the target of [Chain Wrap], dealing 45 piercing damage and reapplying [Chain Wrap], stunning the target's non-mental skills for 1 turn."
        , Skill.require   = HasU 1 "Chain Wrap"
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemies do
                pierce 45
                apply' "Chain Wrap" 1 [Stun NonMental]
                self $ vary' 1 "Chain Wrap" "Chain Shred"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Bladed Gauntlet"
        , Skill.desc      = "Gōzu and Meizu attack an enemy with their gauntlets, dealing 30 damage. Deals 10 additional damage if the target is affected by [Chain Wrap]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Rand, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 10 `bonusIf` targetHas "Chain Wrap"
                damage (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Melding"
        , Skill.desc      = "The Demon Brothers hide in water and gather their strength, gaining 20 permanent destructible defense and a taijutsu chakra."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Self do
                defend 0 20
                gain [Tai]
          ]
        }
      ]
    , [ (invuln "Vanish" "The Demon Brothers" [Mental])
        { Skill.desc = "The Demon Brothers become invulnerable for 1 turn." }
      ]
    ]
  , Character
    "Haku"
    "The sole survivor of the Yuki clan, Haku is Zabuza's young but remarkably strong subordinate. With his inherited ice manipulation techniques, he disrupts his enemies while hiding safely behind crystalline mirrors."
    [ [ Skill.new
        { Skill.name      = "Thousand Needles of Death"
        , Skill.desc      = "Haku flings numerous ice needles at an enemy, dealing 30 damage. Targets all enemies during [Crystal Ice Mirrors]."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Rand]
        , Skill.effects   =
          [ To Enemy $ damage 30 ]
        , Skill.changes   =
            changeWith "Crystal Ice Mirrors" targetAll
        }
      ]
    , [ Skill.new
        { Skill.name      = "Acupuncture"
        , Skill.desc      = "Haku sticks a needle into one of the target's vital points, altering the flow of energy through their body. If used on an enemy, the target is stunned for 1 turn. If used on an ally, all stuns and disabling effects are cured and they ignore stuns for 1 turn. Targets all allies and enemies during [Crystal Ice Mirrors]."
        , Skill.classes   = [Physical, Ranged, Bypassing]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ apply 1 [Stun All]
          , To XAlly do
                cureStun
                apply 1 [Focus]
          ]
        , Skill.changes   =
            changeWith "Crystal Ice Mirrors" targetAll
        }
      ]
    , [ Skill.new
        { Skill.name      = "Crystal Ice Mirrors"
        , Skill.desc      = "Haku fills the battlefield with disorienting crystalline mirrors, becoming invulnerable for 3 turns."
        , Skill.classes   = [Chakra]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 6
        , Skill.effects   =
          [ To Self $ apply 3 [Invulnerable All] ]
        }
      ]
    , [ invuln "Parry" "Haku" [Physical] ]
    ]
  , Character
    "Zabuza Momochi"
    "One of the Seven Swordsmen of the Mist, Zabuza is a rogue operative who specializes in silent assassination. Wielding Kubikiribōchō, the legendary executioner's broadsword, he uses concealing mist to catch his enemies off-guard, bypassing their defenses."
    [ [ Skill.new
        { Skill.name      = "Soundless Murder"
        , Skill.desc      = "Zabuza emerges from mist behind an enemy's defenses to deal 30 piercing damage to them. Deals 15 additional damage and bypasses invulnerability during [Hidden Mist]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- 15 `bonusIf` userHas "Hidden Mist"
                pierce (30 + bonus)
          ]
        , Skill.changes   =
            changeWith "Hidden Mist" \x ->
                x { Skill.classes = Bypassing `insertSet` Skill.classes x }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Water Dragon"
        , Skill.desc      = "A torrent of water shaped like a giant dragon attacks all enemies, dealing 10 damage. Its ferocious attacks knocks back targets for 1 turn, stunning their physical skills and negating their affliction damage."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Enemies do
                damage 10
                apply 1 [Stun Physical, Stun Affliction]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Hidden Mist"
        , Skill.desc      = "Mist covers the battlefield for 2 turns, providing 5 points of damage reduction to Zabuza and increasing the cost of enemy physical and mental skills by 1 arbitrary chakra."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 3
        , Skill.effects   =
          [ To Self    $ apply 2 [Reduce All Flat 5]
          , To Enemies $ apply 2 [Exhaust Physical, Exhaust Mental]
          ]
        }
      ]
    , [ invuln "Water Clone" "Zabuza" [Chakra] ]
    ]
  , Character
    "Itachi Uchiha"
    "A master of Sharingan techniques, Itachi is a rogue operative from the Hidden Leaf Village who has joined Akatsuki. His power comes at a steep price: using his Sharingan causes him to gradually go blind. He intends to make the most of whatever time he has left."
    [ [ Skill.new
        { Skill.name      = "Mangekyō Sharingan"
        , Skill.desc      = "Itachi becomes invulnerable but loses 15 health each turn. While active, the cooldowns and chakra costs of his other skills are doubled. This skill can be used again with no chakra cost to cancel its effect."
        , Skill.classes   = [Mental, Unremovable]
        , Skill.cost      = [Blood]
        , Skill.effects   =
          [ To Self do
                apply 0 [Invulnerable All, Afflict 15]
                vary "Mangekyō Sharingan" "Mangekyō Sharingan"
          ]
        }
      , Skill.new
        { Skill.name      = "Mangekyō Sharingan"
        , Skill.desc      = "Ends the effect of [Mangekyō Sharingan], halving Itachi's cooldowns and chakra costs."
        , Skill.classes   = [Mental]
        , Skill.varicd    = True
        , Skill.effects   =
          [ To Self do
                remove "Mangekyō Sharingan"
                vary "Mangekyō Sharingan" baseVariant
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Amaterasu"
        , Skill.desc      = "Itachi sets an enemy on fire, dealing 15 affliction damage and 5 affliction damage each turn. Targets all enemies and deals double damage during [Mangekyō Sharingan]. Does not stack. Ends if Itachi dies."
        , Skill.classes   = [Bane, Ranged, Soulbound, Nonstacking, Unreflectable]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 15
                apply 0 [Afflict 5]
          ]
        , Skill.changes =
            changeWith "Mangekyō Sharingan" \x ->
                x { Skill.cooldown = 2 * Skill.cooldown x
                  , Skill.cost     = 2 * Skill.cost x
                  , Skill.effects  =
                    [ To Enemies do
                          damage 30
                          apply 0 [Afflict 10]
                    ]
                  }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Tsukuyomi"
        , Skill.desc      = "Itachi mentally tortures an enemy for what feels like an entire day in a matter of seconds, dealing 20 damage and stunning them for 1 turn. During [Mangekyō Sharingan], stuns the target for 3 turns—which is to say, 3 subjective days and nights."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Stun All]
          ]
        , Skill.changes =
            changeWith "Mangekyō Sharingan" \x ->
                x { Skill.cooldown = 2 * Skill.cooldown x
                  , Skill.cost     = 2 * Skill.cost x
                  , Skill.effects  =
                    [ To Enemies do
                          damage 40
                          apply 3 [Stun All]
                    ]
                  }
        }
      ]
    , [ invuln "Sharingan Foresight" "Itachi" [Mental] ]
    ]
  , Character
    "Kisame Hoshigaki"
    "One of the Seven Swordsmen of the Mist, Kisame is a rogue operative who has joined Akatsuki. Wielding the legendary sentient sword Samehada, Kisame disables his enemies while his eternally hungry sword eats their chakra."
    [ [ Skill.new
        { Skill.name      = "Samehada Slash"
        , Skill.desc      = "Kisame slashes an enemy with the legendary sword Samehada, dealing 20 damage and stunning their chakra and mental skills for 1 turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Stun Chakra, Stun Mental]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Samehada Shred"
        , Skill.desc      = "Kisame unwraps Samehada and shreds an enemy. For 2 turns, he deals 15 damage to the target and absorbs 1 random chakra."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Nin]
        , Skill.cooldown  = 2
        , Skill.dur       = Action 2
        , Skill.effects   =
          [ To Enemy do
                absorb 1
                damage 15
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Super Shark Bomb"
        , Skill.desc      = "Kisame shoots a stream of compressed water at an enemy, dealing 20 damage, stunning their physical skills for 1 turn, and negating their affliction damage for 1 turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy do
                damage 20
                apply 1 [Stun Physical, Stun Affliction]
          ]
        }
      ]
    , [ invuln "Scale Shield" "Kisame" [Physical] ]
    ]
  , Character
    "Jirōbō"
    "A member of the Sound Five, Jirōbō hides his arrogance and hot temper beneath a calm facade. His immense strength and earth-rending attacks lay waste to all who stand against him."
    [ [ Skill.new
        { Skill.name      = "Crushing Palm"
        , Skill.desc      = "Jirōbō delivers a ground-shaking punch to an enemy, dealing 30 damage. Deals 10 additional damage if [Sphere of Graves] was used last turn."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Self  $ tag 1
          , To Enemy do
                bonus <- 10 `bonusIf` userHas "Sphere of Graves"
                damage (30 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Sphere of Graves"
        , Skill.desc      = "Jirōbō lifts the ground up and hurls it forward, dealing 20 damage to all enemies. Deals 10 additional damage if [Crushing Palm] was used last turn."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Self $ tag 1
          , To Enemies do
                bonus <- 10 `bonusIf` userHas "Crushing Palm"
                damage (20 + bonus)
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Earth Dome Prison"
        , Skill.desc      = "Jirōbō provides 35 destructible defense to his team for 3 turns. Every turn that Jirōbō has destructible defense from [Earth Dome Prison], he absorbs 1 random chakra from the enemy team."
        , Skill.classes   = [Chakra, Ranged]
        , Skill.cost      = [Nin, Nin, Rand]
        , Skill.cooldown  = 6
        , Skill.dur       = Action 3
        , Skill.start     =
          [ To Allies $ defend 3 35
          , To Self     onBreak'
          ]
        , Skill.effects   =
          [ To REnemy $ absorb 1 ]
        }
      ]
    , [ invuln "Terra Shield" "Jirōbō" [Physical] ]
    ]
  , Character
    "Kidōmaru"
    "A member of the Sound Five, Kidōmaru resembles a spider in both appearance and fighting style. His webs protect his allies and slow his enemies."
    [ [ Skill.new
        { Skill.name      = "Spider War Bow"
        , Skill.desc      = "Kidōmaru fires an enzymatic arrow from his mouth, dealing 50 piercing damage to an enemy."
        , Skill.classes   = [Physical, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 1
        , Skill.effects   =
          [ To Enemy $ pierce 50 ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Kyodaigumo"
        , Skill.desc      = "Kidōmaru summons a giant spider which creates endless swarms of small spiders. For 5 turns, all enemies take 10 damage, their cooldowns are increased by 1, and Kidōmaru gains 10 points of damage reduction."
        , Skill.classes   = [Summon, Ranged]
        , Skill.cost      = [Blood, Nin]
        , Skill.cooldown  = 4
        , Skill.dur       = Ongoing 5
        , Skill.effects   =
          [ To Enemies do
                damage 10
                apply 1 [Snare 1]
          , To Self $ apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Spiral Web"
        , Skill.desc      = "Kidōmaru weaves a protective web around himself or an ally, which counters the first physical skill an enemy uses on the target."
        , Skill.classes   = [Physical, Invisible, Unreflectable]
        , Skill.cost      = [Blood]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Ally $ trap 0 (Counter Physical) $ return () ]
        }
      ]
    , [ invuln "Spider Thread Armor" "Kidōmaru" [Chakra] ]
    ]
  , Character
    "Tayuya"
    "A member of the Sound Five, Tayuya is foul-mouthed and aggressive. She plays her flute to trap her enemies in genjutsu and control the beasts she summons."
    [ [ Skill.new
        { Skill.name      = "Summoning: Doki"
        , Skill.desc      = "Tayuya summons the Doki Demons, which deal 15 damage to all enemies for 2 turns and provide her with 10 points of damage reduction."
        , Skill.classes   = [Summon, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 1
        , Skill.dur       = Ongoing 2
        , Skill.effects   =
          [ To Enemies $ damage 15
          , To Self    $ apply 1 [Reduce All Flat 10]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Demon Flute: Phantom Wave"
        , Skill.desc      = "Illusory ghosts pour out of the Doki demons, dealing 10 affliction damage to an enemy and depleting 1 random chakra. Requires [Summoning: Doki]."
        , Skill.require   = HasI 1 "Summoning: Doki"
        , Skill.classes   = [Ranged]
        , Skill.cost      = [Rand]
        , Skill.effects   =
          [ To Enemy do
                deplete 1
                afflict 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Demon Flute"
        , Skill.desc      = "Playing a hypnotizing melody on her flute, Tayuya stuns all enemies' skills for 1 turn."
        , Skill.classes   = [Mental, Ranged]
        , Skill.cost      = [Gen, Rand]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Enemies $ apply 1 [Stun All] ]
        }
      ]
    , [ invuln "Foresight" "Tayuya" [Mental] ]
    ]
  , Character
    "Sakon and Ukon"
    "Members of the Sound Five, Sakon and Ukon are nearly identical twins with a bloodline that enables each brother to live within the body of the other."
    [ [ Skill.new
        { Skill.name      = "Demon Twin Attack"
        , Skill.desc      = "Acting in unison, Sakon and Ukon punch an enemy, dealing 40 damage. Deals 20 damage and costs 1 taijutsu chakra during [Demon Parasite]."
        , Skill.classes   = [Physical, Melee]
        , Skill.cost      = [Tai, Rand]
        , Skill.effects   =
          [ To Enemy do
                bonus <- (-20) `bonusIf` userHas "Demon Parasite"
                damage (40 + bonus)
          ]
        , Skill.changes   =
            changeWith "Demon Parasite" $ \x -> x { Skill.cost = [Tai] }
        }
      ]
    , [ Skill.new
        { Skill.name      = "Demon Parasite"
        , Skill.desc      = "Sakon deals 20 affliction damage to an enemy and gains 15 points of damage reduction until the target dies. Cannot be used while active."
        , Skill.require   = HasI (-1) "Demon Parasite"
        , Skill.classes   = [Bane, Unreflectable, Unremovable]
        , Skill.cost      = [Blood, Blood]
        , Skill.effects   =
          [ To Enemy do
                trap' 0 OnDeath $ self $ remove "Demon Parasite"
                bomb 0 [Afflict 20]
                       [ To Done $ self $ remove "Demon Parasite" ]
                self $ bomb 0 [Reduce All Flat 15]
                       [ To Done $ everyone $ remove "Demon Parasite" ]
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Regeneration"
        , Skill.desc      = "Ukon merges with Sakon, restoring 30 health and ending [Demon Parasite]."
        , Skill.classes   = [Physical]
        , Skill.cost      = [Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Self do
                heal 30
                cancelChannel "Demon Parasite"
                everyone $ remove "Demon Parasite"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Summoning: Rashōmon"
        , Skill.desc      = "Sakon and Ukon become invulnerable for 1 turn. Ends [Demon Parasite]."
        , Skill.classes   = [Summon]
        , Skill.cooldown  = 4
        , Skill.effects   =
          [ To Self $ remove "Demon Parasite" ]
        }
      ]
    ]
  , Character
    "Kimimaro"
    "A member of the Sound Five, Kimimaro led the team before his illness. His bloodline grants him unstoppable offensive power, but his illness is slowly killing him, eroding the little time he has left."
    [ [ Skill.new
        { Skill.name      = "Camellia Dance"
        , Skill.desc      = "Kimimaro wields his arm bones as swords, dealing 30 damage to an enemy. Kimimaro loses 5 health."
        , Skill.classes   = [Physical, Melee, Uncounterable, Unreflectable]
        , Skill.cost      = [Tai]
        , Skill.effects   =
          [ To Enemy $ damage 30
          , To Self  $ sacrifice 0 5
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Clematis Dance"
        , Skill.desc      = "Kimimaro attacks an enemy with a long, sharp bone spear, dealing 40 damage and stunning them for a turn. Kimimaro loses 10 health."
        , Skill.classes   = [Physical, Melee, Uncounterable, Unreflectable]
        , Skill.cooldown  = 1
        , Skill.cost      = [Blood, Tai]
        , Skill.effects   =
          [ To Enemy do
                damage 40
                apply 1 [Stun All]
          , To Self $ sacrifice 0 10
          ]
        }
      ]
    , [ Skill.new
        { Skill.name      = "Bracken Dance"
        , Skill.desc      = "A forest of razor-sharp bones erupts from the ground, dealing 30 damage to all enemies and reducing all enemy non-mental damage by 20 for 1 turn. Kimimaro loses 15 health and another 15 health at the end of his next turn."
        , Skill.classes   = [Physical, Ranged, Unremovable]
        , Skill.cost      = [Blood, Rand, Rand]
        , Skill.cooldown  = 2
        , Skill.effects   =
          [ To Enemies do
                damage 30
                apply 1 [Weaken NonMental Flat 20]
          , To Self $ apply (-2) [Afflict 15]
          ]
        }
      ]
    , [ invuln "Larch Dance" "Kimimaro" [Physical] ]
    ]
  ]