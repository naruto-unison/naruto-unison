{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original.Organizations (organizationCs) where

import StandardLibrary
import Game.Functions
import Game.Game
import Game.Structure

organizationCs :: [Character]
organizationCs =
  [ Character
    "Demon Brothers"
    "A pair of rogue chūnin from the Hidden Mist Village, the Demon Brothers are Zabuza's professional assassins. Armed with chain weapons, Gōzu and Meizu gang up on an enemy, disable them, and dispose of them in short order."
    [ [ newSkill
        { label   = "Chain Wrap"
        , desc    = "Gōzu throws his chains around an enemy, stunning their non-mental skills for 1 turn. The following turn, this skill becomes [Chain Shred][t]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [ (Enemy, apply 1 [Stun NonMental])
                    , (Self,  vary' 1 "Chain Wrap" "Chain Shred")
                    ]
        }
      , newSkill
        { label   = "Chain Shred"
        , desc    = "Meizu tears his chains through the target of [Chain Wrap], dealing 45 piercing damage and reapplying [Chain Wrap], stunning the target's non-mental skills for 1 turn."
        , require = HasU "Chain Wrap"
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , effects = [(Enemies, pierce 45
                             • apply' "Chain Wrap" 1 [Stun NonMental]
                             • self § vary' 1 "Chain Wrap" "Chain Shred")]
        }
      ]
    , [ newSkill
        { label   = "Bladed Gauntlet"
        , desc    = "Gōzu and Meizu attack an enemy with their gauntlets, dealing 30 damage. Deals 10 additional damage if the target is affected by [Chain Wrap]."
        , classes = [Physical, Melee]
        , cost    = χ [Rand, Rand]
        , effects = [(Enemy, withU "Chain Wrap" 10 damage 30)]
        }
      ]
    , [ newSkill
        { label   = "Water Melding"
        , desc    = "The Demon Brothers hide in water and gather their strength, gaining 20 permanent destructible defense and a taijutsu chakra."
        , classes = [Chakra]
        , cost    = χ [Rand, Rand]
        , cd      = 1
        , effects = [(Self, defend 0 20 • gain [Tai])]
        }
      ]
    , invuln' "Vanish" 
              "The Demon Brothers become invulnerable for 1 turn." 
              [Mental] 
              []
    ] []
  , Character
    "Haku"
    "The sole survivor of the Yuki clan, Haku is Zabuza's young but remarkably strong subordinate. With his inherited ice manipulation techniques, he disrupts his enemies while hiding safely behind crystalline mirrors."
    [ [ newSkill
        { label   = "Thousand Needles of Death"
        , desc    = "Haku flings numerous ice needles at an enemy, dealing 30 damage. Targets all enemies during [Crystal Ice Mirrors]."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Rand]
        , effects = [(Enemy, damage 30)]
        , changes = changeWith "Crystal Ice Mirrors" targetAll
        }
      ]
    , [ newSkill
        { label   = "Acupuncture"
        , desc    = "Haku sticks a needle into one of the target's vital points, altering the flow of energy through their body. If used on an enemy, the target is stunned for 1 turn. If used on an ally, all stun effects are removed and they ignore stuns for 1 turn. Targets all allies and enemies during [Crystal Ice Mirrors]."
        , classes = [Physical, Ranged, Bypassing]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [ (Enemy, apply 1 [Stun All])
                    , (XAlly, cureStun • apply 1 [Ignore Stun])
                    ]
        , changes = changeWith "Crystal Ice Mirrors" targetAll
        }
      ]
    , [ newSkill
        { label   = "Crystal Ice Mirrors"
        , desc    = "Haku fills the battlefield with disorienting crystalline mirrors, becoming invulnerable for 3 turns."
        , classes = [Chakra]
        , cost    = χ [Blood, Nin]
        , cd      = 6
        , effects = [(Self, apply 3 [Invulnerable All])]
        }
      ]
    , invuln "Parry" "Haku" [Physical]
    ] []
  , Character
    "Zabuza Momochi"
    "One of the Seven Swordsmen of the Mist, Zabuza is a rogue operative who specializes in silent assassination. Wielding Kubikiribōchō, the legendary executioner's broadsword, he uses concealing mist to catch his enemies off-guard, bypassing their defenses."
    [ [ newSkill
        { label   = "Soundless Murder"
        , desc    = "Zabuza emerges from mist behind an enemy's defenses to deal 30 piercing damage to them. Deals 15 additional damage and bypasses invulnerability during [Hidden Mist]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, withI "Hidden Mist" 15 pierce 30)]
        , changes = changeWith "Hidden Mist" § addClass Bypassing
        }
      ]
    , [ newSkill
        { label   = "Water Dragon"
        , desc    = "A torrent of water shaped like a giant dragon attacks all enemies, dealing 10 damage. Its ferocious attacks knocks back targets for 1 turn, stunning their physical skills and negating their affliction damage."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin]
        , cd      = 3
        , effects = [ (Enemies, damage 10 • apply 1 
                                [Stun Physical, Stun Affliction])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Hidden Mist"
        , desc    = "Mist covers the battlefield for 2 turns, providing 5 points of damage reduction to Zabuza and increasing the cost of enemy physical and mental skills by 1 random chakra."
        , classes = [Chakra, Ranged]
        , cost    = χ [Gen]
        , cd      = 3
        , effects = [ (Self,    apply 2 [ Reduce All 5])
                    , (Enemies, apply 2 [Exhaust Physical, Exhaust Mental])
                    ]
        }
      ]
    , invuln "Water Clone" "Zabuza" [Chakra]
    ] []
  , Character
    "Itachi Uchiha"
    "A master of Sharingan techniques, Itachi is an S-Rank rogue operative who has joined Akatsuki. His power comes at a steep price: using his Sharingan causes him to gradually go blind. He intends to make the most of whatever time he has left."
    [ [ newSkill
        { label   = "Mangekyō Sharingan"
        , desc    = "Itachi becomes invulnerable but loses 15 health each turn. While active, the cooldowns and chakra costs of his other skills are doubled. This skill can be used again with no chakra cost to cancel its effect."
        , classes = [Mental, Unremovable]
        , cost    = χ [Blood]
        , effects = [(Self, apply 0 [Invulnerable All, Afflict 15]
                           • vary "Mangekyō Sharingan" "Mangekyō Sharingan"
                           • vary "Amaterasu"          "Amaterasu"
                           • vary "Tsukuyomi"          "Tsukuyomi")]
        }
      , newSkill
        { label   = "Mangekyō Sharingan"
        , desc    = "Ends the effect of [Mangekyō Sharingan], halving Itachi's cooldowns and chakra costs."
        , classes = [Mental]
        , varicd  = True
        , effects = [(Self, remove "Mangekyō Sharingan" 
                          • vary "Mangekyō Sharingan" ""
                          • vary "Amaterasu"          ""
                          • vary "Tsukuyomi"          "")]
        }
      ]
    , [ newSkill
        { label   = "Amaterasu"
        , desc    = "Itachi sets an enemy on fire, dealing 15 affliction damage and 5 affliction damage each turn. Targets all enemies and deals double damage during [Mangekyō Sharingan]. Does not stack. Ends if Itachi dies."
        , classes = [Bane, Ranged, Soulbound, Nonstacking, Unreflectable]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [(Enemy, damage 15 • apply 0 [ Afflict 5])
                    ]
        }
      , newSkill
        { label   = "Amaterasu"
        , desc    = "Itachi sets an enemy on fire, dealing 15 affliction damage and 5 affliction damage each turn. Targets all enemies and deals double damage during [Mangekyō Sharingan]. Does not stack. Ends if Itachi dies."
        , classes = [Bane, Ranged, Soulbound, Nonstacking, Unreflectable]
        , cost    = χ [Nin, Nin]
        , cd      = 2
        , effects = [(Enemies, damage 30 • apply 0 [ Afflict 10])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Tsukuyomi"
        , desc    = "Itachi mentally tortures an enemy for what feels like an entire day in a matter of seconds, dealing 20 damage and stunning them for 1 turn. During [Mangekyō Sharingan], stuns the target for 3 turns—which is to say, 3 subjective days and nights."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen]
        , cd      = 1
        , effects = [(Enemy, damage 20 • apply 1 [ Stun All])
                    ]
        }
      , newSkill
        { label   = "Tsukuyomi"
        , desc    = "Itachi mentally tortures an enemy for what feels like an entire day in a matter of seconds, dealing 20 damage and stunning them for 1 turn. During [Mangekyō Sharingan], stuns the target for 3 turns—which is to say, 3 subjective days and nights."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Gen]
        , cd      = 2
        , effects = [(Enemy, damage 20 • apply 3 [ Stun All])
                    ]
        }
      ]
    , invuln "Sharingan Foresight" "Itachi" [Mental]
    ] []
  , Character
    "Kisame Hoshigaki"
    "One of the Seven Swordsmen of the Mist, Kisame is an S-Rank rogue ninja who has joined Akatsuki. Wielding the legendary sentient sword Samehada, Kisame disables his enemies while his eternally hungry sword eats their chakra."
    [ [ newSkill
        { label   = "Samehada Slash"
        , desc    = "Kisame slashes an enemy with the legendary sword Samehada, dealing 20 damage and stunning their chakra and mental skills for 1 turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai]
        , cd      = 1
        , effects = [ (Enemy, damage 20 
                            • apply 1 [Stun Chakra, Stun Mental]) 
                    ]
        }
      ]
    , [ newSkill
        { label   = "Samehada Shred"
        , desc    = "Kisame unwraps Samehada and shreds an enemy. For 2 turns, he deals 15 damage to the target and steals a random chakra."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Nin]
        , cd      = 2
        , channel = Action 2
        , effects = [(Enemy, steal 1 • damage 15)]
        }
      ]
    , [ newSkill
        { label   = "Super Shark Bomb"
        , desc    = "Kisame shoots a stream of compressed water at an enemy, dealing 20 damage, stunning their physical skills for 1 turn, and negating their affliction damage for 1 turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Nin]
        , cd      = 1
        , effects = [ (Enemy, damage 20 
                            • apply 1 [Stun Physical, Stun Affliction])
                    ]
        }
      ]
    , invuln "Scale Shield" "Kisame" [Physical]
    ] []
  , Character
    "Jirōbō"
    "A member of the Sound Five, Jirōbō hides his arrogance and hot temper beneath a calm facade. His immense strength and earth-rending attacks lay waste to all who stand against him."
    [ [ newSkill
        { label   = "Crushing Palm"
        , desc    = "Jirōbō delivers a ground-shaking punch to an enemy, dealing 30 damage. Deals 10 additional damage if [Sphere of Graves] was used last turn."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [ (Self,  tag 1)
                    , (Enemy, withI "Sphere of Graves" 10 damage 30)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Sphere of Graves"
        , desc    = "Jirōbō lifts the ground up and hurls it forward, dealing 20 damage to all enemies. Deals 10 additional damage if [Crushing Palm] was used last turn."
        , classes = [Physical, Ranged]
        , cost    = χ [Tai, Rand]
        , effects = [ (Self,    tag 1)
                    , (Enemies, withI "Crushing Palm" 10 damage 20)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Earth Dome Prison"
        , desc    = "Jirōbō provides 35 destructible defense to his team for 3 turns. Every turn that Jirōbō has destructible defense from [Earth Dome Prison], he steals a random chakra from the enemy team."
        , classes = [Chakra, Ranged]
        , cost    = χ [Nin, Nin, Rand]
        , cd      = 6
        , channel = Ongoing 3
        , start   = [ (Allies, defend 3 35 )
                    , (Self,   onBreak')
                    ]
        , effects = [(REnemy, steal 1)]
        }
      ]
    , invuln "Terra Shield" "Jirōbō" [Physical]
    ] []
  , Character
    "Kidōmaru"
    "A member of the Sound Five, Kidōmaru resembles a spider in both appearance and fighting style. His webs protect his allies and slow his enemies."
    [ [ newSkill
        { label   = "Spider War Bow"
        , desc    = "Kidōmaru fires an enzymatic arrow from his mouth, dealing 50 piercing damage to an enemy."
        , classes = [Physical, Ranged]
        , cost    = χ [Blood, Nin]
        , cd      = 1
        , effects = [(Enemy, pierce 50)]
        }
      ]
    , [ newSkill
        { label   = "Summoning: Kyodaigumo"
        , desc    = "Kidōmaru summons a giant spider which creates endless swarms of small spiders. For 5 turns, all enemies take 10 damage, their cooldowns are increased by 1, and Kidōmaru gains 10 points of damage reduction."
        , classes = [Physical, Ranged, Summon]
        , cost    = χ [Blood, Nin]
        , cd      = 4
        , channel = Ongoing 5
        , effects = [(Enemies, damage 10 • apply 1 [ Snare 1]) 
                    , (Self,    apply 1 [Reduce All 10])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Spiral Web"
        , desc    = "Kidōmaru weaves a protective web around himself or an ally which counters the first harmful physical skill used on the target."
        , classes = [Physical, Invisible, Unreflectable]
        , cost    = χ [Blood]
        , cd      = 2
        , effects = [(Ally, apply 0 [Counter Physical])]
        }
      ]
    , invuln "Spider Thread Armor" "Kidōmaru" [Chakra]
    ] []
  , Character
    "Tayuya"
    "A member of the Sound Five, Tayuya is foul-mouthed and aggressive. She plays her flute to trap her enemies in genjutsu and control the beasts she summons."
    [ [ newSkill
        { label   = "Summoning: Doki"
        , desc    = "Tayuya summons the Doki Demons, which deal 15 damage to all enemies for 2 turns and provide her with 10 points of damage reduction."
        , classes = [Physical, Ranged, Summon]
        , cost    = χ [Gen, Rand]
        , cd      = 1
        , channel = Ongoing 2
        , effects = [ (Enemies, damage 15) 
                    , (Self,    apply 1 [Reduce All 10])
                    ]
        }
      ]
    , [ newSkill
        { label   = "Demon Flute: Phantom Wave"
        , desc    = "Illusory ghosts pour out of the Doki demons, dealing 10 affliction damage to an enemy and removing a random chakra. Requires [Summoning: Doki]."
        , require = HasI 1 "Summoning: Doki"
        , classes = [Ranged]
        , cost    = χ [Rand]
        , effects = [(Enemy, drain 1 • afflict 10)]
        }
      ]
    , [ newSkill
        { label   = "Demon Flute"
        , desc    = "Playing a hypnotizing melody on her flute, Tayuya stuns all enemies' skills for 1 turn."
        , classes = [Mental, Ranged]
        , cost    = χ [Gen, Rand]
        , cd      = 4
        , effects = [(Enemies, apply 1 [Stun All])]
        }
      ]
    , invuln "Foresight" "Tayuya" [Mental]
    ] []
  , Character
    "Sakon and Ukon"
    "Members of the Sound Five, Sakon and Ukon are nearly identical twins with a bloodline that enables each brother to live within the body of the other."
    [ [ newSkill
        { label   = "Demon Twin Attack"
        , desc    = "Acting in unison, Sakon and Ukon punch an enemy, dealing 40 damage. Deals 20 damage and costs 1 taijutsu chakra during [Demon Parasite]."
        , classes = [Physical, Melee]
        , cost    = χ [Tai, Rand]
        , effects = [(Enemy, withI "Demon Parasite" (-20) damage 40)]
        , changes = changeWith "Demon Parasite" $ setCost [Tai]
        }
      ]
    , [ newSkill
        { label   = "Demon Parasite"
        , desc    = "Sakon deals 20 affliction damage to an enemy and gains 15 points of damage reduction until the target dies. Cannot be used while active."
        , classes = [Bane, Unreflectable, Unremovable, Single]
        , cost    = χ [Blood, Blood]
        , effects = [ (Enemy, trap' 0 OnDeath . self § remove "Demon Parasite"
                            • bomb 0 [Afflict 20]
                              [(Done, self § remove "Demon Parasite")]
                            • self § bomb 0 [Reduce All 15]
                              [(Done, everyone § remove "Demon Parasite")]) ]
        }
      ]
    , [ newSkill
        { label   = "Regeneration"
        , desc    = "Ukon merges with Sakon, restoring 30 health and ending [Demon Parasite]."
        , classes = [Physical]
        , cost    = χ [Rand, Rand]
        , cd      = 2
        , effects = [ (Self,     heal 30 • cancelChannel "Demon Parasite"
                               • everyone § remove "Demon Parasite")
                    ]
        }
      ]
    , invuln' "Summoning: Rashōmon" 
              "Sakon and Ukon become invulnerable for 1 turn. Ends [Demon Parasite]." 
              [Chakra, Summon]
              [remove "Demon Parasite"]
    ] []
  , Character
    "Kimimaro"
    "A member of the Sound Five, Kimimaro led the team before his illness. His bloodline grants him unstoppable offensive power, but his illness is slowly killing him, eroding the little time he has left."
    [ [ newSkill
        { label   = "Camellia Dance"
        , desc    = "Kimimaro wields his arm bones as swords, dealing 30 damage to an enemy. Kimimaro loses 5 health."
        , classes = [Physical, Melee, Uncounterable, Unreflectable]
        , cost    = χ [Tai]
        , effects = [ (Enemy, damage 30)
                    , (Self,  sacrifice 0 5)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Clematis Dance"
        , desc    = "Kimimaro attacks an enemy with a long, sharp bone spear, dealing 40 damage and stunning them for a turn. Kimimaro loses 10 health."
        , classes = [Physical, Melee, Uncounterable, Unreflectable]
        , cd      = 1
        , cost    = χ [Blood, Tai]
        , effects = [(Enemy, damage 40 • apply 1 [ Stun All])
                    , (Self,  sacrifice 0 10)
                    ]
        }
      ]
    , [ newSkill
        { label   = "Bracken Dance"
        , desc    = "A forest of razor-sharp bones erupts from the ground, dealing 30 damage to all enemies and reducing all enemy non-mental damage by 20 for 1 turn. Kimimaro loses 15 health and another 15 health at the end of his next turn."
        , classes = [Physical, Ranged, Unremovable]
        , cost    = χ [Blood, Rand, Rand]
        , cd      = 2
        , effects = [(Enemies, damage 30 • apply 1 [ Weaken NonMental 20])
                    , (Self,    apply (-2) [Afflict 15])
                    ]
        }
      ]
    , invuln "Larch Dance" "Kimimaro" [Physical]
    ] []
  ]
