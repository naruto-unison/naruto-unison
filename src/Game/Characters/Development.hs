{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP             #-}
{-# OPTIONS_HADDOCK hide     #-}

module Game.Characters.Development (characters) where

import Game.Characters.Import

#ifdef DEVELOPMENT
import           Game.Model.Character (Category(Original))
import qualified Game.Model.Skill as Skill

characters :: [Character]
characters = (\x -> x 0 Original) <$>
  [ Character
    "Gaara of the Funk"
    "Gaara's life has been marked by abandonment. Shortly after he was born, his mother tragically died from severe funk poisoning. The only true friend he ever had was his uncle, Mr. Expendable, whom Gaara was later forced to murder in self-defense. He has nothing left but hatred, bloodlust, and incredible dance moves.\n[This development-only character exists in stack exec -- yesod devel, but not stack exec -- yesod keter.]"
    []
    [ [ Skill.new
        { Skill.name    = "Nchk-Nchk-Nchk-Nchk"
        , Skill.desc    = "The power of beatboxing grants 50 of each chakra type."
        , Skill.effects =
          [ To Self $ gain $ replicate 50 =<< [Blood, Gen, Nin, Tai] ]
        }
      ]
    , [ Skill.new
        { Skill.name    = "The Funk Wasn't With You"
        , Skill.desc    = "Permanently stuns the entire enemy team. Once used, this skill becomes [Could've Had a V8]."
        , Skill.classes = [Bypassing, Uncounterable, Unreflectable]
        , Skill.effects =
          [ To Enemies $ apply 0 [Stun All]
          , To Self $
              hide 0 [Alternate "The Funk Wasn't With You" "Could've Had a V8"]
          ]
        }
      , Skill.new
        { Skill.name    = "Could've Had a V8"
        , Skill.desc    = "Frees the enemy team from the effect of [The Funk Wasn't With You]. Once used, this skill becomes [The Funk Wasn't With You]."
        , Skill.classes = [Bypassing, Uncounterable, Unreflectable]
        , Skill.effects =
          [ To Enemies $ remove "The Funk Wasn't With You"
          , To Self $ remove "the funk wasn't with you"
          ]
        }
      ]
    , [ Skill.new
        { Skill.name    = "Funk Coffin"
        , Skill.desc    = "Instantly kills a target who does not respect the funk."
        , Skill.classes = [Bypassing, Uncounterable, Unreflectable]
        , Skill.effects =
          [ To Enemy kill
          , To Ally kill
          ]
        }
      ]
    , [ Skill.new
        { Skill.name    = "Dance Dance Resurrection"
        , Skill.desc    = "Revives a dead target to full health."
        , Skill.require = HealthU 0
        , Skill.classes = [Necromancy, Bypassing, Uncounterable, Unreflectable]
        , Skill.effects =
          [ To Enemy factory
          , To XAlly factory
          ]
        }
      ]
    ]
  ]
#else
characters :: [Character]
characters = []
#endif
