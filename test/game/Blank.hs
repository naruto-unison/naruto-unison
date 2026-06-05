module Blank
  ( context
  , character
  , ninja, ninjaWithSlot, ninjaWithSkill
  , ninjas
  , game
  ) where

import ClassyPrelude

import           Game.Model.Character (Category(..), Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as N
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Util ((!!))

import           Wrapper (Wrapper)
import qualified Wrapper

context :: Context
context = Context { skill     = Skill.new
                  , user      = Slot.all !! 0
                  , target    = Slot.all !! 3
                  , new       = True
                  , continues = False
                  }

characterWithSkill :: Skill -> Character
characterWithSkill skill = Character
    { name     = mempty
    , price    = 0
    , bio      = mempty
    , skills   = sk :| [sk, sk, sk]
    , category = Original
    , groups   = mempty
    , ident    = mempty
    }
  where
    sk = skill :| []

character :: Character
character = characterWithSkill Skill.new

blankSlot :: Slot
blankSlot = unsafeHead Slot.all

ninjaWithSlot :: Slot -> Ninja
ninjaWithSlot slot = N.new slot character

ninja :: Ninja
ninja = ninjaWithSlot blankSlot

ninjaWithSkill :: Skill -> Ninja
ninjaWithSkill skill = N.new blankSlot $ characterWithSkill skill

ninjas :: [Ninja]
ninjas = ninjaWithSlot <$> Slot.all

game :: Wrapper
game = Wrapper.new ninjas
