module Blank
  ( context
  , character
  , ninja, ninjaWithSlot, ninjaWithSkill
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

character :: Character
character = Character
    { name     = mempty
    , price    = 0
    , bio      = mempty
    , skills   = newSkill :| [newSkill, newSkill, newSkill]
    , category = Original
    , groups   = mempty
    , ident    = mempty
    }
  where
    newSkill = Skill.new :| []

blankSlot :: Slot
blankSlot = unsafeHead Slot.all

ninjaWithSlot :: Slot -> Ninja
ninjaWithSlot slot = N.new slot character

ninja :: Ninja
ninja = ninjaWithSlot blankSlot

ninjaWithSkill :: Skill -> Ninja
ninjaWithSkill skill = N.new blankSlot character
                        { Character.skills = sk :| [sk, sk, sk]}
  where
    sk = skill :| []

game :: Wrapper
game = Wrapper.new $ ninjaWithSlot <$> Slot.all
