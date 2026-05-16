module Blank
  ( context
  , character
  , ninja, ninjaWithSlot
  , game, gameOf
  ) where

import ClassyPrelude

import           Game.Model.Character (Category(..), Character(Character))
import qualified Game.Model.Character
import           Game.Model.Context (Context(Context))
import qualified Game.Model.Context
import qualified Game.Model.Game as Game
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import qualified Game.Model.Skill as Skill
import           Game.Model.Slot (Slot)
import qualified Game.Model.Slot as Slot
import           Handler.Play.Wrapper (Wrapper(Wrapper))
import qualified Handler.Play.Wrapper
import           Util ((!!))

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
    , bio      = mempty
    , skills   = newSkill :| [newSkill, newSkill, newSkill]
    , category = Original
    , groups   = mempty
    , price    = 0
    }
  where
    newSkill = Skill.new :| []

ninjaWithSlot :: Slot -> Ninja
ninjaWithSlot slot = Ninja.new slot character

ninja :: Ninja
ninja = ninjaWithSlot $ unsafeHead Slot.all

game :: Wrapper
game = gameOf $ ninjaWithSlot <$> Slot.all

gameOf :: [Ninja] -> Wrapper
gameOf ninjas = Wrapper { progress = []
                        , game     = Game.new
                        , ninjas   = fromList ninjas
                        }
