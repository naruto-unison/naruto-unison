module Game.Model.Skill
  ( Skill(..), new, chakraClasses
  , Target(..)
  , Key(..), key
  , Transform
  , defaultName
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import qualified Game.Model.Chakra as Chakra
import           Game.Model.Channel (Channeling(..))
import           Game.Model.Internal (Key(..), Ninja, Skill(..), Requirement(..), Target(..))
import qualified Game.Model.Slot as Slot

-- | The type signature of 'changes'.
type Transform = (Ninja -> Skill -> Skill)

-- | Default values.
new :: Skill
new = Skill { name      = "Unnamed"
            , desc      = ""
            , require   = Usable
            , classes   = mempty
            , cost      = 0
            , cooldown  = 0
            , charges   = 0
            , dur       = Instant
            , start     = []
            , effects   = []
            , stunned   = []
            , interrupt = []
            , changes   = const id
            , owner     = unsafeHead Slot.all
            }

-- | Adds 'Model.Class.Bloodline', 'Model.Class.Genjutsu',
-- 'Model.Class.Ninjutsu', 'Model.Class.Taijutsu', and 'Model.Class.Random'
-- to the 'classes' of a @Skill@ if they are included in its 'cost'.
chakraClasses :: Skill -> Skill
chakraClasses skill =
    skill { classes = Chakra.classes (cost skill) ++ classes skill }

-- | Replaces an empty string with a 'name'.
defaultName :: Text -> Skill -> Text
defaultName ""   skill = name skill
defaultName name _     = name

-- | Generates a 'Key' used for 'Game.Ninja.cooldowns' and 'Game.Ninja.charges'.
key :: Skill -> Key
key x = Key (name x) $ owner x
