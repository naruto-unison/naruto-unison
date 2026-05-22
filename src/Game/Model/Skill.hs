module Game.Model.Skill
  ( Skill(..), new, chakraClasses
  , Target(..)
  , Key(..), key
  , Transform
  , defaultName
  , targets
  ) where

import ClassyPrelude

import Data.Enum.Set (EnumSet)

import qualified Game.Model.Chakras as Chakras
import           Game.Model.Class (Class(..))
import           Game.Model.Internal (Channeling(..), Key(..), Ninja, Skill(..), Requirement(..), Runnable(To), Target(..))
import qualified Game.Model.Slot as Slot

-- | The type signature of 'changes'.
type Transform = (Ninja -> Skill -> Skill)

-- | Default values.
new :: Skill
new = Skill
    { name      = "Unnamed"
    , desc      = ""
    , require   = Usable
    , classes   = singletonSet All
    , cost      = mempty
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
chakraClasses skill@Skill{classes, cost} =
    skill { classes = Chakras.classes cost ++ classes }

-- | Replaces an empty string with a 'name'.
defaultName :: Text -> Skill -> Text
defaultName ""   Skill{name} = name
defaultName name _           = name

-- | Generates a 'Key' used for 'Game.Ninja.cooldowns' and 'Game.Ninja.charges'.
key :: Skill -> Key
key Skill{name, owner} = Key name owner

-- | All targets that a @Skill@ effects.
targets :: Skill -> EnumSet Target
targets Skill{effects, start} = addTargets (addTargets mempty start) effects
  where
    addTargets = foldl' \acc (To target _) -> insertSet target acc
