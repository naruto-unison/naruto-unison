module Game.Model.Skill
  ( Skill(..), new, chakraClasses
  , Target(..)
  , Key(..), key
  , defaultName
  , targets
  -- Mutators
  , addClass, addClasses, removeClass
  , addDesc
  , setCooldown
  , setCost
  , setDur
  , restrict, swap, targetAll
  ) where

import ClassyPrelude hiding (swap)

import Data.Enum.Set (EnumSet)

import           Class.Display (Display(..), buildStrict)
import           Game.Model.Chakras (Chakras)
import qualified Game.Model.Chakras as Chakras
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration)
import           Game.Model.Internal (Channeling(..), Key(..), Skill(..), Requirement(..), Runnable(To), Target(..))
import           Game.Model.Internal.Skill (key)
import qualified Game.Model.Runnable as Runnable
import qualified Game.Model.Slot as Slot

-- | Default values.
new :: Skill
new = Skill
    { name      = "Unnamed"
    , desc      = ""
    , require   = Usable
    , classes   = singleton All
    , cost      = mempty
    , cooldown  = 0
    , charges   = 0
    , dur       = Instant
    , start     = []
    , effects   = []
    , stunned   = []
    , end       = []
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

-- | All targets that a @Skill@ effects.
targets :: Skill -> EnumSet Target
targets Skill{effects, start} = addTargets (addTargets mempty start) effects
  where
    addTargets = foldl' \acc (To target _) -> insertSet target acc

-- Mutators

changeEffects :: ([Runnable Target] -> [Runnable Target]) -> Skill -> Skill
changeEffects f skill@Skill{effects, start, stunned, end} =
    skill { effects   = f effects
          , start     = f start
          , stunned   = f stunned
          , end       = f end
          }

retarget :: (Target -> Target) -> Skill -> Skill
retarget f = changeEffects (Runnable.retarget f <$>)

addClass :: Class -> Skill -> Skill
addClass class' skill@Skill{classes} =
    skill { classes = insertSet class' classes }

addClasses :: EnumSet Class -> Skill -> Skill
addClasses classes skill@Skill{classes = classes'} =
    skill { classes = classes ++ classes' }

removeClass :: Class -> Skill -> Skill
removeClass class' skill@Skill{classes} =
    skill { classes = deleteSet class' classes }

addDesc :: TextBuilder -> Skill -> Skill
addDesc add skill@Skill{desc} =
    skill { desc = buildStrict $ display desc ++ add }

setCooldown :: Duration -> Skill -> Skill
setCooldown cooldown skill = skill { cooldown = cooldown }

setCost :: Chakras -> Skill -> Skill
setCost cost skill = skill { cost = cost }

setDur :: Channeling -> Skill -> Skill
setDur dur skill = skill { dur = dur }

-- | Turns AoE effects into single-target effects.
restrict :: Skill -> Skill
restrict = changeEffects $ mapMaybe f
  where
    f (To XEnemies _)  = Nothing
    f (To Everyone ef) = Just $ To Allies ef
    f (To Enemies  ef) = Just $ To Enemy ef
    f x                  = Just x

-- | Affects enemies instead of allies and allies instead of enemies.
swap :: Skill -> Skill
swap = retarget f
  where
    f Self     = Self
    f Ally     = REnemy
    f XAlly    = REnemy
    f Allies   = Enemies
    f XAllies  = Enemies
    f RAlly    = REnemy
    f RXAlly   = REnemy
    f Enemy    = RAlly
    f REnemy   = RAlly
    f Enemies  = Allies
    f XEnemies = XAllies
    f Everyone = Everyone

-- | Turns single-target effects into AoE effects.
targetAll :: Skill -> Skill
targetAll = retarget f
  where
    f Enemy = Enemies
    f Ally  = Allies
    f XAlly = XAllies
    f x     = x
