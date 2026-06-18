module Game.Model.Skill
  ( Skill(..), new, chakraClasses
  , hasCharges, hasCooldown
  , Target(..)
  , Key(..), key
  , targets
  , provideName
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

import           Class.Classed (Classed(..))
import           Class.Display (Display(..), buildStrict)
import           Game.Model.Chakras (Chakras)
import           Game.Model.Class (Class(..))
import           Game.Model.Duration (Duration)
import           Game.Model.Internal (Channeling(..), Key(..), Skill(..), Runnable(To), Target(..))
import           Game.Model.Internal.Skill (key)
import qualified Game.Model.Runnable as Runnable
import qualified Game.Model.Slot as Slot
import           Util ((∈))

-- | Default values.
new :: Skill
new = Skill
    { name      = "Unnamed"
    , desc      = ""
    , require   = mempty
    , classes   = singleton All
    , cost      = mempty
    , cooldown  = 0
    , charges   = 0
    , dur       = Instant
    , start     = mempty
    , always    = mempty
    , effects   = mempty
    , end       = mempty
    , changes   = const id
    , owner     = unsafeHead Slot.all
    }

hasCharges :: Skill -> Bool
hasCharges Skill{charges = 0} = False
hasCharges _                  = True

hasCooldown :: Skill -> Bool
hasCooldown Skill{cooldown = 0} = False
hasCooldown _                   = True

-- | Adds 'Model.Class.Bloodline', 'Model.Class.Genjutsu',
-- 'Model.Class.Ninjutsu', 'Model.Class.Taijutsu', and 'Model.Class.Random'
-- to the 'classes' of a @Skill@ if they are included in its 'cost'.
chakraClasses :: Skill -> Skill
chakraClasses skill = skill { classes = getClasses skill.cost ++ skill.classes }

-- | All targets that a @Skill@ effects.
targets :: Skill -> EnumSet Target
targets Skill{effects, start} = addTargets (addTargets mempty start) effects
  where
    addTargets = foldl' \acc (To target _) -> insertSet target acc

provideName :: Skill -> Text -> Text
provideName Skill{name} nameOrEmpty
  | null nameOrEmpty = name
  | otherwise        = nameOrEmpty

-- Mutators

changeEffects :: ([Runnable Target] -> [Runnable Target]) -> Skill -> Skill
changeEffects f skill =
    skill { always  = f skill.always
          , effects = f skill.effects
          , start   = f skill.start
          , end     = f skill.end
          }

retarget :: (Target -> Target) -> Skill -> Skill
retarget f = changeEffects (Runnable.retarget f <$>)

addClass :: Class -> Skill -> Skill
addClass cla skill = skill { classes = insertSet cla skill.classes }

addClasses :: EnumSet Class -> Skill -> Skill
addClasses classes skill = skill { classes = classes ++ skill.classes }

removeClass :: Class -> Skill -> Skill
removeClass cla skill = skill { classes = deleteSet cla skill.classes }

addDesc :: TextBuilder -> Skill -> Skill
addDesc add skill = skill { desc = buildStrict $ display skill.desc ++ add }

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
swap skill@Skill{classes}
  | Unreflectable ∈ classes = skill
  | otherwise               = retarget f skill
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
