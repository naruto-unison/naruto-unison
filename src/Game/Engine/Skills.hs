-- | 'Skill.Transform' processing.
module Game.Engine.Skills
  ( change
  , swap
  , targetAll, restrict
  , also
  , addClasses
  , setCooldown, setDur
  , changeWith, changeWithChannel, changeWithDefense
  , changePer
  , extendBy, extendWith
  , setCost
  ) where

import ClassyPrelude hiding (swap)

import Data.Enum.Set (EnumSet)

import qualified Class.TurnBased as TurnBased
import qualified Game.Engine.Effects as Effects
import           Game.Model.Chakras (Chakras)
import           Game.Model.Channel (Channeling(..))
import           Game.Model.Class (Class)
import           Game.Model.Duration (Duration)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (is)
import           Game.Model.Ninja (Ninja(Ninja))
import qualified Game.Model.Ninja as N
import           Game.Model.Runnable (Runnable(To))
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill, Target(..))
import qualified Game.Model.Skill as Skill

-- | Combines two 'Skill.Transform's.
also :: Skill.Transform -> Skill.Transform -> Skill.Transform
(f `also` g) n = g n . f n

-- | Applies a 'Skill.Transform' conditional upon 'N.has'.
changeWith :: Text -> (Skill -> Skill) -> Skill.Transform
changeWith name f n@Ninja{slot}
  | N.has name slot n = f
  | otherwise         = id

-- | Applies a 'Skill.Transform' conditional upon 'N.numStacks'.
changePer :: Text -> (Int -> Skill -> Skill) -> Skill.Transform
changePer name f n@Ninja{slot} = f $ N.numStacks name slot n

-- | Applies a 'Skill.Transform' conditional upon 'N.isChanneling'.
changeWithChannel :: Text -> (Skill -> Skill) -> Skill.Transform
changeWithChannel name f n
  | N.isChanneling name n = f
  | otherwise             = id

-- | Applies a 'Skill.Transform' conditional upon 'N.hasDefense'.
changeWithDefense :: Text -> (Skill -> Skill) -> Skill.Transform
changeWithDefense name f n@Ninja{slot}
  | N.hasDefense name slot n = f
  | otherwise                = id

addClasses :: EnumSet Class -> Skill -> Skill
addClasses classes skill =
    skill { Skill.classes = classes ++ Skill.classes skill }

setDur :: Channeling -> Skill -> Skill
setDur dur skill = skill { Skill.dur = dur }

setCooldown :: Duration -> Skill -> Skill
setCooldown cooldown skill = skill { Skill.cooldown = cooldown }

setCost :: Chakras -> Skill -> Skill
setCost cost skill = skill { Skill.cost = cost }

extendBy :: Int -> Skill -> Skill
extendBy n skill = skill { Skill.dur = TurnBased.setDur dur chan }
  where
    chan  = Skill.dur skill
    dur   = TurnBased.getDur chan + fromIntegral n

-- | Multiplies some number of turns by 'N.numStacks' and adds the total to
-- 'Skill.channel'.
extendWith :: Text -> Int -> Skill.Transform
extendWith name i n@Ninja{slot} skill =
    skill { Skill.dur = TurnBased.setDur dur chan }
  where
    chan  = Skill.dur skill
    added = fromIntegral $ i * N.numStacks name slot n
    dur   = TurnBased.getDur chan + added

-- | Applies a transformation to 'Skill.effects', 'Skill.start', and
-- 'Skill.end'.
changeEffects :: ([Runnable Target] -> [Runnable Target]) -> Skill -> Skill
changeEffects f skill =
    skill { Skill.effects   = f $ Skill.effects skill
          , Skill.start     = f $ Skill.start skill
          , Skill.stunned   = f $ Skill.stunned skill
          , Skill.end       = f $ Skill.end skill
          }

-- | Modifies a 'Skill' by its 'Skill.change' and any other effects on it.
change :: Skill.Transform
change n sk =
    sk' { Skill.cost = Effects.exhaust (Skill.classes sk') n ++ Skill.cost sk' }
  where
    prestrict = Skill.chakraClasses $ Skill.changes sk n sk
    sk'
      | n `is` Restrict = restrict prestrict
      | otherwise       = prestrict

-- | Turns AoE effects into single-target effects.
restrict :: Skill -> Skill
restrict = changeEffects $ mapMaybe f
  where
    f (To XEnemies _)  = Nothing
    f (To Everyone ef) = Just $ To Allies ef
    f (To Enemies  ef) = Just $ To Enemy ef
    f x                  = Just x

-- | Turns single-target effects into AoE effects.
targetAll :: Skill -> Skill
targetAll = changeEffects (Runnable.retarget f <$>)
  where
    f Enemy = Enemies
    f Ally  = Allies
    f XAlly = XAllies
    f x     = x

-- | Affects enemies instead of allies and allies instead of enemies.
swap :: Skill -> Skill
swap = changeEffects (Runnable.retarget f <$>)
  where
    f Self     = Self
    f Ally     = REnemy
    f XAlly    = REnemy
    f Allies   = Enemies
    f XAllies  = Enemies
    f RAlly    = REnemy
    f RXAlly   = REnemy
    f Enemy    = Self
    f REnemy   = RAlly
    f Enemies  = Allies
    f XEnemies = XAllies
    f Everyone = Everyone
