-- | 'Skill.Transform' processing.
module Game.Engine.Skills
  ( change
  , swap
  , targetAll
  , also
  , changeWith, changeWithChannel, changeWithDefense
  , extendWith
  , costPer, reduceCostPer
  ) where

import ClassyPrelude hiding (swap)

import           Class.TurnBased as TurnBased
import qualified Game.Engine.Effects as Effects
import           Game.Model.Chakra (Chakras)
import           Game.Model.Effect (Effect(..))
import           Game.Model.Ninja (is)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Runnable (Runnable(..))
import qualified Game.Model.Runnable as Runnable
import           Game.Model.Skill (Skill, Target(..))
import qualified Game.Model.Skill as Skill

-- | Combines two 'Skill.Transform's.
also :: Skill.Transform -> Skill.Transform -> Skill.Transform
(f `also` g) n = g n . f n

-- | Applies a 'Skill.Transform' conditional upon 'Ninja.has'.
changeWith :: Text -> (Skill -> Skill) -> Skill.Transform
changeWith name f n
  | Ninja.has name (Ninja.slot n) n = f
  | otherwise                       = id

-- | Applies a 'Skill.Transform' conditional upon 'Ninja.isChanneling'.
changeWithChannel :: Text -> (Skill -> Skill) -> Skill.Transform
changeWithChannel name f n
  | Ninja.isChanneling name n = f
  | otherwise                 = id

-- | Applies a 'Skill.Transform' conditional upon 'Ninja.hasDefense'.
changeWithDefense :: Text -> (Skill -> Skill) -> Skill.Transform
changeWithDefense name f n
  | Ninja.hasDefense name (Ninja.slot n) n = f
  | otherwise                              = id

-- | Multiplies @Chakras@ by 'Ninja.numActive' and adds the total to
-- 'Skill.cost'.
costPer :: Text -> Chakras -> Skill.Transform
costPer name chaks n skill = skill { Skill.cost = Skill.cost skill + added }
  where
    added = chaks * fromIntegral (Ninja.numActive name n)

-- | Multiplies @Chakras@ by 'Ninja.numActive' and subtracts the total from
-- 'Skill.cost'.
reduceCostPer :: Text -> Chakras -> Skill.Transform
reduceCostPer name chaks n skill =
    skill { Skill.cost = Skill.cost skill - added }
  where
    added = chaks * fromIntegral (Ninja.numActive name n)

-- | Multiplies some number of turns by 'Ninja.numActive' and adds the total to
-- 'Skill.channel'.
extendWith :: Text -> Int -> Skill.Transform
extendWith name i n skill = skill { Skill.dur = TurnBased.setDur dur chan }
  where
    chan  = Skill.dur skill
    added = fromIntegral $ i * Ninja.numActive name n
    dur   = TurnBased.getDur chan + added

-- | Applies a transformation to 'Skill.effects', 'Skill.start', and
-- 'Skill.interrupt'.
changeEffects :: ([Runnable Target] -> [Runnable Target]) -> Skill -> Skill
changeEffects f skill = skill { Skill.effects   = f $ Skill.effects skill
                              , Skill.start     = f $ Skill.start skill
                              , Skill.interrupt = f $ Skill.interrupt skill
                              , Skill.stunned   = f $ Skill.stunned skill
                              }

-- | Modifies a 'Skill' by its 'Skill.change' and any other effects on it.
change :: Skill.Transform
change n sk =
    sk' { Skill.cost = Effects.exhaust (Skill.classes sk') n + Skill.cost sk' }
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
