-- | 'Skill.Transform' processing.
module Engine.Skills
  ( change
  , safe
  , swap
  , targetAll
  , also
  , changeWith, changeWithChannel, changeWithDefense
  , extendWith
  , costPer, reduceCostPer
  ) where

import ClassyPrelude hiding (swap)

import Data.List (findIndex)
import Data.List.NonEmpty ((!!), NonEmpty(..))

import           Class.TurnBased as TurnBased
import           Model.Chakra (Chakras)
import qualified Model.Character as Character
import           Model.Effect (Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja, is)
import qualified Model.Runnable as Runnable
import           Model.Runnable (Runnable(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))
import qualified Engine.Effects as Effects

-- | Converts a function that uses raw 'Int's as indices in a
-- 'Character.Character''s @[[Skill]]@ list into one that searches by name.
-- Passing an empty string for the second argument will select the base (0)
-- 'Skill' in the lists. Otherwise, the root skill will not be considered.
-- This means that if a skill has a variant with the same name as it, ""
-- selects the base variant, while "<skill name>" selects
-- the identically-named variant.
safe :: ∀ a. a -> (Int -> Int -> a) -> Ninja -> Text -> Text -> a
safe a f n sName vName = fromMaybe a do
    s <- findIndex (any $ (sName ==) . Skill.name) $ toList skills
    v <- case vName of
            "" -> return 0
            _  -> let (_:|xs) = skills !! s
                  in (+1) <$> findIndex ((vName ==) . Skill.name) xs
    return $ f s v
  where
    skills  = Character.skills $ Ninja.character n

-- | Combines two 'Skill.Transform's.
also :: Skill.Transform -> Skill.Transform -> Skill.Transform
(f `also` g) n skill = g n $ f n skill

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
    added = i * Ninja.numActive name n
    dur   = TurnBased.getDur chan + added

-- | Applies a transformation to 'Skill.effects', 'Skill.start', and
-- 'Skill.interrupt'.
changeEffects :: ([Runnable Target] -> [Runnable Target]) -> Skill -> Skill
changeEffects f skill = skill { Skill.effects   = f $ Skill.effects skill
                              , Skill.start     = f $ Skill.start skill
                              , Skill.interrupt = f $ Skill.interrupt skill
                              }

-- | Modifies a 'Skill' by its 'Skill.change' and any other effects on it.
change :: Skill.Transform
change n sk =
    sk' { Skill.cost = Effects.exhaust (Skill.classes sk') n + Skill.cost sk' }
  where
    mRestrict
      | n `is` Restrict = restrict
      | otherwise       = id
    sk' = mRestrict . Skill.chakraClasses $ Skill.changes sk n sk

-- | Turns AoE effects into single-target effects.
restrict :: Skill -> Skill
restrict = changeEffects $ mapMaybe f
  where
    f (To XEnemies _)  = Nothing
    f (To REnemy   _)  = Nothing
    f (To Everyone ef) = Just $ To Allies ef
    f (To Enemies  ef) = Just $ To Enemy ef
    f x                  = Just x

-- | Turns single-target effects into AoE effects.
targetAll :: Skill -> Skill
targetAll = changeEffects . map $ Runnable.retarget f
  where
    f Enemy = Enemies
    f Ally  = Allies
    f XAlly = XAllies
    f x     = x

-- | Affects enemies instead of allies and allies instead of enemies.
swap :: Skill -> Skill
swap = changeEffects . map $ Runnable.retarget f
  where
    f Self         = Self
    f Ally         = REnemy
    f XAlly        = REnemy
    f RAlly        = REnemy
    f Allies       = Enemies
    f XAllies      = Enemies
    f Enemy        = Self
    f REnemy       = RAlly
    f Enemies      = Allies
    f XEnemies     = XAllies
    f Everyone     = Everyone
