module Engine.SkillTransform
  ( safe
  , also
  , change, changeWith
  , addClass
  , restrict, targetAll, targetOnly, swap
  , costPer, reduceCostPer
  , setCost
  , extendWith
  ) where

import ClassyPrelude.Yesod hiding (Status, addClass, swap)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (!!))

import           Core.Util ((∈))
import           Class.Play (Play)
import           Class.TurnBased as TurnBased
import qualified Model.Chakra as Chakra
import           Model.Chakra (Chakra)
import qualified Model.Character as Character
import           Model.Class (Class)
import           Model.Effect (Effect(..))
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))
import qualified Model.Status as Status
import           Model.Status (Status)
import qualified Engine.Effects as Effects

-- | Converts a function that uses raw 'Int's as indices in a
-- 'Character''s `[[Skill]]` list into one that searches by name.
-- Passing an empty string for the second argument will select the base (0)
-- 'Skill' in the lists. Otherwise, the root skill will not be considered.
-- This means that if a skill has a variant with the same name as it, ""
-- selects the base variant, while "<skill name>" selects
-- the identically-named variant.
safe :: ∀ a. a -> (Int -> Int -> a) -> Ninja -> Text -> Text -> a
safe a f n sName vName = fromMaybe a do
    s <- List.findIndex (any $ match sName) $ toList skills
    v <- case vName of
            "" -> return 0
            _  -> let (_:|xs) = skills !! s
                  in (+1) <$> List.findIndex (match vName) xs
    return $ f s v
  where
    match x = (== toCaseFold x) . toCaseFold . Skill.name
    skills  = Character.skills $ Ninja.character n


also :: Skill.Transform -> Skill.Transform -> Skill.Transform
(f `also` g) n skill = g n $ f n skill

-- | Applies a 'SkillTransform' if 'hasOwn'.
changeWith :: Text -> Skill.Transform -> Skill.Transform
changeWith name f n
  | Ninja.hasOwn name n || Ninja.hasDefense name (Ninja.slot n) n = f n
  | otherwise                                                     = id

addClass :: Class -> Skill.Transform
addClass cla _ skill = skill { Skill.classes = cla : Skill.classes skill }

setCost :: [Chakra] -> Skill.Transform
setCost chaks _ skill = skill { Skill.cost = Chakra.collect chaks }

costPer :: Text -> [Chakra] -> Skill.Transform
costPer name chaks n skill = skill { Skill.cost = Skill.cost skill + added }
  where
    added = Chakra.collect chaks
            * fromInteger (toInteger $ Ninja.numActive name n)

reduceCostPer :: Text -> [Chakra] -> Skill.Transform
reduceCostPer name chaks n skill =
    skill { Skill.cost = Skill.cost skill - added }
  where
    added = Chakra.collect chaks
            * fromInteger (toInteger $ Ninja.numActive name n)

extendWith :: Text -> Int -> Skill.Transform
extendWith name i n skill = skill { Skill.channel = TurnBased.setDur dur chan }
  where
    chan  = Skill.channel skill
    added = i * Ninja.numActive name n
    dur   = TurnBased.getDur chan + added

changeEffects :: ([(Target, Play ())] -> [(Target, Play ())])
              -> Skill -> Skill
changeEffects f skill = skill { Skill.effects = f $ Skill.effects skill
                              , Skill.start   = f $ Skill.start skill
                              , Skill.disrupt = f $ Skill.disrupt skill
                              }

change :: Skill.Transform
change n sk =
    sk' { Skill.cost = Effects.exhaust (Skill.classes sk') n + Skill.cost sk' }
  where
    sk' = Skill.chakraClasses $ Skill.changes sk n sk

-- | Turns AoE effects into single-target effects.
restrict :: Skill.Transform
restrict n
  | Ninja.is Restrict n = changeEffects $ mapMaybe f
  | otherwise           = id
  where
    f (XEnemies, _)  = Nothing
    f (REnemy,   _)  = Nothing
    f (Everyone, ef) = Just (Allies, ef)
    f (Enemies, ef)  = Just (Enemy, ef)
    f x              = Just x

-- | Turns single-target effects into AoE effects.
targetAll :: Skill.Transform
targetAll = const . changeEffects . map $ first f
  where
    f Enemy = Enemies
    f Ally  = Allies
    f XAlly = XAllies
    f x     = x

-- | Restricts skill effects to a specified list of 'Target's.
targetOnly :: [Target] -> Skill.Transform
targetOnly xs = const . changeEffects . filter $ (∈ xs) . fst

-- | Affects enemies instead of allies and allies instead of enemies.
swap :: Status -> Skill -> Skill
swap st = changeEffects . map $ first f
  where
    f Self         = Self
    f Ally         = Specific $ Status.source st
    f XAlly        = Specific $ Status.source st
    f RAlly        = REnemy
    f Allies       = Enemies
    f XAllies      = Enemies
    f Enemy        = Self
    f REnemy       = RAlly
    f Enemies      = Allies
    f XEnemies     = XAllies
    f Everyone     = Everyone
    f (Specific x) = Specific x
