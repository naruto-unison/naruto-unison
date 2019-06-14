-- 'Ninja.cooldowns' processing.
module Engine.Cooldown
  ( active
  , updateGame, update
  , alter
  , insert
  , reset, resetAll
  ) where

import ClassyPrelude.Yesod hiding (head, insert, update)
import           Data.List.NonEmpty (head)
import qualified Data.Sequence as Seq
import           Data.Sequence ((|>))

import qualified Model.Copy as Copy
import           Model.Duration (sync)
import qualified Model.Game as Game
import           Model.Game (Game)
import qualified Model.Ninja as Ninja
import           Model.Ninja (Ninja)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)
import qualified Model.Variant as Variant
import qualified Engine.Effects as Effects
import qualified Engine.SkillTransform as SkillTransform

-- | Cooldowns of the currently active 'Skill's in all four slots of 
-- 'Ninja.variants'.
active :: Ninja -> Seq Int
active n = zipWith copyCd (Ninja.copies n) .
           zipWith display (head <$> Ninja.variants n) $
           Ninja.cooldowns n
  where
    display = (fromMaybe 0 .) . Seq.lookup . Variant.cooldown
    isShallow Copy.Shallow{} = True
    isShallow _              = False
    copyCd (Just copied)
        | isShallow . Skill.copying $ Copy.skill copied = const 0
    copyCd _ = id

-- Safely adjusts a row in 'Ninja.cooldowns' by appending to it if incomplete.
adjust' :: Int -> (Int -> Int) -> Seq Int -> Seq Int
adjust' v f cds
  | len > v   =  Seq.adjust' f v cds
  | otherwise = (cds ++ replicate (v - len) 0) |> f 0
  where
    len = length cds

-- Safely adjusts 'Ninja.cooldowns' by appending to it if incomplete.
adjust :: Int -- ^ 'Skill' index (0-3).
       -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'.
       -> (Int -> Int) -- ^ Adjustment function.
       -> Seq (Seq Int) -> Seq (Seq Int)
adjust s v f cds
  | len > s   = Seq.adjust' (adjust' v f) s cds
  | otherwise = (cds ++ replicate (s - len) (singleton 0))
                |> adjust' v f mempty
  where
    len = length cds

-- | Adds to an element in 'cooldowns'.
alter :: Int -- ^ Skill index (0-3)
      -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'
      -> Int -- ^ Amount added
      -> Ninja -> Ninja
alter s v cd n = n { Ninja.cooldowns = adjust s v (+ cd) $ Ninja.cooldowns n }

-- | Safely inserts an element into a row of 'Ninja.cooldowns' by appending to
-- it if incomplete.
insert' :: Int -> Int -> Seq Int -> Seq Int
insert' v toCd cds
  | len > v   = Seq.update v toCd cds
  | otherwise = (cds ++ replicate (v - len) 0) |> toCd
  where
    len = length cds

-- | Safely inserts an element into 'Ninja.cooldowns' by appending to it if
-- incomplete.
insert :: Int -- ^ 'Skill' index (0-3).
       -> Int -- ^ 'Variant' index in 'characterSkills' of 'nCharacter'.
       -> Int -- ^ New cooldown.
       -> Seq (Seq Int)
       -> Seq (Seq Int)
insert s v toCd cds
  | len > s   = Seq.adjust' (insert' v toCd) s cds
  | otherwise = (cds ++ replicate (s - len) (singleton 0))
                |> insert' v toCd mempty
  where
    len = length cds

-- | Updates an element in 'Ninja.cooldowns'. 
-- If 'True', also increments 'Ninja.charges'.
update :: Bool -> Int -> Skill -> Int -> Ninja -> Ninja
update True a skill s n =
    (update False a skill s n)
    { Ninja.charges = Seq.adjust' (+ 1) s $ Ninja.charges n }
update False a skill s n
   | copied $ Skill.copying skill = n
   | Skill.cooldown skill == 0    = n
   | otherwise = n { Ninja.cooldowns = insert s vari cd' $ Ninja.cooldowns n }
  where
    cd'  = sync (Skill.cooldown skill) + 2 + 2 * a
    vari = Variant.cooldown . head $ Ninja.variants n `Seq.index` s
    copied Copy.NotCopied = False
    copied Copy.Shallow{} = True
    copied Copy.Deep{}    = False

-- | 'update's a corresponding 'Ninja' when they use a new 'Skill'.
updateGame :: Bool -> Skill -> Slot -> Either Int Skill -> Game -> Game
updateGame _      _     _    (Right _) = id
updateGame charge skill user (Left s)  =
    Game.adjust user \n -> update charge (Effects.snare n) skill s n

-- | Sets an element in 'Ninja.cooldowns' to 0 by indices.
unsafeReset :: Int -- ^ Skill index (0-3).
            -> Int -- ^ Index in 'Character.skills' of 'Ninja.character'.
            -> Ninja -> Ninja
unsafeReset s v n =
    n { Ninja.cooldowns = insert s v 0 $ Ninja.cooldowns n }

-- | Sets an element in 'Ninja.coooldowns' to 0 by name.
reset :: Text -- ^ 'Skill.name' of the base 'Skill'.
      -> Text -- ^ 'Skill.name' of the variant to search for.
      -> Ninja -> Ninja
reset name v n = safe n name v n
  where
    safe = SkillTransform.safe id unsafeReset

-- | Sets all 'Ninja.cooldowns' to 'mempty'.
resetAll :: Ninja -> Ninja
resetAll n = n { Ninja.cooldowns = mempty }
