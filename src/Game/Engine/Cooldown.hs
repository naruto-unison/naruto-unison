-- 'Ninja.cooldowns' processing.
module Game.Engine.Cooldown
  ( active
  , spendCharge, update
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import           Data.List.NonEmpty ((!!))
import qualified Data.Sequence as Seq

import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Skills as Skills
import qualified Game.Model.Character as Character
import           Game.Model.Copy (Copy(Copy), Copying(..))
import qualified Game.Model.Copy as Copy
import           Game.Model.Duration (sync)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Util ((!?), adjustWith)

-- | Cooldowns of the currently active 'Skill's in all four slots of
-- 'Ninja.variants'.
active :: Ninja -> Seq Int
active n = zipWith4 copyCd
                    (Ninja.copies n)
                    (Ninja.alternates n)
                    (Ninja.cooldowns n)
                    (fromList . toList . Character.skills $ Ninja.character n)
  where
    copyCd (Just Copy{skill = Skill{copying = Shallow{}}}) _ _ _ = 0
    copyCd _ alt cd skills
      | Skill.varicd $ skills !! alt = fromMaybe 0 $ cd !? alt
      | otherwise                    = fromMaybe 0 $ headMay cd

-- Safely adjusts 'Ninja.cooldowns'.
adjust :: Int -- ^ 'Skill' index (0-3).
       -> Int -- ^ Alternate index in 'Character.skills' of 'Ninja.character'.
       -> (Int -> Int) -- ^ Adjustment function.
       -> Seq (Seq Int) -> Seq (Seq Int)
adjust s alt f = Seq.adjust' (adjustWith 0 f alt) s

-- | Adds to an element in 'Ninja.cooldowns'.
alter :: Int -- ^ Skill index (0-3)
      -> Int -- ^ Alternate index in 'Character.skills' of 'Ninja.character'.
      -> Int -- ^ Amount added
      -> Ninja -> Ninja
alter s alt cd n =
    n { Ninja.cooldowns = adjust s alt (+ cd) $ Ninja.cooldowns n }

-- | Safely inserts an element into 'Ninja.cooldowns'.
insert :: Int -- ^ 'Skill' index (0-3).
       -> Int -- ^ Alternate index in 'Character.skills' of 'Ninja.character'.
       -> Int -- ^ New cooldown.
       -> Seq (Seq Int)
       -> Seq (Seq Int)
insert s alt toCd = adjust s alt $ const toCd

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Either Int Skill -> Ninja -> Ninja
update Skill{copying = Shallow{}} _ n = n
update _     (Right _) n = n
update skill (Left s)  n =
    n { Ninja.cooldowns = insert s alt cd' $ Ninja.cooldowns n }
  where
    cd' = sync (Skill.cooldown skill) + 2 + 2 * Effects.snare n
    alt
      | Skill.varicd $ Ninja.baseSkill s n = Ninja.alternates n `indexEx` s
      | otherwise                          = 0

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
spendCharge :: Skill -> Either Int Skill -> Ninja -> Ninja
spendCharge Skill{copying = Shallow{}} _ n = n
spendCharge _ (Right _) n = n
spendCharge _ (Left s)  n =
    n { Ninja.charges = adjustWith 0 (+1) s $ Ninja.charges n }

-- | Sets an element in 'Ninja.cooldowns' to 0 by indices.
unsafeReset :: Int -- ^ Skill index (0-3).
            -> Int -- ^ Index in 'Character.skills' of 'Ninja.character'.
            -> Ninja -> Ninja
unsafeReset s alt n =
    n { Ninja.cooldowns = insert s alt 0 $ Ninja.cooldowns n }

-- | Sets an element in 'Ninja.coooldowns' to 0 by name.
reset :: Text -- ^ 'Skill.name' of the base 'Skill'.
      -> Text -- ^ 'Skill.name' of the alternate to search for.
      -> Ninja -> Ninja
reset name alt n = Skills.safe id unsafeReset n name alt n

-- | Sets all 'Ninja.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n = n { Ninja.cooldowns = mempty }
