-- 'Ninja.cooldowns' processing.
module Game.Engine.Cooldown
  ( active
  , spendCharge, update
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import qualified Data.Sequence as Seq

import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Skills as Skills
import           Game.Model.Copy (Copy(Copy), Copying(..))
import qualified Game.Model.Copy as Copy
import           Game.Model.Duration (sync)
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import qualified Game.Model.Variant as Variant
import           Util ((!!), (!?), adjustWith)

-- | Cooldowns of the currently active 'Skill's in all four slots of
-- 'Ninja.variants'.
active :: Ninja -> Seq Int
active n = [copyCd copy vari cd | copy <- Ninja.copies n
                                | vari <- head <$> Ninja.variants n
                                | cd   <- Ninja.cooldowns n
                                ]
  where
    copyCd (Just Copy{skill = Skill{copying = Shallow{}}}) _ _ = 0
    copyCd _ vari cd = fromMaybe 0 $ cd !? Variant.cooldown vari

-- Safely adjusts 'Ninja.cooldowns'.
adjust :: Int -- ^ 'Skill' index (0-3).
       -> Int -- ^ 'Variant.Variant' index in 'Character.skills' of 'Ninja.character'.
       -> (Int -> Int) -- ^ Adjustment function.
       -> Seq (Seq Int) -> Seq (Seq Int)
adjust s v f = Seq.adjust' (adjustWith 0 f v) s

-- | Adds to an element in 'Ninja.cooldowns'.
alter :: Int -- ^ Skill index (0-3)
      -> Int -- ^ 'Variant.Variant' index in 'Character.skills' of 'Ninja.character'.
      -> Int -- ^ Amount added
      -> Ninja -> Ninja
alter s v cd n = n { Ninja.cooldowns = adjust s v (+ cd) $ Ninja.cooldowns n }

-- | Safely inserts an element into 'Ninja.cooldowns'.
insert :: Int -- ^ 'Skill' index (0-3).
       -> Int -- ^ 'Variant.Variant' index in 'Character.skills' of 'Ninja.character'.
       -> Int -- ^ New cooldown.
       -> Seq (Seq Int)
       -> Seq (Seq Int)
insert s v toCd = adjust s v $ const toCd

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
update :: Skill -> Either Int Skill -> Ninja -> Ninja
update Skill{copying = Shallow{}} _ n = n
update _     (Right _) n = n
update skill (Left s)  n =
    n { Ninja.cooldowns = insert s vari cd' $ Ninja.cooldowns n }
  where
    cd' = sync (Skill.cooldown skill) + 2 + 2 * Effects.snare n
    vari = Variant.cooldown . head $ Ninja.variants n !! s

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
unsafeReset s v n =
    n { Ninja.cooldowns = insert s v 0 $ Ninja.cooldowns n }

-- | Sets an element in 'Ninja.coooldowns' to 0 by name.
reset :: Text -- ^ 'Skill.name' of the base 'Skill'.
      -> Text -- ^ 'Skill.name' of the variant to search for.
      -> Ninja -> Ninja
reset name v n = Skills.safe id unsafeReset n name v n

-- | Sets all 'Ninja.cooldowns' to @mempty@.
resetAll :: Ninja -> Ninja
resetAll n = n { Ninja.cooldowns = mempty }
