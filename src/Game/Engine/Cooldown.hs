-- 'Ninja.cooldowns' processing.
module Game.Engine.Cooldown
  ( active
  , update, updateN
  , alter
  , reset, resetAll
  ) where

import ClassyPrelude

import qualified Data.Sequence as Seq

import           Util ((!!), (!?), adjustWith)
import qualified Game.Model.Copy as Copy
import           Game.Model.Duration (sync)
import qualified Game.Model.Ninja as Ninja
import           Game.Model.Ninja (Ninja)
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill)
import qualified Game.Model.Variant as Variant
import qualified Game.Engine.Effects as Effects
import qualified Game.Engine.Skills as Skills

-- | Cooldowns of the currently active 'Skill's in all four slots of
-- 'Ninja.variants'.
active :: Ninja -> Seq Int
active n = [copyCd copy vari cd | copy <- Ninja.copies n
                                | vari <- head <$> Ninja.variants n
                                | cd   <- Ninja.cooldowns n
                                ]
  where
    isShallow Copy.Shallow{} = True
    isShallow _              = False
    copyCd (Just copied) _ _
        | isShallow . Skill.copying $ Copy.skill copied = 0
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

-- | Updates an element in 'Ninja.cooldowns'.
-- If 'True', also increments 'Ninja.charges'.
update :: Bool -> Int -> Skill -> Int -> Ninja -> Ninja
update alsoCharge a skill s n
   | copied $ Skill.copying skill = n
   | Skill.cooldown skill == 0    = n
   | otherwise = n { Ninja.cooldowns = insert s vari cd' $ Ninja.cooldowns n
                   , Ninja.charges   = charges $ Ninja.charges n
                   }
  where
    cd'  = sync (Skill.cooldown skill) + 2 + 2 * a
    vari = Variant.cooldown . head $ Ninja.variants n !! s
    copied Copy.NotCopied = False
    copied Copy.Shallow{} = True
    copied Copy.Deep{}    = False
    charges
      | alsoCharge = adjustWith 0 (+1) s
      | otherwise  = id

-- | 'update's a corresponding @Ninja@ when they use a new @Skill@.
updateN :: Bool -> Skill -> Either Int Skill -> Ninja -> Ninja
updateN _      _     (Right _) n = n
updateN charge skill (Left s)  n = update charge (Effects.snare n) skill s n

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
