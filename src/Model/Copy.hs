module Model.Copy
  ( Copy(..)
  , Copying(..)
  , maxDur
  , source
  ) where
import Model.Internal (Copy(..), Copying(..))

import ClassyPrelude

import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)

-- | Finds the value with lesser magnitude.
absmin :: ∀ a. (Ord a, Num a) => a -> a -> a
absmin x 0 = x
absmin 0 y = y
absmin x y
  | abs x <= abs y = x
  | otherwise      = y

-- | Maximum duration of an effect.
-- Effects from 'Copy' 'Skill's must not last longer than the 'Copy' 'dur'.
maxDur :: Copying -> Int -> Int
maxDur (Shallow _ d) = absmin d
maxDur (Deep    _ d) = absmin d
maxDur  NotCopied    = id

-- | @Skill@ owner. Determines the folder location of the icon image.
source :: Skill -> Slot -> Slot
source skill slot = case Skill.copying skill of
    NotCopied   -> slot
    Shallow a _ -> a
    Deep    a _ -> a
