module Game.Model.Copy
  ( Copy(..)
  , Copying(..)
  , maxDur
  , source
  ) where
import Game.Model.Internal (Copy(..), Copying(..))

import ClassyPrelude

import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill)
import           Game.Model.Slot (Slot)

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
maxDur (Shallow _ d) i = absmin d i
maxDur (Deep    _ d) i = absmin d i
maxDur  NotCopied    i = i

-- | @Skill@ owner. Determines the folder location of the icon image.
source :: Skill -> Slot -> Slot
source skill slot = case Skill.copying skill of
    NotCopied   -> slot
    Shallow a _ -> a
    Deep    a _ -> a
