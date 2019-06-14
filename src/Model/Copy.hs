module Model.Copy
  ( Copy(..)
  , Copying(..)
  , maxDur
  , source
  ) where
import Model.Internal (Copy(..), Copying(..))

import ClassyPrelude.Yesod

import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)

-- | Finds the value with lesser magnitude.
absmin :: ∀ a. (Ord a, Num a) => a -> a -> a
absmin _ 0 = 0
absmin x y
  | abs x <= abs y = x
  | otherwise = y

-- | Maximum duration of an effect.
-- Effects from 'Copy' 'Skill's must not last longer than the 'copyDuration'.
maxDur :: Copying -> Int -> Int
maxDur (Shallow _ d) = absmin d
maxDur (Deep    _ d) = absmin d
maxDur  NotCopied    = id

-- | 'Skill' owner. Determines the folder location of the icon image.
source :: Skill -> Slot -> Slot
source = cp . Skill.copying
  where
    cp (Shallow a _) = const a
    cp (Deep    a _) = const a
    cp NotCopied     = id
