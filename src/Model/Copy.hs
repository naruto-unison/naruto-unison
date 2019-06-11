module Model.Copy
  ( Copy(..)
  , Copying(..)
  , maxDur
  , root
  ) where
import Model.Internal (Copy(..), Copying(..))

import ClassyPrelude.Yesod

import           Core.Util (absmin)
import qualified Model.Skill as Skill
import           Model.Skill (Skill)
import           Model.Slot (Slot)

-- | Maximum duration of an effect.
-- Effects from 'Copy' 'Skill's must not last longer than the 'copyDuration'.
maxDur :: Copying -> Int -> Int
maxDur (Shallow _ d) = absmin d
maxDur (Deep    _ d) = absmin d
maxDur  NotCopied    = id

-- | 'Skill' owner. Determines the folder location of the icon image.
root :: Skill -> Slot -> Slot
root = cp . Skill.copying
  where
    cp (Shallow a _) = const a
    cp (Deep    a _) = const a
    cp NotCopied     = id
