module FFI.Progress (ANIMATION, progress) where

import Prelude

import Control.Monad.Eff  (Eff, kind Effect)
import Data.Time.Duration (Milliseconds)

foreign import data ANIMATION ∷ Effect

foreign import progress ∷ ∀ e. Milliseconds → Int → Int → Eff (animation ∷ ANIMATION | e) Unit