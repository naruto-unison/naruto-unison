module FFI.Progress (progress) where

import Prelude

import Control.Monad.Eff  (Eff, kind Effect)
import Data.Time.Duration (Milliseconds)
import DOM                (DOM)

foreign import progress ∷ ∀ e. Milliseconds → Int → Int → Eff (dom ∷ DOM | e) Unit
