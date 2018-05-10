module FFI.Form (getForm) where

import Control.Monad.Eff       (Eff)
import Data.Maybe              (Maybe(..))
import Data.StrMap             (StrMap)
import DOM                     (DOM)

foreign import getForm_ ∷ ∀ a e. (StrMap String → a) → a → String 
                        → Eff (dom ∷ DOM | e) a

getForm ∷ ∀ e. String → Eff (dom ∷ DOM | e) (Maybe (StrMap String))
getForm = getForm_ Just Nothing
