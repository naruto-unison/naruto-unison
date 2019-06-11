module FFI.Form (getForm) where

import Prelude ((<<<), map)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import getForm_ :: ∀ a. ((String -> Maybe String) -> a) -> a -> String
                        -> Effect a

getForm :: String -> Effect (Maybe Form)
getForm = map (map makeForm) <<< getForm_ Just Nothing

type Form = { lookup :: String -> Maybe String }

makeForm :: (String -> Maybe String) -> Form
makeForm lookup = { lookup }
