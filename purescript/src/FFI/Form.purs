module FFI.Form (getForm) where

import StandardLibrary

foreign import getForm_ :: ∀ a. (Map String String -> a) -> a -> String 
                        -> Effect a

getForm :: String -> Effect (Maybe (Map String String))
getForm = getForm_ Just Nothing
