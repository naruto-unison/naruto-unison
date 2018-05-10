module FFI.Import 
    ( avatars
    , bg
    , cs
    , cs'
    , getPageSize
    , hostname
    , user
    , userTeam
    , reload
    ) where

import Prelude

import Control.Monad.Eff           (Eff)
import Data.Argonaut.Core          (Json)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either                 (Either(..))
import Data.Maybe                  (Maybe(..))
import Data.Nullable               (Nullable, toMaybe)
import Data.StrMap                 (StrMap)
import DOM                         (DOM)

import Structure (Character, User)

foreign import avatars     ∷ Array String
foreign import bg          ∷ String
foreign import cs          ∷ StrMap Character
foreign import cs'         ∷ Array Character
foreign import userTeam    ∷ Array Character
foreign import user'       ∷ Nullable Json
foreign import hostname    ∷ String
foreign import reload      ∷ ∀ e. Eff (dom ∷ DOM | e) Unit
foreign import getPageSize ∷ ∀ e. Eff (dom ∷ DOM | e) Int

user ∷ Maybe User
user = parseUser =<< toMaybe user'
  where parseUser a = case decodeJson a of
                        Left _  → Nothing
                        Right u → Just u
