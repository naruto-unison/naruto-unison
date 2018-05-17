module FFI.Import 
    ( avatars
    , bg
    , cs
    , cs'
    , getPageSize
    , groupCs
    , groupCs'
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
import Data.NonEmpty               (NonEmpty, (:|))
import Data.Nullable               (Nullable, toMaybe)
import Data.StrMap                 (StrMap, fromFoldable)
import Data.Tuple                  (Tuple(..))
import DOM                         (DOM)

import Operators
import Functions

import Structure (Character(..), User)

foreign import avatars     ∷ Array String
foreign import bg          ∷ String
foreign import cs'         ∷ Array Character
foreign import userTeam    ∷ Array Character
foreign import user'       ∷ Nullable Json
foreign import hostname    ∷ String
foreign import reload      ∷ ∀ e. Eff (dom ∷ DOM | e) Unit
foreign import getPageSize ∷ ∀ e. Eff (dom ∷ DOM | e) Int

cs ∷ StrMap Character
cs = fromFoldable $ makeKey ↤ cs'
  where makeKey c@Character{characterName} = Tuple characterName c

groupCs' ∷ Array (NonEmpty Array Character)
groupCs' = groupBy' (eqs shortName) cs'

groupCs ∷ StrMap (NonEmpty Array Character)
groupCs = fromFoldable $ makeKey ↤ groupCs'
  where makeKey xs@(x :| _) = Tuple (shortName x) xs

user ∷ Maybe User
user = parseUser =<< toMaybe user'
  where parseUser a = case decodeJson a of
                        Left _  → Nothing
                        Right u → Just u
