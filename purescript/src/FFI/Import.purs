module FFI.Import
    ( avatars
    , bg
    , chars
    , getPageSize
    , hostname
    , user
    , userTeam
    , reload
    , getJson
    , unJson
    , csrf
    ) where

import Prelude
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign as Foreign
import Foreign (Foreign)
import Foreign.JSON as JSON
import Data.Either (Either(..))
import Data.List as List
import Data.List.Types as ListTypes
import Data.Maybe (Maybe(..))
import Generic as G
import Partial.Unsafe as Partial

import Data.List.NonEmpty (NonEmptyList)
import Data.Nullable (Nullable, toMaybe)

import Model.Character (Character)
import Model.User (User)

foreign import avatars        :: Array String
foreign import bg             :: String
foreign import fChars         :: Array Foreign
foreign import fUserTeam      :: Array Foreign
foreign import fUser          :: Nullable Foreign
foreign import hostname       :: String
foreign import reload         :: Effect Unit
foreign import getPageSize    :: Effect Int
foreign import csrf           :: String

chars :: Array Character
chars = unsafeImport <$> fChars

userTeam :: Array Character
userTeam = unsafeImport <$> fUserTeam

user :: Maybe User
user = parseUser =<< toMaybe fUser
  where
    parseUser x = case runExcept (G.decode x) of
                      Right u   -> Just u
                      Left err  -> Nothing

unsafeImport :: ∀ a. G.Decode a => Foreign -> a
unsafeImport x = case runExcept (G.decode x) of
    Right u  -> u
    Left err -> Partial.unsafeCrashWith $ showForeignErrors err

showForeignErrors :: NonEmptyList Foreign.ForeignError -> String
showForeignErrors = String.joinWith "\n" <<<
                    (Foreign.renderForeignError <$> _) <<<
                    List.toUnfoldable <<< ListTypes.toList

mapLeft :: ∀ a b c. (a -> c) -> Either a b -> Either c b
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x

unJson :: ∀ a. G.Decode a => String -> Either String a
unJson = mapLeft showForeignErrors <<< runExcept <<<
         JSON.decodeJSONWith (G.decode)

getJson :: ∀ a. G.Decode a => String -> Aff (Either String a)
getJson url = do
    {body} <- Affjax.get ResponseFormat.string url
    pure $ mapLeft Affjax.printResponseFormatError body
       >>= mapLeft showForeignErrors <<< runExcept <<<
           JSON.decodeJSONWith (G.decode)
