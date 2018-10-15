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
    , shortName
    , showForeignErrors
    ) where

import StandardLibrary
import Foreign         as Foreign
import Generic         as G
import Data.List       as List
import Data.List.Types as ListTypes
import Data.Map        as Map
import Partial.Unsafe  as Partial
import Data.String     as String

import Data.List.NonEmpty (NonEmptyList)
import Data.Nullable (Nullable, toMaybe)
import Data.Profunctor.Strong ((&&&))

import Database.Structure

foreign import avatars     :: Array String
foreign import bg          :: String
foreign import fCs'        :: Array Foreign
foreign import fUserTeam   :: Array Foreign
foreign import fUser       :: Nullable Foreign
foreign import hostname    :: String
foreign import reload      :: Effect Unit
foreign import getPageSize :: Effect Int


cs' :: Array Character
cs' = unsafeImport <$> fCs'

userTeam :: Array Character
userTeam = unsafeImport <$> fUserTeam

cs :: Map String Character
cs = Map.fromFoldable $ makeKey <$> cs'
  where 
    makeKey c'@(Character c) = flip Tuple c' $ 
                               c.characterName <> case c.characterGroup of
                                   Original   -> ""
                                   Shippuden  -> " (R)"
                                   Reanimated -> " (S)"

groupCs' :: Array (NonEmpty Array Character)
groupCs' = groupBy (eq `on` shortName) cs'

groupCs :: Map String (NonEmpty Array Character)
groupCs = Map.fromFoldable $ makeKey <$> groupCs'
  where 
    makeKey xs@(x :| _) = Tuple (shortName x) xs

user :: Maybe User
user = parseUser =<< toMaybe fUser
  where 
    parseUser x = case runExcept (G.decode x) of
                      Right u   -> Just u
                      Left err  -> Nothing

makeShortName :: Character -> String
makeShortName (Character c) = case c.characterName of
    "Tobi"          -> "Obito"
    "Masked Man"    -> "Obito"
    "Nagato"        -> "Pain"
    "Shukaku Gaara" -> "Gaara"
    _  -> fromMaybe' (\_ -> strip c.characterName) $ do
        skills       <- c.characterSkills !! 3
        Skill {desc} <- head skills
        pure $ strip desc
  where
    strip = String.takeWhile (_ /= String.codePointFromChar ' ') <<<
            maybeDo (String.stripPrefix $ Pattern "The")

shortNames :: Map Character String
shortNames = Map.fromFoldable $ (identity &&& makeShortName) <$> cs'

shortName :: Character -> String
shortName c = fromMaybe' (\_ -> makeShortName c) $ Map.lookup c shortNames

unsafeImport :: ∀ a. G.Decode a => Foreign -> a
unsafeImport x = case runExcept (G.decode x) of
    Right u -> u
    Left err -> Partial.unsafeCrashWith $ showForeignErrors err

showForeignErrors :: NonEmptyList Foreign.ForeignError -> String
showForeignErrors = String.joinWith "\n" <<< map Foreign.renderForeignError <<< 
                    List.toUnfoldable <<< ListTypes.toList
