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
    ) where

import StandardLibrary
import Generic     as G
import Data.Map    as Map
import Data.String as String

import Data.Nullable (Nullable, toMaybe)
import Data.Profunctor.Strong ((&&&))

import Database.Structure

foreign import avatars     :: Array String
foreign import bg          :: String
foreign import cs'         :: Array Character
foreign import userTeam    :: Array Character
foreign import user'       :: Nullable Foreign
foreign import hostname    :: String
foreign import reload      :: Effect Unit
foreign import getPageSize :: Effect Int

cs :: Map String Character
cs = Map.fromFoldable $ makeKey <$> cs'
  where 
    makeKey c@Character{characterName} = Tuple characterName c

groupCs' :: Array (NonEmpty Array Character)
groupCs' = groupBy (eq `on` shortName) cs'

groupCs :: Map String (NonEmpty Array Character)
groupCs = Map.fromFoldable $ makeKey <$> groupCs'
  where 
    makeKey xs@(x :| _) = Tuple (shortName x) xs

user :: Maybe User
user = parseUser =<< toMaybe user'
  where 
    parseUser a = case runExcept (G.decode a) of
                      Right u   -> Just u
                      Left err  -> Nothing

makeShortName :: Character -> String
makeShortName (Character c) = case c.characterName of
    "Tobi (S)"      -> "Obito"
    "Masked Man"    -> "Obito"
    "Nagato (S)"    -> "Pain"
    "Nagato (R)"    -> "Pain"
    "Shukaku Gaara" -> "Gaara"
    _  -> fromMaybe' (\_ -> strip c.characterName) $ do
        skills       <- c.characterSkills !! 3
        Skill {desc} <- head skills
        pure $ strip desc
  where
    strip a = String.takeWhile (_ /= String.codePointFromChar ' ') $
                  maybeDo (String.stripPrefix $ Pattern "The") a

shortNames :: Map Character String
shortNames = Map.fromFoldable $ (identity &&& makeShortName) <$> cs'

shortName :: Character -> String
shortName c = fromMaybe' (\_ -> makeShortName c) $ Map.lookup c shortNames
