module Characters
    ( map
    , list
    , groupMap
    , groupList
    , shortName
    ) where

import Prelude
import Data.Array ((!!), head)
import Data.Function (on)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, fromMaybe')
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong ((&&&))
import Data.String as String
import Data.String (Pattern(..))
import Data.Tuple (Tuple(..))

import FFI.Import as Import
import Model.Character (Character(..))
import Model.Skill (Skill(..))
import Util (groupBy)

list :: Array Character
list = Import.chars

map :: Map String Character
map = Map.fromFoldable $ (\c -> Tuple (show c) c) <$> list

groupList :: Array (NonEmpty Array Character)
groupList = groupBy (eq `on` shortName) list

groupMap :: Map String (NonEmpty Array Character)
groupMap = Map.fromFoldable $ makeKey <$> groupList
  where
    makeKey xs@(x :| _) = Tuple (shortName x) xs

makeShortName :: Character -> String
makeShortName (Character c) = case c.name of
    "Tobi"          -> "Obito"
    "Masked Man"    -> "Obito"
    "Nagato"        -> "Pain"
    "Shukaku Gaara" -> "Gaara"
    _  -> fromMaybe' (\_ -> strip c.name) $ do
        skills       <- c.skills !! 3
        Skill {desc} <- head skills
        pure $ strip desc
  where
    strip x = String.takeWhile (_ /= String.codePointFromChar ' ') <<<
              fromMaybe x $ String.stripPrefix (Pattern "The") x

shortNames :: Map Character String
shortNames = Map.fromFoldable $ (identity &&& makeShortName) <$> list

shortName :: Character -> String
shortName c = fromMaybe' (\_ -> makeShortName c) $ Map.lookup c shortNames
