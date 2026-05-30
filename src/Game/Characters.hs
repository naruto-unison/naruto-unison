{-# LANGUAGE CPP             #-}
-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map
  , lookup, lookupAll
  ) where

import ClassyPrelude hiding (link, lookup, map)

import qualified Data.HashMap.Strict as HashMap

import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Chakras (Chakras(Chakras))
import qualified Game.Model.Chakras
import           Game.Model.Class (Class(..))
import           Game.Model.Group (Group(..))
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Util ((∉), mapFromKeyed)

#ifdef DEVELOPMENT
import qualified Game.Characters.Development
#endif
import qualified Game.Characters.Original
import qualified Game.Characters.Reanimated
import qualified Game.Characters.Shippuden

list :: [Character]
list = setIdent . addGroups . addClasses <$>
#ifdef DEVELOPMENT
    Game.Characters.Development.characters ++
#endif
    Game.Characters.Original.characters ++
    Game.Characters.Shippuden.characters ++
    Game.Characters.Reanimated.characters
{-# NOINLINE list #-}

map :: HashMap Text Character
map = mapFromKeyed (Character.ident, id) list
{-# NOINLINE map #-}

lookup :: Text -> Maybe Character
lookup k = HashMap.lookup k map

lookupAll :: [Text] -> [Character]
lookupAll ks = mapMaybe lookup ks

setIdent :: Character -> Character
setIdent char@Character{category, name} =
    char { Character.ident = Character.identFrom category name }

addGroups :: Character -> Character
addGroups char@Character{groups, skills} =
    char { Character.groups = added ++ groups }
  where
    Chakras{blood, gen, nin, tai} = concatMap (Skill.cost)
                                  $ concatMap toList skills
    added = setFromList $ fst <$> filter snd [ (BloodlineUser, blood /= 0)
                                             , (GenjutsuUser,  gen   /= 0)
                                             , (NinjutsuUser,  nin   /= 0)
                                             , (TaijutsuUser,  tai   /= 0)
                                             ]

addClasses :: Character -> Character
addClasses char@Character{skills} =
    char { Character.skills = (addClass <$>) <$> skills }

addClass :: Skill -> Skill
addClass skill@Skill{classes} = skill { Skill.classes = added ++ classes }
  where
    added = setFromList $ fst <$> filter snd
            [ (All,       True)
            , (NonBane,   Bane ∉ classes)
            , (NonMental, Mental ∉ classes)
            , (NonRanged, Ranged ∉ classes)
            ]
