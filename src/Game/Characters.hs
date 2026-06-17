{-# LANGUAGE CPP             #-}
-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map
  , lookup, lookupAll
  ) where

import ClassyPrelude hiding (link, lookup, map)

import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Chakras (Chakra(..))
import           Game.Model.Class (Class(..))
import           Game.Model.Group (Group(..))
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Util ((∈), (∉), (?), lazyMapFromKeyed)

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
map = lazyMapFromKeyed (Character.ident, id) list
{-# NOINLINE map #-}

lookup :: Text -> Maybe Character
lookup k = map ? k

lookupAll :: [Text] -> [Character]
lookupAll ks = mapMaybe lookup ks

setIdent :: Character -> Character
setIdent char@Character{category, name} =
    char { Character.ident = Character.identFrom category name }

addGroups :: Character -> Character
addGroups char = char { Character.groups = added ++ char.groups }
  where
    chakras = concatMap Skill.cost $ join char.skills
    added = setFromList $ fst <$> filter ((∈ chakras) . snd)
                [ (BloodlineUser, Blood)
                , (GenjutsuUser, Gen)
                , (NinjutsuUser, Nin)
                , (TaijutsuUser,  Tai)
                ]

addClasses :: Character -> Character
addClasses char = char { Character.skills = (addClass <$>) <$> char.skills }

addClass :: Skill -> Skill
addClass skill@Skill{classes} = skill { Skill.classes = added ++ classes }
  where
    added = setFromList $ fst <$> filter snd
            [ (All,       True)
            , (NonBane,   Bane ∉ classes)
            , (NonMental, Mental ∉ classes)
            , (NonRanged, Ranged ∉ classes)
            ]
