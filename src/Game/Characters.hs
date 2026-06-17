{-# LANGUAGE CPP             #-}
-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map
  , lookup, lookupAll
  ) where

import ClassyPrelude hiding (link, lookup, map)

import           Game.Model.Character (Character)
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
list = processCharacter <$>
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

processCharacter :: Character -> Character
processCharacter char =
    char { Character.ident  = Character.identFrom char.category char.name
         , Character.groups = groups ++ char.groups
         , Character.skills = (processSkill <$>) <$> char.skills
         }
  where
    chakras = concatMap Skill.cost $ join char.skills
    groups  = setFromList $ fst <$> filter ((∈ chakras) . snd)
                [ (BloodlineUser, Blood)
                , (GenjutsuUser, Gen)
                , (NinjutsuUser, Nin)
                , (TaijutsuUser,  Tai)
                ]

processSkill :: Skill -> Skill
processSkill skill@Skill{classes} = skill { Skill.classes = added ++ classes }
  where
    added = setFromList $ fst <$> filter snd
            [ (All,       True)
            , (NonBane,   Bane ∉ classes)
            , (NonMental, Mental ∉ classes)
            , (NonRanged, Ranged ∉ classes)
            ]
