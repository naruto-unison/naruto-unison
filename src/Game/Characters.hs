{-# LANGUAGE CPP #-}
-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map, lookup
  , siteList, siteMap, siteLookup
  ) where

import ClassyPrelude hiding (link, lookup, map)
import Data.Text (dropWhileEnd)

import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Chakras (Chakra(..))
import           Game.Model.Group (Group(..))
import qualified Game.Model.Requirement as Requirement
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Util ((∈), (?), insertIf, lazyMapFromKeyed)

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

siteList :: [Character]
siteList = dedupAlternates <$> list
{-# NOINLINE siteList #-}

map :: HashMap Text Character
map = lazyMapFromKeyed (Character.ident, id) list
{-# NOINLINE map #-}

siteMap :: HashMap Text Character
siteMap = lazyMapFromKeyed (Character.ident, id) siteList
{-# NOINLINE siteMap #-}

lookup :: Text -> Maybe Character
lookup k = map ? k

siteLookup :: Text -> Maybe Character
siteLookup k = siteMap ? k

processCharacter :: Character -> Character
processCharacter char =
    char { Character.ident  = Character.identFrom char.category char.name
         , Character.groups = groups ++ char.groups
         , Character.skills = (processSkill <$>) <$> char.skills
         }
  where
    chakras = concatMap Skill.cost $ join char.skills
    groups  = insertIf (Blood ∈ chakras) BloodlineUser
            . insertIf (Gen ∈ chakras) GenjutsuUser
            . insertIf (Nin ∈ chakras) NinjutsuUser
            . insertIf (Tai ∈ chakras) TaijutsuUser
            $ mempty

processSkill :: Skill -> Skill
processSkill skill = Skill.withExtraClasses skill
    { Skill.require = Requirement.withSkillName skill.name <$> skill.require }

dedupAlternates :: Character -> Character
dedupAlternates char@Character{skills}
  | hasDups   = char { Character.skills = dedupedSkills }
  | otherwise = char
  where
    hasDups = or $ zipWith ((/=) `on` length) skills dedupedSkills
    dedupedSkills = dedup <$> skills
    dedup xxs@(x:|xs)
      | length deduped == length xs = xxs
      | otherwise                   = x :| deduped
      where
        deduped = filter ((/= x.name) . dropWhileEnd (== ' ') . Skill.name) xs

