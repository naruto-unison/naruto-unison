-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map
  , lookupName
  , link, lookupSite
  , listJSON, mapJSON
  ) where

import ClassyPrelude hiding (link, map)

import           Data.Aeson (Value, toJSON)
import qualified Data.Enum.Memo as Enum
import           Data.HashMap.Strict (HashMap)

import           Util ((∉), mapFromKeyed, unaccent)
import qualified Game.Model.Character as Character
import           Game.Model.Character (Character, Category(..))
import           Game.Model.Class (Class(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill)

import qualified Game.Characters.Original
import qualified Game.Characters.Shippuden
import qualified Game.Characters.Reanimated
import qualified Game.Characters.Development

list :: [Character]
list = addClasses
    <$> Game.Characters.Development.characters
    ++ Game.Characters.Original.characters
    ++ Game.Characters.Shippuden.characters
    ++ Game.Characters.Reanimated.characters
{-# NOINLINE list #-}

listJSON :: Value
listJSON = toJSON list
{-# NOINLINE listJSON #-}

map :: HashMap Text Character
map = mapFromKeyed (Character.format, id) list
{-# NOINLINE map #-}

mapJSON :: Value
mapJSON = toJSON map
{-# NOINLINE mapJSON #-}

lookupName :: Text -> Maybe Character
lookupName k = lookup k map

addClasses :: Character -> Character
addClasses char = char { Character.skills = doSkills <$> Character.skills char }

doSkills :: NonEmpty Skill -> NonEmpty Skill
doSkills (x:|xs) = doSkill x :| (doSkill . vari <$> xs)
  where
    vari skill = skill
                  { Skill.varicd = Skill.varicd skill
                                   || Skill.cooldown x /= Skill.cooldown skill
                                   || diff skill }
    diff skill = Skill.name x ∉
                 [Skill.name skill, fromMaybe "" . initMay $ Skill.name x]

doSkill :: Skill -> Skill
doSkill skill = skill { Skill.classes = added ++ Skill.classes skill }
  where
    added = setFromList $ fst <$> filter snd
            [ (All,       True)
            , (NonMental, Mental ∉ Skill.classes skill)
            ]

link :: Character -> Text
link = omap clean . toLower . Character.name
  where
    clean ' ' = '-'
    clean x   = unaccent x

siteChars :: Category -> HashMap Text Character
siteChars =
    Enum.memoize \category -> mapFromKeyed (link, id) $
        filter ((== category) . Character.category) list
{-# NOINLINE siteChars #-}

lookupSite :: Category -> Text -> Maybe Character
lookupSite category x = lookup x $ siteChars category
