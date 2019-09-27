-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Characters
  ( list, map
  , lookupName
  , link, lookupSite
  , listJSON, mapJSON
  ) where

import ClassyPrelude hiding (link, map)

import           Data.Aeson (Value, toJSON)
import qualified Data.Enum.Memo as Enum
import           Data.HashMap.Strict (HashMap)

import           Core.Util ((∉), mapFromKeyed, unaccent)
import qualified Model.Character as Character
import           Model.Character (Character, Category(..))
import           Model.Class (Class(..))
import qualified Model.Skill as Skill
import           Model.Skill (Skill)

import qualified Characters.Original.Kids
import qualified Characters.Original.Exams
import qualified Characters.Original.Teachers
import qualified Characters.Original.Organizations
import qualified Characters.Original.Leaders
import qualified Characters.Original.Versions
import qualified Characters.Original.Family
import qualified Characters.Original.Flashbacks

import qualified Characters.Shippuden.Kids
import qualified Characters.Shippuden.Adults
import qualified Characters.Shippuden.Leaders
import qualified Characters.Shippuden.Organizations
import qualified Characters.Shippuden.Akatsuki
import qualified Characters.Shippuden.Jinchuriki
import qualified Characters.Shippuden.Versions

import qualified Characters.Reanimated

import qualified Characters.Development

list :: [Character]
list = addClasses <$> original ++ shippuden ++ reanimated
  where
    original = ($ Original)
        <$> Characters.Development.cs
        ++ Characters.Original.Kids.cs
        ++ Characters.Original.Exams.cs
        ++ Characters.Original.Teachers.cs
        ++ Characters.Original.Organizations.cs
        ++ Characters.Original.Leaders.cs
        ++ Characters.Original.Versions.cs
        ++ Characters.Original.Family.cs
        ++ Characters.Original.Flashbacks.cs
    shippuden = ($ Shippuden)
        <$> Characters.Shippuden.Kids.cs
          ++ Characters.Shippuden.Adults.cs
          ++ Characters.Shippuden.Organizations.cs
          ++ Characters.Shippuden.Akatsuki.cs
          ++ Characters.Shippuden.Leaders.cs
          ++ Characters.Shippuden.Jinchuriki.cs
          ++ Characters.Shippuden.Versions.cs
    reanimated = ($ Reanimated) <$> Characters.Reanimated.cs
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
