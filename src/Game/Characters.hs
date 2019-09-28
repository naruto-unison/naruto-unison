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

import           Core.Util ((∉), mapFromKeyed, unaccent)
import qualified Game.Model.Character as Character
import           Game.Model.Character (Character, Category(..))
import           Game.Model.Class (Class(..))
import qualified Game.Model.Skill as Skill
import           Game.Model.Skill (Skill)

import qualified Game.Characters.Original.Kids
import qualified Game.Characters.Original.Exams
import qualified Game.Characters.Original.Teachers
import qualified Game.Characters.Original.Organizations
import qualified Game.Characters.Original.Leaders
import qualified Game.Characters.Original.Versions
import qualified Game.Characters.Original.Family
import qualified Game.Characters.Original.Flashbacks

import qualified Game.Characters.Shippuden.Kids
import qualified Game.Characters.Shippuden.Adults
import qualified Game.Characters.Shippuden.Leaders
import qualified Game.Characters.Shippuden.Organizations
import qualified Game.Characters.Shippuden.Akatsuki
import qualified Game.Characters.Shippuden.Jinchuriki
import qualified Game.Characters.Shippuden.Versions

import qualified Game.Characters.Reanimated

import qualified Game.Characters.Development

list :: [Character]
list = addClasses <$> original ++ shippuden ++ reanimated
  where
    original = ($ Original)
       <$> Game.Characters.Development.cs
        ++ Game.Characters.Original.Kids.cs
        ++ Game.Characters.Original.Exams.cs
        ++ Game.Characters.Original.Teachers.cs
        ++ Game.Characters.Original.Organizations.cs
        ++ Game.Characters.Original.Leaders.cs
        ++ Game.Characters.Original.Versions.cs
        ++ Game.Characters.Original.Family.cs
        ++ Game.Characters.Original.Flashbacks.cs
    shippuden = ($ Shippuden)
         <$> Game.Characters.Shippuden.Kids.cs
          ++ Game.Characters.Shippuden.Adults.cs
          ++ Game.Characters.Shippuden.Organizations.cs
          ++ Game.Characters.Shippuden.Akatsuki.cs
          ++ Game.Characters.Shippuden.Leaders.cs
          ++ Game.Characters.Shippuden.Jinchuriki.cs
          ++ Game.Characters.Shippuden.Versions.cs
    reanimated = ($ Reanimated) <$> Game.Characters.Reanimated.cs
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
