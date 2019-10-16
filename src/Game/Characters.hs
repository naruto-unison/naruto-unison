-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map
  , lookup
  , listJSON, mapJSON
  ) where

import ClassyPrelude hiding (link, lookup, map)

import           Data.Aeson (Value, toJSON)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           Game.Model.Character (Character)
import qualified Game.Model.Character as Character
import           Game.Model.Class (Class(..))
import           Game.Model.Skill (Skill)
import qualified Game.Model.Skill as Skill
import           Util ((∉), mapFromKeyed)

import qualified Game.Characters.Development
import qualified Game.Characters.Original
import qualified Game.Characters.Reanimated
import qualified Game.Characters.Shippuden

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
map = mapFromKeyed (Character.ident, id) list
{-# NOINLINE map #-}

mapJSON :: Value
mapJSON = toJSON map
{-# NOINLINE mapJSON #-}

lookup :: Text -> Maybe Character
lookup k = HashMap.lookup k map

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
            , (NonBane,   Bane ∉ Skill.classes skill)
            , (NonMental, Mental ∉ Skill.classes skill)
            , (NonRanged, Ranged ∉ Skill.classes skill)
            ]
