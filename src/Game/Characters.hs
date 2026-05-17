-- | The character database.
-- Contains everything in the [Characters](src/Characters/) folder.
module Game.Characters
  ( list, map
  , lookup
  , listJSON, mapJSON
  ) where

import ClassyPrelude hiding (link, lookup, map)

import           Data.Aeson (Value, toJSON)
import qualified Data.HashMap.Strict as HashMap

import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Game.Model.Chakra (Chakra(..))
import qualified Game.Model.Chakra as Chakra
import           Game.Model.Class (Class(..))
import           Game.Model.Group (Group(..))
import           Game.Model.Skill (Skill(Skill))
import qualified Game.Model.Skill as Skill
import           Util ((∉), mapFromKeyed)

import qualified Game.Characters.Development
import qualified Game.Characters.Original
import qualified Game.Characters.Reanimated
import qualified Game.Characters.Shippuden

list :: [Character]
list = addGroups . addClasses
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

addGroups :: Character -> Character
addGroups char@Character{groups, skills} =
    char { Character.groups = added `union` groups }
  where
    costs = Skill.cost <$> concatMap toList skills
    added = setFromList $ filter is [minBound..maxBound]
    uses chakra chakras = not . Chakra.lack $ chakras - Chakra.toChakras chakra
    is BloodlineUser = any (uses Blood) costs
    is GenjutsuUser  = any (uses Gen)   costs
    is NinjutsuUser  = any (uses Nin)   costs
    is TaijutsuUser  = any (uses Tai)   costs
    is _             = False

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
