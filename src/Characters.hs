module Characters (map, list) where

import ClassyPrelude hiding (map)

import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty(..))

import           Core.Util ((∉), intersects)
import qualified Model.Character as Character
import           Model.Character (Character, Category(..))
import           Model.Class (Class(..))
import qualified Model.Runnable as Runnable
import qualified Model.Skill as Skill
import           Model.Skill (Skill, Target(..))

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
import qualified Characters.Shippuden.Family
import qualified Characters.Shippuden.Organizations
import qualified Characters.Shippuden.Akatsuki
import qualified Characters.Shippuden.Versions

import qualified Characters.Reanimated

original :: [Character]
original = ($ Original)
           <$> Characters.Original.Kids.cs
            ++ Characters.Original.Exams.cs
            ++ Characters.Original.Teachers.cs
            ++ Characters.Original.Organizations.cs
            ++ Characters.Original.Leaders.cs
            ++ Characters.Original.Versions.cs
            ++ Characters.Original.Family.cs
            ++ Characters.Original.Flashbacks.cs

shippuden :: [Character]
shippuden = ($ Shippuden)
            <$> Characters.Shippuden.Kids.cs
             ++ Characters.Shippuden.Adults.cs
             ++ Characters.Shippuden.Family.cs
             ++ Characters.Shippuden.Organizations.cs
             ++ Characters.Shippuden.Akatsuki.cs
             ++ Characters.Shippuden.Versions.cs

reanimated :: [Character]
reanimated = ($ Reanimated) <$> Characters.Reanimated.cs

list :: [Character]
list = addClasses <$> original ++ shippuden ++ reanimated

map :: HashMap Text Character
map = mapFromList $ (\c -> (Character.format c, c)) <$> list

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
            , (Harmful,   harm)
            ]
    harm = [Enemy, Enemies, REnemy, XEnemies] `intersects` ts
    ts   = Runnable.target
           <$> Skill.start skill ++ Skill.effects skill ++ Skill.interrupt skill
