{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Original (characters) where

import Prelude (($), (<$>), (++))

import Game.Model.Character (Character, Category(Original))

import qualified Game.Characters.Original.Kids
import qualified Game.Characters.Original.Exams
import qualified Game.Characters.Original.Teachers
import qualified Game.Characters.Original.Organizations
import qualified Game.Characters.Original.Leaders
import qualified Game.Characters.Original.Versions
import qualified Game.Characters.Original.Family
import qualified Game.Characters.Original.Flashbacks

characters :: [Character]
characters = ($ Original)
    <$> Game.Characters.Original.Kids.characters
    ++ Game.Characters.Original.Exams.characters
    ++ Game.Characters.Original.Teachers.characters
    ++ Game.Characters.Original.Organizations.characters
    ++ Game.Characters.Original.Leaders.characters
    ++ Game.Characters.Original.Versions.characters
    ++ Game.Characters.Original.Family.characters
    ++ Game.Characters.Original.Flashbacks.characters
