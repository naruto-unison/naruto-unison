{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Shippuden (characters) where

import Prelude ((<$>), (++))

import Game.Model.Character (Character, Category(Shippuden))

import qualified Game.Characters.Shippuden.Kids
import qualified Game.Characters.Shippuden.Adults
import qualified Game.Characters.Shippuden.Leaders
import qualified Game.Characters.Shippuden.Organizations
import qualified Game.Characters.Shippuden.Akatsuki
import qualified Game.Characters.Shippuden.Jinchuriki
import qualified Game.Characters.Shippuden.Versions

characters :: [Character]
characters = (\x -> x 0 Shippuden)
    <$> Game.Characters.Shippuden.Kids.characters
    ++ Game.Characters.Shippuden.Adults.characters
    ++ Game.Characters.Shippuden.Organizations.characters
    ++ Game.Characters.Shippuden.Akatsuki.characters
    ++ Game.Characters.Shippuden.Leaders.characters
    ++ Game.Characters.Shippuden.Jinchuriki.characters
    ++ Game.Characters.Shippuden.Versions.characters
