{-# OPTIONS_HADDOCK hide #-}

module Game.Characters.Reanimated (characters) where

import Prelude (($), (<$>), (++))

import Game.Model.Character (Character, Category(Reanimated))

import qualified Game.Characters.Reanimated.Adults
import qualified Game.Characters.Reanimated.Kage
import qualified Game.Characters.Reanimated.Organizations

characters :: [Character]
characters = ($ Reanimated)
    <$> Game.Characters.Reanimated.Kage.characters
     ++ Game.Characters.Reanimated.Adults.characters
     ++ Game.Characters.Reanimated.Organizations.characters
