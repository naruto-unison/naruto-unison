{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Character's are used as 'PathPiece's in Yesod's routing system, but their
-- encoding and decoding depends on the 'Characters.map' value, which itself
-- depends on the definition of @Character@.
module OrphanInstances.Character () where

import ClassyPrelude

import           Text.Read hiding (read)
import Yesod.Core.Dispatch (PathPiece(..))

import qualified Game.Characters as Characters
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character

instance PathPiece Character where
    toPathPiece   = Character.ident
    fromPathPiece = Characters.lookup

instance Show Character where
    showsPrec i Character{ident} = showsPrec i ident

instance Read Character where
    readPrec = parens $ prec 10 do
        String s <- lexP
        case Characters.lookup (pack s) of
            Just character -> return character
            Nothing        -> empty
