{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Character's are used as 'PathPiece's in Yesod's routing system, but their
-- encoding and decoding depends on the 'Characters.map' value, which itself
-- depends on the definition of @Character@.
module OrphanInstances.Character () where

import ClassyPrelude

import Text.Read
import Yesod.Core.Dispatch (PathPiece(..))

import           Class.Parse (Parse)
import qualified Class.Parse as Parse
import qualified Game.Characters as Characters
import           Game.Model.Character (Character(Character))
import qualified Game.Model.Character as Character
import           Util ((∈))

allValidChars :: IntSet
allValidChars = concatMap identChars Characters.list
  where
    identChars Character{ident} = setFromList $ fromEnum <$> unpack ident

instance Parse Character where
    parser = do
        ident <- Parse.toUtf8 <$> Parse.takeWhile isValidChar
        case Characters.lookup ident of
            Just c  -> return c
            Nothing -> fail . unpack $ ident ++ " is not a character"
      where
        isValidChar c = fromEnum c ∈ allValidChars

instance PathPiece Character where
    toPathPiece   = Character.ident
    fromPathPiece = Characters.lookup

instance Read Character where
    readPrec = parens $ prec 10 do
        String s <- lexP
        Just character <- return $ Characters.lookup (pack s)
        return character

instance Show Character where
    showsPrec i Character{ident} = showsPrec i ident
