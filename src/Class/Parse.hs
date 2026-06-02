module Class.Parse
    ( Parse(..)
    , parseOnly
    , module Data.Attoparsec.ByteString.Char8
    ) where

import ClassyPrelude

import Data.Attoparsec.ByteString.Char8 hiding (parseOnly)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

parseOnly :: ∀ a. Parse a => ByteString -> Either String a
parseOnly = Attoparsec.parseOnly parser

class Parse a where
    parser :: Parser a

instance Parse Int where
    parser = decimal
