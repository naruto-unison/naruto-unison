module Class.Parse
    ( Parse(..)
    , Parsed(..)
    , Parser
    , parseOnly, parseOnly'
    , parseToEnd
    , module Data.Attoparsec.Combinator
    , module Char8Import
    ) where

import ClassyPrelude

import Data.Attoparsec.ByteString.Char8 as Char8Import (isDigit, isDigit_w8, isAlpha_iso8859_15, isAlpha_ascii, isSpace, isSpace_w8, inClass, notInClass)

import qualified Data.Attoparsec.ByteString.Char8 as Char8
import qualified Data.Attoparsec.ByteString as BS
import qualified Data.Attoparsec.ByteString.Lazy as LBS
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Internal.Types (Chunk, Parser)
import qualified Data.Attoparsec.Text as T
import qualified Data.Attoparsec.Text.Lazy as LT
import           Data.Bits (Bits)
import           Data.Scientific (Scientific)

class (Chunk i, IsString i, IsString (Parser i i)) => Parsed i where
    type Lazy i
    anyChar     :: Parser i Char
    char        :: Char -> Parser i Char
    decimal     :: ∀ a. Integral a => Parser i a
    digit       :: Parser i Char
    double      :: Parser i Double
    endOfLine   :: Parser i ()
    hexadecimal :: ∀ a. (Integral a, Bits a) => Parser i a
    notChar     :: Char -> Parser i Char
    peekChar    :: Parser i (Maybe Char)
    peekChar'   :: Parser i Char
    rational    :: ∀ a. Fractional a => Parser i a
    satisfy     :: (Char -> Bool) -> Parser i Char
    scan        :: ∀ s. s -> (s -> Char -> Maybe s) -> Parser i i
    scientific  :: Parser i Scientific
    signed      :: ∀ a. Num a => Parser i a -> Parser i a
    skipSpace   :: Parser i ()
    skipWhile   :: (Char -> Bool) -> Parser i ()
    space       :: Parser i Char
    string      :: i -> Parser i i
    stringCI    :: i -> Parser i i
    take        :: Int -> Parser i i
    takeLazy    :: Parser i (Lazy i)
    takeStrict  :: Parser i i
    takeTill    :: (Char -> Bool) -> Parser i i
    takeWhile   :: (Char -> Bool) -> Parser i i
    takeWhile1  :: (Char -> Bool) -> Parser i i
    toUtf8      :: i -> Text

instance Parsed ByteString where
    type Lazy ByteString = LByteString
    anyChar     = Char8.anyChar
    char        = Char8.char
    decimal     = Char8.decimal
    digit       = Char8.digit
    double      = Char8.double
    endOfLine   = Char8.endOfLine
    hexadecimal = Char8.hexadecimal
    notChar     = Char8.notChar
    peekChar    = Char8.peekChar
    peekChar'   = Char8.peekChar'
    rational    = Char8.rational
    satisfy     = Char8.satisfy
    scan        = Char8.scan
    scientific  = Char8.scientific
    signed      = Char8.signed
    skipSpace   = Char8.skipSpace
    skipWhile   = Char8.skipWhile
    space       = Char8.space
    string      = Char8.string
    stringCI    = Char8.stringCI
    take        = Char8.take
    takeLazy    = Char8.takeLazyByteString
    takeStrict  = Char8.takeByteString
    takeTill    = Char8.takeTill
    takeWhile   = Char8.takeWhile
    takeWhile1  = Char8.takeWhile1
    toUtf8      = decodeUtf8

instance Parsed Text where
    type Lazy Text = LText
    anyChar     = T.anyChar
    char        = T.char
    decimal     = T.decimal
    digit       = T.digit
    double      = T.double
    endOfLine   = T.endOfLine
    hexadecimal = T.hexadecimal
    notChar     = T.notChar
    peekChar    = T.peekChar
    peekChar'   = T.peekChar'
    rational    = T.rational
    satisfy     = T.satisfy
    scan        = T.scan
    scientific  = T.scientific
    signed      = T.signed
    skipSpace   = T.skipSpace
    skipWhile   = T.skipWhile
    space       = T.space
    string      = T.string
    stringCI    = T.asciiCI
    take        = T.take
    takeLazy    = T.takeLazyText
    takeStrict  = T.takeText
    takeTill    = T.takeTill
    takeWhile   = T.takeWhile
    takeWhile1  = T.takeWhile1
    toUtf8      = id

class Parse a where
    parser :: ∀ i. Parsed i => Parser i a

class Parseable i where
    type ParseBacking i
    type ParseBacking i = i
    parseOnly' :: ∀ a. Parser (ParseBacking i) a -> i -> Either String a

instance Parseable ByteString where
    parseOnly' = BS.parseOnly

instance Parseable LByteString where
    type ParseBacking LByteString = ByteString
    parseOnly' = LBS.parseOnly

instance Parseable Text where
    parseOnly' = T.parseOnly

instance Parseable LText where
    type ParseBacking LText = Text
    parseOnly' = LT.parseOnly

parseOnly :: ∀ a i. (Parse a, Parseable i, Parsed (ParseBacking i))
          => i -> Either String a
parseOnly = parseOnly' $ parser @a @(ParseBacking i)

parseToEnd :: ∀ a i. (Parse a, Parseable i, Parsed (ParseBacking i))
           => i -> Either String a
parseToEnd = parseOnly' $ parser <* endOfInput

instance Parse Char where
    parser = anyChar

instance Parse Double where
    parser = double

instance Parse Int where
    parser = decimal
