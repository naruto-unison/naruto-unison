-- | 'Model.Skill.desc' descriptions are slightly smarter than plaintext.
-- Words in brackets turn red and italicized.
-- The letters b, g, n, t, and r in brackets become icons indicating
-- blood, genjutsu, ninjutsu, taijutsu, and random (a.k.a. arbitrary) chakra.
module Handler.Parse (richText) where

import ClassyPrelude

import           Data.Attoparsec.Text (Parser)
import           Text.Blaze ((!))
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as HTML

import qualified Class.Parse as Parse
import           Game.Model.Chakras (Chakra(..))
import qualified Game.Model.Character as Character
import           Game.Model.Character (Category(..))

-- | Parses a 'Model.Skill.desc' into HTML.
richText :: Text -> Html
richText s = case Parse.parseOnly' parser s of
    Left  _    -> HTML.toMarkup s
    Right html -> mconcat html
  where
    parser = Parse.many' parseSegment <* Parse.endOfInput

parseSegment :: Parser Html
parseSegment = Parse.choice
    [ Parse.char '\n'     $> HTML.br
    , Parse.string " (S)" $> HTML.toMarkup Shippuden
    , Parse.string " (R)" $> HTML.toMarkup Reanimated
    , parseBrackets
    , parseBraces
    , Parse.takeWhile1 (== ' ') <&> HTML.toMarkup
    , Parse.takeWhile1 (Parse.notInClass " [\n") <&> HTML.toMarkup
    ]

takeBetween :: (Char, Char) -> Parser Text
takeBetween (before, after) = do
    Parse.char before
    text <- Parse.takeWhile (/= after)
    Parse.char after
    return text

parseBrackets :: Parser Html
parseBrackets = do
    name <- takeBetween ('[', ']')
    return case name of
        "b" -> HTML.toMarkup Blood
        "g" -> HTML.toMarkup Gen
        "n" -> HTML.toMarkup Nin
        "t" -> HTML.toMarkup Tai
        "r" -> HTML.toMarkup Rand
        _   -> HTML.i $ HTML.toMarkup name

parseBraces :: Parser Html
parseBraces = do
    name <- takeBetween ('{', '}')
    return $ HTML.a ! HTML.href (href name) $ HTML.toMarkup name
  where
    href name = HTML.toValue $ "/characters/" ++ Character.clean name
