-- | 'Model.Skill.desc' descriptions are slightly smarter than plaintext.
-- Words in brackets turn red and italicized.
-- The letters b, g, n, t, and r in brackets become icons indicating
-- blood, genjutsu, ninjutsu, taijutsu, and random (a.k.a. arbitrary) chakra.
module Handler.Parse (richText) where

import ClassyPrelude

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parse
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as HTML

import Game.Model.Chakras (Chakra(..))
import Game.Model.Character (Category(..))

-- | Parses a 'Model.Skill.desc' into HTML.
richText :: Text -> Html
richText s = case Parse.parseOnly parser s of
    Left  _    -> HTML.toMarkup s
    Right html -> mconcat html
  where
    parser = Parse.many' parseSegment <* Parse.endOfInput

parseSegment :: Parser Html
parseSegment = Parse.choice
    [ Parse.char '\n'     $> HTML.br
    , Parse.string " (S)" $> HTML.toMarkup Shippuden
    , Parse.string " (R)" $> HTML.toMarkup Reanimated
    , parseName
    , Parse.takeWhile1 (== ' ') <&> HTML.toMarkup
    , Parse.takeWhile1 (Parse.notInClass " [\n") <&> HTML.toMarkup
    ]

parseName :: Parser Html
parseName = do
    Parse.char '['
    name <- Parse.takeWhile (/= ']')
    Parse.char ']'
    return case name of
        "b" -> HTML.toMarkup Blood
        "g" -> HTML.toMarkup Gen
        "n" -> HTML.toMarkup Nin
        "t" -> HTML.toMarkup Tai
        "r" -> HTML.toMarkup Rand
        _   -> HTML.i $ HTML.toMarkup name
