-- | 'Model.Skill.desc' descriptions are slightly smarter than plaintext.
-- Words in brackets turn red and italicized.
-- The letters b, g, n, t, and r in brackets become icons indicating
-- blood, genjutsu, ninjutsu, taijutsu, and random (a.k.a. arbitrary) chakra.
module Handler.Parse (desc) where

import ClassyPrelude
import Yesod

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parse
import qualified Text.Blaze.Html5 as HTML

import Game.Model.Chakra (Chakra(..))
import Game.Model.Character (Category(..))

-- | Parses a 'Model.Skill.desc' into HTML.
desc :: Text -> Html
desc s = case Parse.parseOnly (Parse.many' parseSegment) s of
    Left  _    -> HTML.toMarkup s
    Right html -> mconcat html

parseSegment :: Parser Html
parseSegment = Parse.choice
    [ " (S)"           $> HTML.toMarkup Shippuden
    , " (R)"           $> HTML.toMarkup Reanimated
    , Parse.char '\n' $> HTML.br
    , Parse.choice    $  parseChakra <$> [minBound..maxBound]
    , parseName
    , HTML.toMarkup   <$> Parse.takeWhile1 (Parse.notInClass " [\n")
    , HTML.toMarkup   <$> Parse.char ' '
    ]

parseChakra :: Chakra -> Parser Html
parseChakra kind = Parse.string (token kind) $> HTML.toMarkup kind
  where
    token Blood = "[b]"
    token Gen   = "[g]"
    token Nin   = "[n]"
    token Tai   = "[t]"
    token Rand  = "[r]"

parseName :: Parser Html
parseName = do
    void $ Parse.char '['
    name <- Parse.takeWhile (/= ']')
    void $ Parse.char ']'
    return . HTML.i $ HTML.toMarkup name
