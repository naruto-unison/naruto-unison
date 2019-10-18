-- | 'Model.Skill.desc' descriptions are slightly smarter than plaintext.
-- Words in brackets turn red and italicized.
-- The letters b, g, n, t, and r in brackets become icons indicating
-- blood, genjutsu, ninjutsu, taijutsu, and random (a.k.a. arbitrary) chakra.
module Handler.Parse (desc) where

import ClassyPrelude
import Yesod

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import qualified Text.Blaze.Html5 as HTML

import Game.Model.Chakra (Chakra(..))
import Game.Model.Character (Category(..))

-- | Parses a 'Model.Skill.desc' into HTML.
desc :: Text -> Html
desc s = case Parser.parseOnly (Parser.many' parseSegment) s of
    Left  _    -> HTML.toMarkup s
    Right html -> mconcat html

parseSegment :: Parser Html
parseSegment = Parser.choice
    [ " (S)"           $> HTML.toMarkup Shippuden
    , " (R)"           $> HTML.toMarkup Reanimated
    , Parser.char '\n' $> HTML.br
    , Parser.choice    $  parseChakra <$> [minBound .. maxBound]
    , parseName
    , HTML.toMarkup   <$> Parser.takeWhile1 (Parser.notInClass " [\n")
    , HTML.toMarkup   <$> Parser.char ' '
    ]

parseChakra :: Chakra -> Parser Html
parseChakra kind = Parser.string (token kind) $> HTML.toMarkup kind
  where
    token Blood = "[b]"
    token Gen   = "[g]"
    token Nin   = "[n]"
    token Tai   = "[t]"
    token Rand  = "[r]"

parseName :: Parser Html
parseName = do
    void $ Parser.char '['
    name <- Parser.takeWhile (/= ']')
    void $ Parser.char ']'
    return . HTML.i $ HTML.toMarkup name
