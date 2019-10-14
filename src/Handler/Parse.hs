-- | 'Model.Skill.desc' descriptions are slightly smarter than plaintext.
-- Words in brackets turn red and italicized.
-- The letters b, g, n, t, and r in brackets become icons indicating
-- blood, genjutsu, ninjutsu, taijutsu, and random (a.k.a. arbitrary) chakra.
module Handler.Parse (desc) where

import ClassyPrelude
import Yesod

import qualified Text.Blaze.Html5 as HTML
import           Text.ParserCombinators.ReadP ((<++), ReadP)
import qualified Text.ParserCombinators.ReadP as Parser

import Game.Model.Chakra (Chakra(..))
import Game.Model.Character (Category(..))

parseBreak :: ReadP Html
parseBreak = do
    void $ Parser.char '\n'
    return HTML.br

parseCategory :: ReadP Html
parseCategory = Parser.choice [parseShippuden, parseReanimated]
  where
      parseShippuden = do
          void $ Parser.string " (S)"
          return $ HTML.toMarkup Shippuden
      parseReanimated = do
          void $ Parser.string " (R)"
          return $ HTML.toMarkup Reanimated


parseChakra :: Chakra -> ReadP Html
parseChakra kind = do
    void $ Parser.string ['[', token kind, ']']
    return $ HTML.toMarkup kind
  where
    token Blood = 'b'
    token Gen   = 'g'
    token Nin   = 'n'
    token Tai   = 't'
    token Rand  = 'r'

parseChakras :: ReadP Html
parseChakras = Parser.choice $ parseChakra <$> [minBound .. maxBound]

parseName :: ReadP Html
parseName = do
    void $ Parser.char '['
    name <- Parser.munch1 $ (/= ']')
    void $ Parser.char ']'
    return . HTML.i $ HTML.toMarkup name

parseText :: ReadP Html
parseText = HTML.toMarkup <$> Parser.munch1 continue
  where
    continue ' '  = False
    continue '['  = False
    continue '\n' = False
    continue _    = True

parseSegment :: ReadP Html
parseSegment = parseChakras
               <++ parseCategory
               <++ Parser.choice [parseBreak, parseName, parseText]
               <++ (HTML.toMarkup <$> Parser.char ' ')

parseDesc :: ReadP Html
parseDesc = mconcat <$> Parser.manyTill parseSegment Parser.eof

desc :: Text -> Html
desc s = case Parser.readP_to_S parseDesc $ unpack s of
    [(html, "")] -> html
    _            -> HTML.toMarkup s
