module Handler.Parse (desc) where

import ClassyPrelude
import Yesod

import qualified Text.Blaze.Html5 as HTML
import qualified Text.ParserCombinators.ReadP as Parser
import           Text.ParserCombinators.ReadP (ReadP)

import Model.Chakra (Chakra(..))

parseBreak :: ReadP Html
parseBreak = do
    void $ Parser.char '\n'
    return HTML.br

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

parseName :: ReadP Html
parseName = do
    void $ Parser.char '['
    name <- Parser.munch1 $ (/= ']')
    guard . not . null $ drop 1 name
    void $ Parser.char ']'
    return . HTML.i $ HTML.toMarkup name

parseText :: ReadP Html
parseText =
    HTML.toMarkup <$> Parser.munch1 continue
  where
    continue '['  = False
    continue '\n' = False
    continue _    = True

parseDesc :: ReadP Html
parseDesc = (mconcat <$>) . flip Parser.manyTill Parser.eof . Parser.choice $
    (parseChakra <$> [minBound .. maxBound]) ++
    [parseBreak, parseName, parseText]

desc :: Text -> Html
desc s = case Parser.readP_to_S parseDesc $ unpack s of
    [(html, "")] -> html
    _            -> HTML.toMarkup s
