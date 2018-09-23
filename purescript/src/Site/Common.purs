module Site.Common where

import StandardLibrary
import Affjax                  as Affjax
import Foreign                 as Foreign
import Generic                 as G
import Halogen.HTML            as H
import Data.List               as List
import Data.List.Types         as ListTypes
import Foreign.JSON            as JSON
import Halogen.HTML.Properties as P
import Affjax.ResponseFormat   as ResponseFormat
import Data.String             as String

import Halogen.HTML (HTML, ClassName(..))
import Halogen.HTML.Properties (IProp)

import Database.Structure
import Database.Functions
import Database.Info 

type HTMLQ a = HTML a (ChildQuery Unit)

newtype JSONFail = JSONFail { message :: String 
                            , errors  :: Array String
                            }
derive instance genericJSONFail :: G.Generic JSONFail _

newtype SocketMsg = SocketMsg String

data ChildQuery a = QuerySelect SelectQuery a 
                  | QueryPlay   PlayQuery   a

data SelectQuery = SwitchLogin                    
                 | Scroll Int               
                 | Preview Previewing  
                 | Untoggle
                 | Team ArrayOp Character 
                 | Enqueue QueueType  
                 | Vary Int Int        
                 | ChooseAvatar String
                 | TryUpdate

data ArrayOp = Add | Delete

data QueueType = Quick | Practice | Private

data PlayQuery = Enact ArrayOp Act
               | ExchangeBegin 
               | ExchangeConclude Chakras
               | ExchangeReset
               | Ready Boolean String
               | ReceiveGame Game
               | Spend Chakras
               | Toggle Act
               | Unhighlight
               | View Viewable
             
data Previewing = NoPreview | PreviewUser User | PreviewChar Character  

data Viewable = ViewBarrier   Barrier
              | ViewCharacter Character
              | ViewDefense   Defense
              | ViewInfo      (SkillEffect -> Boolean) Info
              | ViewSkill     Int (Array Int) Int Skill
              | ViewUser      User

unJson :: ∀ a. G.Decode a => String -> Either String a
unJson = mapLeft show <<< runExcept <<< JSON.decodeJSONWith (G.decode)

getJson :: ∀ a. G.Decode a => String -> Aff (Either String a)
getJson url = do
    {body} <- Affjax.get ResponseFormat.string url
    pure $ mapLeft Affjax.printResponseFormatError body 
       >>= mapLeft showErrors <<< runExcept <<< JSON.decodeJSONWith (G.decode)
  where
    showErrors = String.joinWith "\n" <<< map Foreign.renderForeignError <<< 
                 List.toUnfoldable <<< ListTypes.toList


hCost :: ∀ a b. Chakras -> Array (HTML a b)
hCost = map hCost' <<< unχ
  where hCost' s = H.div [_c $ "chakra " <> s] []

parseDesc :: ∀ a b. String -> Array (HTML a b)
parseDesc = memoize parseBefore'
  where 
    parseBefore' str = parseBefore before (String.drop 1 after)
      where 
        {before, after} = splitBy (Pattern "[") str
        parseBefore "" "" = []
        parseBefore "" b  = parseAfter' b
        parseBefore a ""  = [H.text a]
        parseBefore a b   = H.text a : parseAfter' b
        parseAfter' b     = parseAfter before (String.drop 1 after)
          where 
            {before, after} = splitBy (Pattern "]") b
        parseAfter "" ""  = []
        parseAfter "" b   = parseAfter' b
        parseAfter b ""  = [H.text b]
        parseAfter "b" b = H.div [_c "chakra blood"] [] : parseBefore' b
        parseAfter "g" b = H.div [_c "chakra gen"]   [] : parseBefore' b
        parseAfter "n" b = H.div [_c "chakra nin"]   [] : parseBefore' b
        parseAfter "t" b = H.div [_c "chakra tai"]   [] : parseBefore' b
        parseAfter "r" b = H.div [_c "chakra rand"]  [] : parseBefore' b
        parseAfter a b   = H.em_ [H.text a] : parseBefore' b

splitBy :: Pattern -> String -> { before :: String, after :: String }
splitBy p s = fromMaybe { before: s, after: ""} do
    i <- String.indexOf p s
    pure $ String.splitAt i s

charName :: ∀ a b. Character -> Array (HTML a b)
charName = go <<< sillySplit (Pattern " (") <<< show
  where
    sillySplit p s = case String.split p s of
        [a, b] -> { before: a, after: b  }
        _      -> { before: s, after: "" }
    go {before, after} = case after of
        "R)" -> [ H.text before, _minor "ℝ" ]
        "S)" -> [ H.text before, _minor "𝕊" ]
        _    -> [ H.text before ]

cIcon :: ∀ a b. Character -> (String -> IProp (src :: String | b) a)
cIcon = memoize go <<< show 
  where 
    go characterName a = P.src $
                         "/img/ninja/" <> shorten characterName <> "/" 
                         <> shorten' a <> ".jpg"
    shorten' = shorten <<< 
               String.takeWhile (_ /= String.codePointFromChar '(')
  
_i   :: ∀ a b. String -> IProp (id :: String | b) a 
_i   = P.id_
_c   :: ∀ a b. String -> IProp (class :: String | b) a
_c   = P.class_ <<< ClassName
_src :: ∀ a b. String -> IProp (src :: String | b) a
_src = P.src
_style :: ∀ a b. String -> IProp (style :: String | b) a
_style = P.attr (H.AttrName "style")

_txt :: ∀ a b. String -> Array (HTML a b)
_txt = (_ : []) <<< H.text

_a :: ∀ a b. String -> String -> String -> String -> HTML a b
_a id' class' href' = H.a [_i id', _c class', P.href href'] <<< _txt

_b :: ∀ a b. String -> HTML a b
_b = H.b_ <<< _txt

_span :: ∀ a b. String -> HTML a b
_span = H.span_ <<< _txt

_extra :: ∀ a b. String -> HTML a b
_extra = H.span [_c "extra"] <<< _txt

_minor :: ∀ a b. String -> HTML a b
_minor = H.span [_c "minor"] <<< _txt
