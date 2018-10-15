module Site.Common where

import StandardLibrary
import Affjax                  as Affjax
import Generic                 as G
import Halogen.HTML            as H
import Foreign.JSON            as JSON
import Halogen.HTML.Properties as P
import Affjax.ResponseFormat   as ResponseFormat
import Data.String             as String

import Halogen.HTML (HTML, ClassName(..))
import Halogen.HTML.Properties (IProp)

import FFI.Import
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
unJson = mapLeft showForeignErrors <<< runExcept <<< 
         JSON.decodeJSONWith (G.decode)

getJson :: ∀ a. G.Decode a => String -> Aff (Either String a)
getJson url = do
    {body} <- Affjax.get ResponseFormat.string url
    pure $ mapLeft Affjax.printResponseFormatError body 
       >>= mapLeft showForeignErrors <<< runExcept <<< 
           JSON.decodeJSONWith (G.decode)


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
        parseBefore "" y  = parseAfter' y
        parseBefore x ""  = [H.text x]
        parseBefore x y   = H.text x : parseAfter' y
        parseAfter' y     = parseAfter before (String.drop 1 after)
          where 
            {before, after} = splitBy (Pattern "]") y
        parseAfter "" ""  = []
        parseAfter "" y   = parseAfter' y
        parseAfter x ""  = [H.text x]
        parseAfter "b" y = H.div [_c "chakra blood"] [] : parseBefore' y
        parseAfter "g" y = H.div [_c "chakra gen"]   [] : parseBefore' y
        parseAfter "n" y = H.div [_c "chakra nin"]   [] : parseBefore' y
        parseAfter "t" y = H.div [_c "chakra tai"]   [] : parseBefore' y
        parseAfter "r" y = H.div [_c "chakra rand"]  [] : parseBefore' y
        parseAfter x y   = H.em_ [H.text x] : parseBefore' y

splitBy :: Pattern -> String -> { before :: String, after :: String }
splitBy p s = fromMaybe { before: s, after: ""} do
    i <- String.indexOf p s
    pure $ String.splitAt i s

charName :: ∀ a b. Character -> Array (HTML a b)
charName (Character c) = case c.characterGroup of
    Original   -> [H.text c.characterName]
    Shippuden  -> [H.text c.characterName, _minor "𝕊"]
    Reanimated -> [H.text c.characterName, _minor "ℝ"]

cIcon :: ∀ a b. Character -> (String -> IProp (src :: String | b) a)
cIcon = memoize go <<< show 
  where 
    go characterName x = P.src $
                         "/img/ninja/" <> shorten characterName <> "/" 
                         <> shorten' x <> ".jpg"
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
