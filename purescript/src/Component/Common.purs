module Component.Common 
  ( ArrayOp(..)
  , ChildQuery(..)
  , Icon
  , QueueType(..)
  , SelectQuery(..)
  , PlayQuery(..)
  , Previewing(..)
  , SocketMsg(..)
  , Viewable(..)
  , hCost
  , parseDesc
  , charName
  , cIcon
  , _a, _b, _c, _i, _span, _src, _style, _txt, _minor
  ) where

import Prelude

import Halogen.HTML            as H
import Halogen.HTML.Properties as P

import Data.Array              ((:))
import Data.Function.Memoize   (memoize)
import Data.Maybe              
import Data.String 
import Halogen                 (HTML)
import Halogen.HTML            (ClassName(..))
import Halogen.HTML.Properties (IProp)

import Operators 
import Structure 
import Functions
import Info      

newtype SocketMsg = SocketMsg String

data ChildQuery a = QuerySelect SelectQuery a 
                  | QueryPlay   PlayQuery   a

data SelectQuery = SwitchLogin                    
                 | Scroll Int               
                 | Preview Previewing  
                 | Team ArrayOp Character 
                 | Enqueue QueueType  
                 | Vary Int Int        
                 | ChooseAvatar String     
  
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
             
data Previewing = NoPreview 
                | PreviewUser
                | PreviewCharacter Character  

data Viewable = ViewBarrier   Barrier
              | ViewCharacter Character
              | ViewDefense   Defense
              | ViewInfo      (Effect → Boolean) Info
              | ViewSkill     Int (Array Int) Skill
              | ViewUser      User

hCost ∷ ∀ a b. Chakras → Array (HTML a b)
hCost = hCost' ↤∘ unχ
  where hCost' s = H.div [_c $ "chakra " ⧺ s] []

parseDesc ∷ ∀ a b. String → Array (HTML a b)
parseDesc = memoize parseBefore'
  where parseBefore' str = parseBefore before (drop 1 after)
          where {before, after} = splitBy (Pattern "[") str
        parseBefore "" "" = []
        parseBefore "" b  = parseAfter' b
        parseBefore a ""  = [H.text a]
        parseBefore a b   = H.text a : parseAfter' b
        parseAfter' b     = parseAfter before (drop 1 after)
          where {before, after} = splitBy (Pattern "]") b
        parseAfter "" ""  = []
        parseAfter "" b   = parseAfter' b
        parseAfter b ""  = [H.text b]
        parseAfter "b" b = H.div [_c "chakra blood"] [] : parseBefore' b
        parseAfter "g" b = H.div [_c "chakra gen"]   [] : parseBefore' b
        parseAfter "n" b = H.div [_c "chakra nin"]   [] : parseBefore' b
        parseAfter "t" b = H.div [_c "chakra tai"]   [] : parseBefore' b
        parseAfter "r" b = H.div [_c "chakra rand"]  [] : parseBefore' b
        parseAfter a b   = H.em_ [H.text a] : parseBefore' b

splitBy ∷ Pattern → String → { before ∷ String, after ∷ String }
splitBy p s = fromMaybe { before: s, after: ""} do
    i ← indexOf p s
    splitAt i s

sillySplit ∷ Pattern → String → { before ∷ String, after ∷ String }
sillySplit p s = case split p s of
    [a, b] → { before: a, after: b  }
    _      → { before: s, after: "" }

charName' ∷ ∀ a b. String → String → Array (HTML a b)
charName' a "R)" = [H.text a, _minor "ℝ"]
charName' a "S)" = [H.text a, _minor "𝕊"]
charName' a _    = [H.text a]
  
charName ∷ ∀ a b. Character → Array (HTML a b)
charName (Character {characterName}) = charName' before after
  where {before, after} = sillySplit (Pattern " (") characterName

type Icon = ∀ a b. String → IProp (src ∷ String | b) a

cIcon ∷ Character → Icon
cIcon (Character {characterName}) = memoize $ \a → P.src 
                      $ "/img/ninja/" ⧺ shorten characterName ⧺ "/" 
                      ⧺ shorten' a ⧺ ".jpg"
  where shorten' = shorten ∘ takeWhile ('(' ≠ _)
  

_i   ∷ ∀ a b. String → IProp (id ∷ String | b) a 
_i   = P.id_
_c   ∷ ∀ a b. String → IProp (class ∷ String | b) a
_c   = P.class_ ∘ ClassName
_src ∷ ∀ a b. String → IProp (src ∷ String | b) a
_src = P.src
_style ∷ ∀ a b. String → IProp (style ∷ String | b) a
_style = P.attr (H.AttrName "style")

_txt ∷ ∀ a b. String → Array (HTML a b)
_txt = (_ : []) ∘ H.text

_a ∷ ∀ a b. String → String → String → String → HTML a b
_a id' class' href' = H.a [_i id', _c class', P.href href'] ∘ _txt

_b ∷ ∀ a b. String → HTML a b
_b = H.b_ ∘ _txt

_span ∷ ∀ a b. String → HTML a b
_span = H.span_ ∘ _txt


_minor ∷ ∀ a b. String → HTML a b
_minor = H.span [_c "minor"] ∘ _txt
