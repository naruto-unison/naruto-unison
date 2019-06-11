module Site.Common where

import Prelude
import Data.Array ((:))
import Generic as G
import Halogen.HTML as H
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties (IProp)

import Model (Act, Barrier, Defense)
import Model.Character (Character)
import Model.Chakra (Chakras)
import Model.Game (Game)
import Model.Skill (Skill)
import Model.Status (Effect)
import Model.User (User)
import Model.Info (Info)

type HTMLQ a = HTML a (ChildQuery Unit)

newtype JSONFail = JSONFail { message :: String
                            , errors  :: Array String
                            }
derive instance genericJSONFail :: G.Generic JSONFail _

newtype SocketMsg = SocketMsg String

data ChildQuery a
    = QuerySelect SelectQuery a
    | QueryPlay   PlayQuery   a

data SelectQuery
    = SwitchLogin
    | Scroll Int
    | Preview Previewing
    | Untoggle
    | Team ArrayOp Character
    | Enqueue QueueType
    | Vary Int Int
    | ChooseAvatar String
    | TryUpdate

data ArrayOp
    = Add
    | Delete

data QueueType
    = Quick
    | Practice
    | Private

data PlayQuery
    = Enact ArrayOp Act
    | ExchangeBegin
    | ExchangeConclude Chakras
    | ExchangeReset
    | Ready Boolean String
    | ReceiveGame Game
    | Spend Chakras
    | Toggle Act
    | Unhighlight
    | View Viewable

data Previewing
    = NoPreview
    | PreviewUser User
    | PreviewChar Character

data Viewable
    = ViewBarrier   Barrier
    | ViewCharacter Character
    | ViewDefense   Defense
    | ViewInfo      (Effect -> Boolean) Info
    | ViewSkill     Int (Array Int) Int Skill
    | ViewUser      User

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
