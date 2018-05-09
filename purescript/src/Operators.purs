module Operators where

import Prelude   
 
import Data.Array   (difference, elem, notElem, intersect)
import Data.Functor (mapFlipped)
import Data.Maybe   (Maybe(..))
import Data.Ord     (greaterThanOrEq)

infix  0 justIf          as ??
infixl 1 bind            as ≫=
infixl 1 advance         as ≫
infixr 2 disj            as ∨
infixr 3 conj            as ∧
infix  4 eq              as ≡
infix  4 notEq           as ≠
infix  4 elem            as ∈
infix  4 notElem         as ∉
infixl 4 greaterThanOrEq as ≥
infix  5 difference      as ∖
infixr 5 append          as ⧺
infixr 6 intersect       as ∩
infixr 7 mod             as %
infixl 7 mapFlipped      as ↦
infixl 8 map             as ↤
infixr 9 doIf            as ?
infixr 9 compose         as ∘
infixr 9 multimap        as ↤∘

doIf ∷ ∀ a. Boolean → (a → a) → (a → a)
doIf true  = id
doIf false = const id

multimap :: ∀ a f b c. Functor f ⇒ (c → b) → (a → f c) → a → f b
multimap f = (map f ∘ _)

justIf ∷ ∀ a. Boolean → a → Maybe a
justIf true = Just
justIf false = const Nothing

advance ∷ ∀ a b c. Bind a ⇒ Applicative a ⇒ a c → b → a b
advance a b = a ≫= \_ → pure b
