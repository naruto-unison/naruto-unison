-- | Various unicode synonyms for basic functions.
module Core.Unicode
    ( module Import
    , (↦), (↤), (∈), (∉), (▷), (⧺), (◁), (⩀), (÷), (∘), (↤∘), (٪), (—), ø
    ) where

import Prelude.Unicode       as Import hiding ((¬), (÷), (∘), (∈), (∉), (⧺)) 
import Control.Monad.Unicode as Import
import Data.List.Unicode     as Import hiding ((∈), (∉), (⧺))

import Data.Monoid   (Monoid, (<>), mempty)
import Data.List     (intersect)
import Data.Sequence (Seq, (|>), (<|))

-- | 'elem'
infix 4 ∈
(∈) ∷ (Foldable a, Eq b) ⇒ b → a b → Bool
(∈) = elem
{-# INLINE (∈) #-}

-- | 'notElem'
infix 4 ∉
(∉) ∷ (Foldable a, Eq b) ⇒ b → a b → Bool
(∉) = notElem
{-# INLINE (∉) #-}

-- | '|>'
infixl 5 ▷
(▷) ∷ Seq a → a → Seq a
(▷) = (|>)
{-# INLINE (▷) #-}

-- | '<|'
infixr 5 ◁
(◁) ∷ a → Seq a → Seq a
(◁) = (<|)
{-# INLINE (◁) #-}

infixr 5 ⧺
(⧺) ∷ Monoid m ⇒ m → m → m
(⧺) = (<>)
{-# INLINE (⧺) #-}

-- | 'not' . 'null' . 'intersect'
infix 6 ⩀
(⩀) ∷ Eq a ⇒ [a] → [a] → Bool
(⩀) a b = not ∘ null $ intersect a b

-- | @'reverse' . '<$>'@
infixl 6 ↦
(↦) ∷ Functor f ⇒ f a → (a → b) → f b
(↦) = flip (<$>)
{-# INLINE (↦) #-}

-- | '<$>'
infixl 6 ↤
(↤) ∷ Functor f ⇒ (a → b) → f a → f b
(↤) = (<$>)
{-# INLINE (↤) #-}

-- | 'quot'
infixl 7 ÷
(÷) ∷ Integral a ⇒ a → a → a
(÷) = quot
{-# INLINE (÷) #-}

-- | '.'
infixr 7 ∘ 
(∘) ∷ (b → g) → (a → b) → a → g
(∘) = (.)
{-# INLINE (∘) #-}

infixr 7 ↤∘
(↤∘) ∷ Functor f ⇒ (a → b) → (c → f a) → c → f b
f ↤∘ g = fmap f ∘ g

-- | 'mod'
infix 7 ٪
(٪) ∷ Integral a ⇒ a → a → a
(٪) = mod
{-# INLINE (٪) #-}

-- | 'subtract' allowing for sections
(—) ∷ Num a ⇒ a → a → a
(—) = (-) 

-- | 'mempty'
ø ∷ Monoid a ⇒ a
ø = mempty
