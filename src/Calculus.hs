{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RankNTypes #-}

module Calculus where

import qualified Data.Sequence as S
import qualified Data.Text     as T

import Data.Function
import Data.List
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text     (pack, Text)
import System.Random

import Core.Unicode

absmin ∷ (Ord a, Num a) ⇒ a → a → a
absmin a b
  | b ≡ 0 ∨ abs a ≤ abs b = a
  | otherwise             = b

($?) ∷ Bool → Maybe a → Maybe a
False $? _ = Nothing
True  $? a = a

eqs ∷ Eq b ⇒ (a → b) → a → a → Bool
eqs = on (≡)

f2 ∷ a → b → [a → b → c] → [c]
f2 a b = map $ ($ b) ∘ ($ a)

f2all ∷ [a → a → Bool] → a → a → Bool
f2all f a b = and $ f2 a b f

safeTail ∷ [a] → [a]
safeTail [] = []
safeTail xs = tail xs

tInit ∷ Text → Text
tInit "" = ""
tInit a  = T.init a

tTail ∷ Text → Text
tTail "" = ""
tTail a  = T.tail a

doIf ∷ Bool → (a → a) → a → a
doIf True  f a = f a
doIf False _ a = a

infixr 8 ?
(?) ∷ Bool → (a → a) → (a → a)
True  ? f = f
False ? _ = id

-- | fst if True, snd if Falsel
out2 ∷ Bool → ((a, a) → a)
out2 True  = fst
out2 False = snd

-- | fst if True, snd if Falsel
in2 ∷ Bool → b → (b, b) → (b, b)
in2 True  a (_, b) = (a, b)
in2 False b (a, _) = (a, b)

-- | fst if True, snd if Falsel
do2 ∷ Bool → (a → a) → (a, a) → (a, a)
do2 True  f (a, b) = (f a, b)
do2 False f (a, b) = (a, f b)

both ∷ (a → b) → (a, a) → (b, b)
both f (a, b) = (f a, f b)

map2 ∷ (a → b) → [(a, a)] → [(b, b)]
map2 = map ∘ both

duplic ∷ Eq a ⇒ [a] → Bool
duplic a = nub a ≠ a

tshow ∷ Show a ⇒ a → Text
tshow = pack ∘ show

seqConcat ∷ Foldable a ⇒ a (Seq b) → Seq b
seqConcat = foldl' (◇) ø

seqMap ∷ (a → Seq b) → Seq a → Seq b
seqMap f = seqConcat ∘ (f <$>)

seqMaybe ∷ Seq (Maybe a) → Seq a
seqMaybe = (fromJust <$>) ∘ S.filter (isJust)

tryJust ∷ Monad m ⇒ m a → Maybe a → m a
tryJust ifNothing = maybe ifNothing return

deleteOne ∷ (a → Bool) → [a] → [a]
deleteOne predicate xs = before ⧺ safeTail after
  where (before, after) = break predicate xs

pick ∷ StdGen → [a] → (a, StdGen)
pick stdGen xs = (xs !! i, stdGen')
  where (i, stdGen') = randomR (0, length xs - 1) stdGen

safePick ∷ StdGen → [a] → (Maybe a, StdGen)
safePick stdGen [] = (Nothing, stdGen)
safePick stdGen xs = (Just $ xs !! i, stdGen')
  where (i, stdGen') = randomR (0, length xs - 1) stdGen

{-# INLINE unfoldl #-} 
unfoldl ∷ (a → Maybe (a, a)) → a → [a] 
unfoldl f b0 = case f b0 of
                  Just (a, b) → b : unfoldl f a
                  Nothing     → []