{-# LANGUAGE RankNTypes #-}

-- | Miscellaneous simple functions.
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

-- | If @False@, turns a 'Maybe' into 'Nothing'.
($?) ∷ Bool → Maybe a → Maybe a
False $? _ = Nothing
True  $? a = a

-- | Equality by applying a function to both arguments.
-- Goes well with 'f2all', e.g. @f2all [eqs recordFieldA,  recordFieldB]@.
eqs ∷ Eq b ⇒ (a → b) → a → a → Bool
eqs = on (≡)

-- | Applies two arguments to a list of functions that accept two arguments.
f2 ∷ a → b → [a → b → c] → [c]
f2 a b = map $ ($ b) ∘ ($ a)

-- | 'and' 'f2'. 
-- Goes well with 'f2all', e.g. @f2all [eqs recordFieldA,  recordFieldB]@.
f2all ∷ [a → a → Bool] → a → a → Bool
f2all f a b = and $ f2 a b f

-- | @Text@ 'T.init' that returns @""@ if given @""@.
tInit ∷ Text → Text
tInit "" = ""
tInit a  = T.init a

-- | @Text@ 'T.tail' that returns @""@ if given @""@.
tTail ∷ Text → Text
tTail "" = ""
tTail a  = T.tail a

-- | Second argument if 'True', otherwise 'id'.
doIf ∷ Bool → (a → a) → a → a
doIf True  f = f
doIf False _ = id

-- | 'doIf'
infixr 8 ?
(?) ∷ Bool → (a → a) → (a → a)
True  ? f = f
False ? _ = id

-- | 'fst' if 'True', 'snd' if 'False'.
out2 ∷ Bool → ((a, a) → a)
out2 True  = fst
out2 False = snd

-- | Sets first if 'True', second if 'False'.
in2 ∷ Bool → b → (b, b) → (b, b)
in2 True  a (_, b) = (a, b)
in2 False b (a, _) = (a, b)

-- | Applies a function to first if 'True', second if 'False'.
do2 ∷ Bool → (a → a) → (a, a) → (a, a)
do2 True  f (a, b) = (f a, b)
do2 False f (a, b) = (a, f b)

-- | Applies a function to both.
both ∷ (a → b) → (a, a) → (b, b)
both f (a, b) = (f a, f b)

-- | @'map' . 'both'@.
map2 ∷ (a → b) → [(a, a)] → [(b, b)]
map2 = map ∘ both

-- | List contains duplicates.
duplic ∷ Eq a ⇒ [a] → Bool
duplic a = nub a ≠ a

-- | @'pack' . 'show'@
tshow ∷ Show a ⇒ a → Text
tshow = pack ∘ show

-- | 'Seq' concatenation.
seqConcat ∷ Foldable a ⇒ a (Seq b) → Seq b
seqConcat = foldl' (◇) ø

-- | @'seqConcat' . '<$>'@
seqMap ∷ (a → Seq b) → Seq a → Seq b
seqMap f = seqConcat ∘ (f <$>)

-- | 'Seq' equivalent of 'catMaybes'.
seqMaybes ∷ Seq (Maybe a) → Seq a
seqMaybes = (fromJust <$>) ∘ S.filter isJust

-- | Extracts a 'Just' in a monad.
-- Returns the error message argument if given 'Nothing'.
tryJust ∷ Monad m ⇒ m a → Maybe a → m a
tryJust ifNothing = maybe ifNothing return

-- | Deletes first element that matches a predicate.
deleteOne ∷ (a → Bool) → [a] → [a]
deleteOne predicate xs = before ⧺ drop 1 after
  where (before, after) = break predicate xs

-- | Chooses an element of a list at random.
pick ∷ StdGen → [a] → (a, StdGen)
pick stdGen xs = (xs !! i, stdGen')
  where (i, stdGen') = randomR (0, length xs - 1) stdGen

-- | 'Nothing' if given an empty list, otherwise 'Just' 'pick'.
safePick ∷ StdGen → [a] → (Maybe a, StdGen)
safePick stdGen [] = (Nothing, stdGen)
safePick stdGen xs = (Just $ xs !! i, stdGen')
  where (i, stdGen') = randomR (0, length xs - 1) stdGen

-- | Left equivalent of 'unfoldr' that ends when it reaches a 'Nothing'.
{-# INLINE unfoldl #-} 
unfoldl ∷ (a → Maybe (a, a)) → a → [a] 
unfoldl f b0 = case f b0 of
                  Just (a, b) → b : unfoldl f a
                  Nothing     → []
