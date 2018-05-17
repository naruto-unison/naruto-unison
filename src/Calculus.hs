{-# LANGUAGE RankNTypes #-}

-- | Miscellaneous simple functions.
module Calculus where

import qualified Data.Text     as T

import Preludesque hiding ((!!))
import Data.List ((!!))
import Data.List.NonEmpty ((<|))
import Data.Function
import Data.Text     (Text, pack)
import System.Random

absmin ∷ Ord a ⇒ Num a ⇒ a → a → a
absmin a b
  | b ≡ 0 ∨ abs a ≤ abs b = a
  | otherwise             = b

-- | If @False@, turns a 'Maybe' into 'Nothing'.
($?) ∷ Bool → Maybe a → Maybe a
False $? _ = Nothing
True  $? a = a

-- | Equality by applying a function to both arguments.
-- Goes well with 'andOn', e.g. @andOn [eqs recordFieldA,  recordFieldB]@.
eqs ∷ Eq b ⇒ (a → b) → a → a → Bool
eqs = on (≡)

-- | Lists all members of an 'Enum' from 'minBound' to 'maxBound'.
enums ∷ Bounded a ⇒ Enum a ⇒ [a]
enums = [minBound .. maxBound]

-- | Apply the same two arguments to a list of functions and 'and' the result.
-- Goes well with 'eq', e.g. @andOn [eqs recordFieldA,  recordFieldB]@.
andOn ∷ Foldable a ⇒ Functor a ⇒ a (b → b → Bool) → b → b → Bool
andOn fs a b = and $ (($ b) ∘ ($ a)) ↤ fs

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

-- | List contains duplicates.
duplic ∷ Eq a ⇒ [a] → Bool
duplic a = nub a ≠ a

-- | @'pack' . 'show'@
tshow ∷ Show a ⇒ a → Text
tshow = pack ∘ show

-- | Removes spaces and special characters.
shorten ∷ Text → Text
shorten = T.map shorten' ∘ T.filter (∉ filterOut)
  where filterOut    = " -:()®'/?" ∷ String
        shorten' 'ō' = 'o'
        shorten' 'Ō' = 'O'
        shorten' 'ū' = 'u'
        shorten' 'Ū' = 'U'
        shorten' 'ä' = 'a'
        shorten'  a  =  a

-- | Extracts a 'Just' in a monad.
-- Returns the error message argument if given 'Nothing'.
tryJust ∷ Monad m ⇒ m a → Maybe a → m a
tryJust ifNothing = maybe ifNothing return

-- | Deletes first element that matches a predicate.
deleteOne ∷ (a → Bool) → [a] → [a]
deleteOne predicate xs = before ⧺ drop 1 after
  where (before, after) = break predicate xs

-- | Chooses an element of a list at random.
pick ∷ StdGen → [a] → (Maybe a, StdGen)
pick stdGen [] = (Nothing, stdGen)
pick stdGen xs = (Just $ xs !! i, stdGen')
  where (i, stdGen') = randomR (0, length xs - 1) stdGen

-- | Left equivalent of 'unfoldr' that ends when it reaches a 'Nothing'.
{-# INLINE unfoldl #-} 
unfoldl ∷ (a → (a, a)) → a → NonEmpty a
unfoldl f b0 = b <| unfoldl f a
  where (a, b) = f b0
