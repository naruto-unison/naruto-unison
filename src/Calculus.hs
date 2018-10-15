{-# LANGUAGE RankNTypes #-}

-- | Miscellaneous simple functions.
module Calculus where

import qualified Data.List     as List
import qualified System.Random as Random
import qualified Data.Text     as Text

import StandardLibrary hiding (and)
import Data.Foldable (and)

absmin :: ∀ a. (Ord a, Num a) => a -> a -> a
absmin _ 0 = 0
absmin x y
  | abs x <= abs y = x
  | otherwise      = y

-- | If @False@, turns a 'Maybe' into 'Nothing'.
($?) :: ∀ a. Bool -> Maybe a -> Maybe a
False $? _ = Nothing
True  $? x = x

-- | Equality by applying a function to both arguments.
-- Goes well with 'andOn', e.g. @andOn [eqs recordFieldA,  recordFieldB]@.
eqs :: ∀ a b. Eq b => (a -> b) -> a -> a -> Bool
eqs = on (==)

-- | Lists all members of an 'Enum' from 'minBound' to 'maxBound'.
enums :: ∀ a. (Bounded a, Enum a) => [a]
enums = [minBound .. maxBound]

-- | Apply the same two arguments to a list of functions and 'and' the result.
-- Goes well with 'eq', e.g. @andOn [eqs recordFieldA,  recordFieldB]@.
andOn :: ∀ f a. (Foldable f, Functor f) => f (a -> a -> Bool) -> a -> a -> Bool 
andOn fs x y = and $ ($ y) . ($ x) <$> fs

-- | @Text@ 'T.init' that returns @""@ if given @""@.
tInit :: Text -> Text
tInit "" = ""
tInit a  = Text.init a

-- | @Text@ 'T.tail' that returns @""@ if given @""@.
tTail :: Text -> Text
tTail "" = ""
tTail a  = Text.tail a

-- | Second argument if 'True', otherwise 'id'.
doIf :: ∀ a. Bool -> (a -> a) -> a -> a
doIf True  f = f
doIf False _ = id

-- | 'doIf'
infixr 0 ?
(?) :: ∀ a. Bool -> (a -> a) -> (a -> a)
True  ? f = f
False ? _ = id

-- | 'fst' if 'True', 'snd' if 'False'.
out2 :: ∀ a. Bool -> ((a, a) -> a)
out2 True  = fst
out2 False = snd

-- | Sets first if 'True', second if 'False'.
in2 :: ∀ a. Bool -> a -> (a, a) -> (a, a)
in2 True  x (_, y) = (x, y)
in2 False y (x, _) = (x, y)

-- | Applies a function to first if 'True', second if 'False'.
do2 :: ∀ a. Bool -> (a -> a) -> (a, a) -> (a, a)
do2 True  f (x, y) = (f x, y)
do2 False f (x, y) = (x, f y)

-- | Applies a function to both.
both :: ∀ a b. (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | List contains duplicates.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic x = nub x /= x

-- | Removes spaces and special characters.
shorten :: Text -> Text
shorten = omap shorten' . filter (`notElem` filterOut)
  where 
    filterOut    = " -:()®'/?" :: String
    shorten' 'ō' = 'o'
    shorten' 'Ō' = 'O'
    shorten' 'ū' = 'u'
    shorten' 'Ū' = 'U'
    shorten' 'ä' = 'a'
    shorten'  a  =  a

-- | Chooses an element of a list at random.
pick :: ∀ a. Random.StdGen -> [a] -> (Maybe a, Random.StdGen)
pick stdGen [] = (Nothing, stdGen)
pick stdGen xs = (Just $ xs List.!! i, stdGen')
  where (i, stdGen') = Random.randomR (0, length xs - 1) stdGen

-- | Left equivalent of 'unfoldr' that ends when it reaches a 'Nothing'.
-- Used with `Random.split`.
{-# INLINE unfoldl #-} 
unfoldl :: ∀ a. (a -> (a, a)) -> a -> NonEmpty a
unfoldl f b0 = y <| unfoldl f x
  where (x, y) = f b0

-- | True if any elements are shared by both collections.
intersects :: ∀ o. (MonoFoldable o, Eq (Element o)) => o -> o -> Bool
intersects x = any (`elem` x)
