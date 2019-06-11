module Core.Util where

import ClassyPrelude.Yesod hiding ((<|))
import qualified Data.List as List
import qualified Data.Text as Text

-- | 'elem'.
(∈) :: ∀ o. MonoFoldable o => Eq (Element o) => Element o -> o -> Bool
(∈) = elem

-- | 'notElem'.
(∉) :: ∀ o. MonoFoldable o => Eq (Element o) => Element o -> o -> Bool
(∉) = notElem

-- | '-' allowing for sections.
(—) :: ∀ a. Num a => a -> a -> a
(—) = (-)

-- | Finds the value with lesser magnitude.
absmin :: ∀ a. (Ord a, Num a) => a -> a -> a
absmin _ 0 = 0
absmin x y
  | abs x <= abs y = x
  | otherwise = y

-- | True if a list contains multiple identical values.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic x = List.nub x /= x

-- | Lists all members of an 'Enum' from 'minBound' to 'maxBound'.
enumerate :: ∀ a. (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

-- | Equality with a projecting function. Analogous to 'comparing'.
equaling :: ∀ a b. Eq b => (a -> b) -> a -> a -> Bool
equaling f x y = f x == f y

-- | Adds 1 to positives, subtracts 1 from negatives, and leaves 0s unchanged.
incr :: Int -> Int
incr x
  | x < 0     = x - 1
  | x > 0     = x + 1
  | otherwise = x

-- | True if any elements are shared by both collections.
intersects :: ∀ o. (MonoFoldable o, Eq (Element o)) => o -> o -> Bool
intersects x = any (∈ x)

mapMaybe :: ∀ (f :: * -> *) a b.
            ( IsSequence (f (Maybe b))
            , Functor f
            , Element (f (Maybe b)) ~ Maybe b
            )
         => (a -> Maybe b) -> f a -> f b
mapMaybe f = catMaybes . (f <$>)

-- | Removes spaces and special characters.
shorten :: Text -> Text
shorten = omap f . filter (∉ bans)
  where
    bans  = " -:()®'/?" :: String
    f 'ō' = 'o'
    f 'Ō' = 'O'
    f 'ū' = 'u'
    f 'Ū' = 'U'
    f 'ä' = 'a'
    f a = a

-- Converts from turns to sub-turns.
-- Each turn consists of two sub-turns, one for each player.
sync :: Int -> Int
sync n
  | n >= 0    = 2 * n
  | otherwise = -2 * n - 1

-- | @Text@ 'T.init' that returns @""@ if given @""@.
textInit :: Text -> Text
textInit "" = ""
textInit a  = Text.init a

textTail :: Text -> Text
textTail "" = ""
textTail x  = Text.tail x
