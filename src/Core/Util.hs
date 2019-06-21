-- Helper functions.
module Core.Util
  ( (—), (∈), (∉)
  , intersects
  , duplic
  , enumerate
  , equaling
  , mapMaybe
  , shorten
  , textInit, textTail
  ) where

import ClassyPrelude hiding ((<|), mapMaybe)

import qualified Data.List as List
import qualified Data.Text as Text

-- | '-' allowing for sections.
(—) :: ∀ a. Num a => a -> a -> a
(—) = (-)

-- | 'elem'.
(∈) :: ∀ o. MonoFoldable o => Eq (Element o) => Element o -> o -> Bool
(∈) = elem

-- | 'notElem'.
(∉) :: ∀ o. MonoFoldable o => Eq (Element o) => Element o -> o -> Bool
(∉) = notElem

-- | True if any elements are shared by both collections.
intersects :: ∀ a b.
    (MonoFoldable a, MonoFoldable b, Element a ~ Element b, Eq (Element a))
    => a -> b -> Bool
intersects x = any (∈ x)

-- | True if a list contains multiple identical values.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic x = List.nub x /= x

-- | Lists all members of an 'Enum' from 'minBound' to 'maxBound'.
enumerate :: ∀ a. (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

-- | Equality with a projecting function. Analogous to 'comparing'.
equaling :: ∀ a b. Eq b => (a -> b) -> a -> a -> Bool
equaling f x y = f x == f y

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

-- | 'Text.init' that returns @""@ if given @""@ instead of crashing.
textInit :: Text -> Text
textInit "" = ""
textInit a  = Text.init a

-- | 'Text.tail' that returns @""@ if given @""@ instead of crashing.
textTail :: Text -> Text
textTail "" = ""
textTail x  = Text.tail x
