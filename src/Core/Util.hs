-- Helper functions.
module Core.Util
  ( (—), (∈), (∉)
  , intersects
  , duplic
  , enumerate
  , mapMaybe
  , shorten
  ) where

import ClassyPrelude hiding ((<|), mapMaybe)

import Data.List (nub)

-- | '-' allowing for sections.
{-# INLINE (—) #-}
(—) :: ∀ a. Num a => a -> a -> a
(—) = (-)

{-# INLINE (∈) #-}
-- | 'elem'.
(∈) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∈) = elem

{-# INLINE (∉) #-}
-- | 'notElem'.
(∉) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∉) = notElem

{-# INLINE intersects #-}
-- | True if any elements are shared by both collections.
intersects :: ∀ a b.
    (MonoFoldable a, MonoFoldable b, Element a ~ Element b, Eq (Element a))
    => a -> b -> Bool
intersects x = any (∈ x)

{-# INLINE duplic #-}
-- | True if a list contains multiple identical values.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic x = nub x /= x

{-# INLINE enumerate #-}
-- | Lists all members of an 'Enum' from 'minBound' to 'maxBound'.
enumerate :: ∀ a. (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

{-# INLINE mapMaybe #-}
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
