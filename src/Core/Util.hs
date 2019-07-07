-- Helper functions.
module Core.Util
  ( (!!), (—), (∈), (∉)
  , Lift
  , enumerate
  , intersects
  , duplic
  , mapMaybe
  , shorten
  ) where

import ClassyPrelude hiding ((<|), mapMaybe)
import Control.Monad.Trans.Class (MonadTrans)

import Data.List (nub)

{-# INLINE (!!) #-}
infixl 9 !!
-- | 'unsafeIndex'.
(!!) :: ∀ o. IsSequence o => o -> Index o -> Element o
(!!) = unsafeIndex

-- | '-' allowing for sections.
{-# INLINE (—) #-}
infixl 6 —
(—) :: ∀ a. Num a => a -> a -> a
(—) = (-)

-- | 'elem'.
{-# INLINE (∈) #-}
infix 4 ∈
(∈) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∈) = elem

-- | 'notElem'.
{-# INLINE (∉) #-}
infix 4 ∉
(∉) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∉) = notElem

-- | Maps across an enum.
{-# INLINE enumerate #-}
enumerate :: ∀ a b. (Bounded a, Enum a) => (a -> b) -> [b]
enumerate = (<$> [minBound..maxBound])

-- | True if any elements are shared by both collections.
intersects :: ∀ a b.
    (MonoFoldable a, MonoFoldable b, Element a ~ Element b, Eq (Element a))
    => a -> b -> Bool
intersects x = any (∈ x)
{-# INLINE intersects #-}

-- | True if a list contains multiple identical values.
{-# INLINE duplic #-}
duplic :: ∀ a. Eq a => [a] -> Bool
duplic x = nub x /= x

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

-- | A metaconstraint for liftable functions.
-- Useful for default signatures of MTL classes:
-- > default myfunc :: Lift MyMonad m => m ()
-- > myfunc = lift myfunc
type Lift mtl m = (MonadTrans (Car m), mtl (Cdr m), m ~ Car m (Cdr m))
-- Just don't worry about it
type family Car m :: (* -> *) -> * -> * where Car (t n) = t
type family Cdr (m :: * -> *) :: * -> * where Cdr (t n) = n
