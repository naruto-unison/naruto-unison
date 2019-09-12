-- Helper functions.
module Core.Util
  ( (!?), (!!)
  , (—), (∈), (∉)
  , Lift
  , intersects
  , duplic
  , shorten
  ) where

import ClassyPrelude hiding ((<|), mapMaybe)
import Control.Monad.Trans.Class (MonadTrans)

infixl 9 !?
-- | 'unsafeIndex'.
(!?) :: ∀ o. IsSequence o => o -> Index o -> Maybe (Element o)
(!?) = index
{-# INLINE (!?) #-}

infixl 9 !!
-- | 'unsafeIndex'.
(!!) :: ∀ o. IsSequence o => o -> Index o -> Element o
(!!) = unsafeIndex
{-# INLINE (!!) #-}

-- | '-' allowing for sections.
infixl 6 —
(—) :: ∀ a. Num a => a -> a -> a
(—) = (-)
{-# INLINE (—) #-}

-- | 'elem'.
infix 4 ∈
(∈) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∈) = elem
{-# INLINE (∈) #-}

-- | 'notElem'.
infix 4 ∉
(∉) :: ∀ o. (MonoFoldable o, Eq (Element o)) => Element o -> o -> Bool
(∉) = notElem
{-# INLINE (∉) #-}

-- | True if any elements are shared by both collections.
intersects :: ∀ a. SetContainer a => a -> a -> Bool
xs `intersects` ys = not . null $ intersection xs ys
{-# INLINE intersects #-}

-- | True if a list contains multiple identical values.
duplic :: ∀ a. Eq a => [a] -> Bool
duplic = go []
  where
    go _ [] = False
    go seen (x:xs)
      | x ∈ seen  = True
      | otherwise = go (x:seen) xs

-- | Removes spaces and special characters.
shorten :: Text -> Text
shorten = omap f . filter (∉ bans)
  where
    bans :: String
    bans  = " -:()®'/?"
    f 'ō' = 'o'
    f 'Ō' = 'O'
    f 'ū' = 'u'
    f 'Ū' = 'U'
    f 'ä' = 'a'
    f a   = a
    {-# INLINE f #-}

-- | A metaconstraint for liftable functions.
-- Useful for default signatures of MTL classes:
-- > default myfunc :: Lift MyMonad m => m ()
-- > myfunc = lift myfunc
type Lift mtl m = (MonadTrans (Car m), mtl (Cdr m), m ~ Car m (Cdr m))
-- Just don't worry about it
type family Car m :: (* -> *) -> * -> * where Car (t n) = t
type family Cdr (m :: * -> *) :: * -> * where Cdr (t n) = n
