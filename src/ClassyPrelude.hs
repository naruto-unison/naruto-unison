{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The [classy-prelude](https://hackage.haskell.org/package/classy-prelude)
-- package, but using "Data.List.NonEmpty" instead of "Data.NonNull".
module ClassyPrelude
  ( module CP
  , module Prelude
  , module Data.Kind
  , module NonNull
  , maybeToList
  , pattern Empty, pattern (:<), pattern (:>)
  ) where

import "classy-prelude" ClassyPrelude as CP hiding
    ( group, groupBy
    , maybeToList
    , NonNull(..)
    , fromNullable
    , ncons, nuncons
    , head, tail, last, init
    , ofoldMap1, ofold1, ofoldr1, ofoldl1'
    , maximum, minimum, maximumBy, minimumBy)
import Prelude (type (~), MonadFail(..), errorWithoutStackTrace, ShowS, Show(..), shows, showChar, showString, showParen, ReadS, Read(..), reads, readParen, read, lex)
import Data.Kind (Constraint, Type)
import NonNull

maybeToList :: ∀ o. (Monoid o, MonoPointed o) => Maybe (Element o) -> o
maybeToList (Just el) = singleton el
maybeToList Nothing   = mempty
{-# SPECIALIZE maybeToList :: Maybe a -> [a] #-}

pattern Empty :: ∀ o. IsSequence o => o
pattern Empty <- (null -> True) where
    Empty = mempty

-- | 'cons' and 'uncons'
pattern (:<) :: ∀ o. IsSequence o => Element o -> o -> o
pattern x :< xs <- (uncons -> Just (x, xs)) where
    (:<) = cons
infixr 5 :<
{-# COMPLETE Empty, (:<) #-}

-- | 'snoc' and 'unsnoc'
pattern (:>) :: ∀ o. IsSequence o => o -> Element o -> o
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
    (:>) = snoc
infixl 5 :>
{-# COMPLETE Empty, (:>) #-}
