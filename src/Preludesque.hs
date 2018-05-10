{-# OPTIONS_HADDOCK hide #-}

module Preludesque (module Import, concat, concatMap, catMaybes, mapMaybe) where

import Core.Unicode       as Import
import Prelude            as Import hiding ((!!), concat, concatMap, head, last, tail, init)
import Data.Foldable      as Import hiding (concat, concatMap)
import Data.List          as Import hiding ((!!), concat, concatMap, group, groupBy, head, last, tail, init, insert)
import Data.List.NonEmpty as Import (NonEmpty(..), (!!), group, groupBy, head, last, tail, init)
import Data.Maybe         as Import hiding (catMaybes, mapMaybe)

concat ∷ (Foldable a, Monoid b) ⇒ a b → b
concat = foldr mappend mempty

concatMap ∷ (Functor a, Foldable a, Monoid b) ⇒ (c → b) → a c → b
concatMap f = concat ∘ (f ↤)

catMaybes ∷ Monad m ⇒ m (Maybe a) → m a
catMaybes xs = do
    Just x ← xs
    return x

mapMaybe ∷ Monad m ⇒ (a → Maybe b) → m a → m b
mapMaybe f xs = [ x | Just x ← f ↤ xs ]
