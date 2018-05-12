{-# OPTIONS_HADDOCK hide #-}

-- | Replaces Prelude's partial functions with 'Data.List.NonEmpty'.
module Preludesque (module Import, concat, concatMap, catMaybes, filter, mapMaybe, drop', take') where

import Core.Unicode       as Import
import Prelude            as Import hiding ((!!), concat, concatMap, filter, head, last, tail, init)
import Data.Foldable      as Import hiding (concat, concatMap)
import Data.List          as Import hiding ((!!), concat, concatMap, filter, group, groupBy, head, last, tail, init, insert)
import Data.List.NonEmpty as Import (NonEmpty(..), (!!), group, groupBy, head, last, tail, init)
import Data.Maybe         as Import hiding (catMaybes, mapMaybe)

import Control.Monad
import qualified Data.List.NonEmpty as L

concat ∷ (Foldable a, Monoid b) ⇒ a b → b
concat = foldr mappend mempty

concatMap ∷ (Functor a, Foldable a, Monoid b) ⇒ (c → b) → a c → b
concatMap f = concat ∘ (f ↤)

catMaybes ∷ Monad m ⇒ m (Maybe a) → m a
catMaybes xs = do
    Just x ← xs
    return x

filter ∷  MonadPlus m ⇒ (a → Bool) → m a → m a
filter = filter 

mapMaybe ∷ Monad m ⇒ (a → Maybe b) → m a → m b
mapMaybe f xs = catMaybes $ f ↤ xs

drop' ∷ Int → NonEmpty a → [a]
drop' = L.drop

take' ∷ Int → NonEmpty a → [a]
take' = L.take
