{-# LANGUAGE OverloadedLists #-}

module Game.Model.ChakrasSpec (spec) where

import Import hiding (it, shouldBe, shouldNotBe, groupBy, groupAllOn, minimum)

import Data.Sequences (groupBy, groupAllOn)
import Test.QuickCheck
import Test.Hspec.QuickCheck

import qualified Game.Model.Chakras as Chakras
import OrphanInstances ()

import Sim (simGame)

type IsChakraSequence o = (IsSequence o, Chakra ~ Element o, Int ~ Index o)

spec :: Spec
spec = parallel do
    describe "Chakras" do
        prop "random" \i -> simGame do
            chakras <- Chakras.random i
            return $ chakras === replicate i Blood
        prop "ofoldr" $ oprop
            $ ofoldr (:) []
        prop "ofoldl'" $ oprop
            $ ofoldl' (flip (:)) []
        prop "otoList" $ opropList
            $ id
        prop "oall" $ oprop
            $ oall (>= Nin)
        prop "oany" $ oprop
            $ oany (>= Nin)
        prop "onull" $ oprop
            $ onull
        prop "olength" $ oprop
            $ olength
        prop "ofoldr1Ex" $ opropEx
            $ ofoldr1Ex const
        prop "ofoldl1Ex'" $ opropEx
            $ ofoldl1Ex' const
        prop "headEx" $ opropEx
            $ headEx
        prop "lastEx" $ opropEx
            $ lastEx
        prop "maximumByEx" $ opropEx
            $ maximumByEx compare
        prop "minimumByEx" $ opropEx
            $ minimumByEx compare
        prop "oelem" \x -> oprop
            $ oelem x
        prop "onotElem" \x -> oprop
            $ onotElem x
        prop "otraverse" $ opropList
            $ runIdentity . otraverse Identity
        prop "opoint" \x -> opropApp
            $ opoint x
        prop "intersperse" \x -> opropList
            $ sort . intersperse x
        prop "find" \x -> oprop
            $ find (== x)
        prop "cons" \x -> opropList
            $ cons x
        prop "snoc" \x -> opropList
            $ flip snoc x
        prop "break" \x -> opropTuple
            $ break (== x)
        prop "span" \x -> opropTuple
            $ span (== x)
        prop "dropWhile" \x -> opropList
            $ dropWhile (== x)
        prop "takeWhile" \x -> opropList
            $ takeWhile (== x)
        prop "splitAt" \x -> opropTuple
            $ splitAt x
        prop "partition" \x -> opropTuple
            $ partition (== x)
        prop "uncons" $ oprop
            $ (second toList <$>) . uncons
        prop "unsnoc" $ oprop
            $ (first toList <$>) . unsnoc
        prop "filter" \x -> opropList
            $ filter (>= x)
        prop "filterM" \x -> opropList
            $ runIdentity . filterM \y -> Identity $ y >= x
        prop "replicate" \i x -> opropApp
            $ replicate i x
        prop "replicateM" \i x -> opropApp
            $ runIdentity . replicateM i $ Identity x
        prop "groupBy" $ opropLists
            $ groupBy (==)
        prop "groupAllOn" $ opropLists
            $ groupAllOn id
        prop "index" \i -> oprop
            $ flip index i
        prop "splitWhen" $ opropLists
            $ splitWhen \x -> x == Gen || x == Tai

toChakras :: [Chakra] -> Chakras
toChakras = fromList

oprop :: ∀ a. (Eq a, Show a)
         => (∀ o. IsChakraSequence o => o -> a)
         -> (SortedList Chakra -> Property)
oprop f (Sorted xs) = f (toChakras xs) === f xs

opropEx :: ∀ a. (Eq a, Show a)
        => (∀ o. IsChakraSequence o => o -> a)
        -> (NonEmptyList Chakra -> Property)
opropEx f (NonEmpty xs) = f (toChakras xs) === f (sort xs)

opropList :: (∀ o. IsChakraSequence o => o -> o)
          -> (SortedList Chakra -> Property)
opropList f (Sorted xs) = toList (f $ toChakras xs) === sort (f xs)

opropLists :: (∀ o. IsChakraSequence o => o -> [o])
           -> (SortedList Chakra -> Property)
opropLists f (Sorted xs) = (toList <$> f (toChakras xs)) === f xs

opropTuple :: (∀ o. IsChakraSequence o => o -> (o, o))
           -> (SortedList Chakra -> Property)
opropTuple f (Sorted xs) = both toList (f $ toChakras xs) === f xs
  where
    both g (a, b) = (g a, g b)

opropApp :: (∀ o. IsChakraSequence o => o)
         -> Property
opropApp f = toList (f @Chakras) === f @[Chakra]
