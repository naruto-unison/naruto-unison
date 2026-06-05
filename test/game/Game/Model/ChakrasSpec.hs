{-# LANGUAGE OverloadedLists #-}

module Game.Model.ChakrasSpec (spec) where

import Import hiding (it, shouldBe, shouldNotBe)

import Test.QuickCheck
import Test.Hspec.QuickCheck

import qualified Game.Model.Chakras as Chakras

import Sim (simGame)

spec :: Spec
spec = parallel do
    describe "Chakras.random" do
        prop "generates chakra" \i -> simGame do
            chakras <- Chakras.random i
            return $ chakras === replicate i Blood
