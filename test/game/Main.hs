module Main where

import Prelude

import Test.Hspec.Runner
import Test.Hspec.Api.Formatters.V3

import Spec (spec)

main :: IO ()
main = hspecWith (useFormatter ("failed-examples", failed_examples) defaultConfig) spec
