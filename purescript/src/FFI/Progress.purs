module FFI.Progress (progress) where

import StandardLibrary

import Data.Time.Duration (Milliseconds)

foreign import progress :: Milliseconds -> Int -> Int -> Effect Unit
