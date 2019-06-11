module FFI.Progress (progress) where

import Prelude
import Effect (Effect)

import Data.Time.Duration (Milliseconds)

foreign import progress :: Milliseconds -> Int -> Int -> Effect Unit
