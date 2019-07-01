module Model.Duration
  ( Duration(..)
  , throttle
  , incr
  , sync, unsync
  , Turns
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON)

type Turns = Int
newtype Duration = Duration Turns deriving (Num, Eq, Ord, Show, Read, ToJSON)

throttle :: Int -> Duration -> Maybe Duration
throttle 0 dur = Just dur
throttle amount dur@(Duration d)
  | d > 0     = makeDur . max 0 $ d - amount
  | d < 0     = makeDur . min 0 $ d + amount
  | otherwise = Just dur
  where
    makeDur 0 = Nothing
    makeDur x = Just $ Duration x

-- | Increments a number's magnitude unless it is 0.
incr :: ∀ a. (Ord a, Num a) => a -> a
incr x
  | x > 0     = x + 1
  | x < 0     = x - 1
  | otherwise = x

-- | Converts from turns to sub-turns. Output is always positive.
-- Each turn consists of two sub-turns, one for each player.
sync :: Duration -> Int
sync (Duration n)
  | n >= 0    = 2 * n
  | otherwise = -2 * n - 1

-- | Inverse of 'sync'. Input should always be positive.
unsync :: Int -> Duration
unsync x
  | even x    = Duration $ x `quot` 2
  | otherwise = Duration . negate $ 1 + x `quot` 2
