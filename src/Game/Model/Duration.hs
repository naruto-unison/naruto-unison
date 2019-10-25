module Game.Model.Duration
  ( Duration(..)
  , throttle
  , incr
  , sync, unsync
  , Turns
  ) where

import ClassyPrelude

import Data.Aeson (ToJSON(..))

import Class.Display (Display(..))
import Text.Blaze (ToMarkup(..))

-- | A type synonym that should always be used in "Game.Action" modules to
-- represent time, rather than @Int@ or @Duration@.
-- Wherever it appears, it should immediately be converted to a @Duration@ via
-- view pattern.
type Turns = Int
newtype Duration = Duration Turns deriving (Num, Eq, Ord, Show, Read)

instance Display Duration where
    display (Duration d) = display $ abs d

instance ToJSON Duration where
    toJSON = toJSON . sync
    {-# INLINE toJSON #-}

instance ToMarkup Duration where
      toMarkup (Duration d) = toMarkup $ abs d

-- | Decreases a duration.
-- Returns Nothing if the duration is reduced to 0.
throttle :: Int -> Duration -> Maybe Duration
throttle 0 dur = Just dur
throttle amount dur@(Duration d) = case d `compare` 0 of
    EQ -> Just dur
    GT -> makeDur . max 0 $ d - amount
    LT -> makeDur . min 0 $ d + amount
  where
    makeDur 0 = Nothing
    makeDur x = Just $ Duration x

-- | Increments a number's magnitude unless it is 0.
incr :: ∀ a. (Ord a, Num a) => a -> a
incr x = case x `compare` 0 of
    EQ -> x
    GT -> x + 1
    LT -> x - 1

-- | Converts from turns to sub-turns. Output is always positive.
-- Each turn consists of two sub-turns, one for each player.
sync :: Duration -> Int
sync (Duration d)
  | d >= 0    = 2 * d
  | otherwise = -2 * d - 1

-- | Inverse of 'sync'. Input should always be positive.
unsync :: Int -> Duration
unsync d
  | even d    = Duration $ d `quot` 2
  | otherwise = Duration . negate $ 1 + d `quot` 2
